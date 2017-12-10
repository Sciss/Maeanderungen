/*
 *  RhizomSim.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.stream.Control
import de.sciss.numbers
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

object RhizomeSim {
  private val GRAPH_COOKIE = 0x5268697A // "Rhiz"

  final case class Vertex(fIn: File) {
    def quote: String = fIn.name
  }

  val projectDir: File  = file("/data/projects/Maeanderungen")
  val rhizomeDir: File  = projectDir / "materials" / "audio_work" / "rhizom"

  def main(args: Array[String]): Unit = {
    val filesIn     = rhizomeDir.children(f => f.name.endsWith("-1.aif")).sorted(File.NameOrdering)
    val vertices    = filesIn.map(Vertex).toList
    val fOut        = rhizomeDir / "rhizome-edges.bin"
    val edges       = if (fOut.exists()) {
      println(s"File $fOut already exists. Not overwriting.")
      readGraph(fOut)
    } else {
      val fut = calcSim(vertices, fOut = fOut)
      Await.result(fut, Duration.Inf)
    }
  }

  case class SimEdge(sourceVertex: Vertex, targetVertex: Vertex, weight: Double)

  def readGraph(fIn: File): List[SimEdge] = {
    val sIn = new FileInputStream(fIn)
    try {
      val dIn = new DataInputStream(sIn)
      import dIn._
      val cookie = readInt()
      require (cookie == GRAPH_COOKIE)
      val b = List.newBuilder[SimEdge]
      while (available() > 0) {
        val v1Name  = readUTF()
        val v2Name  = readUTF()
        val sim     = readFloat()
        val e       = SimEdge(Vertex(rhizomeDir / v1Name), Vertex(rhizomeDir / v2Name), sim)
        b += e
      }
      val edgesI = b.result()
      normalizeEdges(edgesI)

    } finally {
      sIn.close()
    }
  }

  def normalizeEdges(in: List[SimEdge]): List[SimEdge] = {
    val minSim = in.iterator.map(_.weight).min
    val maxSim = in.iterator.map(_.weight).max
    println(f"minSim = $minSim%g, maxSim = $maxSim%g")
    import numbers.Implicits._
    val edges = in.map(e => e.copy(weight = e.weight.linlin(minSim, maxSim, 1.0, 0.0)))
    edges
  }

  def calcSim(selection: List[Vertex], fOut: File): Future[List[SimEdge]] = {
    val numComb = selection.combinations(2).size
    println(s"Number of combinations: $numComb")

    import scala.concurrent.ExecutionContext.Implicits.global

    val t: Future[List[SimEdge]] = Future {
      val mapAf = mutable.Map.empty[Int, AudioFile]
      val dos = new DataOutputStream(new FileOutputStream(fOut))
      dos.writeInt(GRAPH_COOKIE)

      def run(): List[SimEdge] = try {
        def loop(rem: List[Vertex], res: List[SimEdge], mapTemp: Map[Vertex, File]): List[SimEdge] =
          rem match {
            case v1 :: tail if tail.nonEmpty =>
              var map = mapTemp

              def getTemp(v: Vertex): File =
                map.getOrElse(v, {
                  val f2  = File.createTemp()
                  val g   = mkAnalysisGraph(inFile = v.fIn, outFile = f2, name = s"ana $v")
                  val c   = Control()
                  c.run(g)
                  Await.result(c.status, Duration.Inf)
                  map += v -> f2
                  f2
                })

              val fileA = getTemp(v1)
              val edge = tail.flatMap { v2 =>
                val fileB = getTemp(v2)
                val (g, fut) = mkCorrelationGraph(fileA = fileA, fileB = fileB)
                val c = Control()
                c.run(g)
                val sim = Await.result(fut, Duration.Inf)
                dos.writeUTF(v1.fIn.name)
                dos.writeUTF(v2.fIn.name)
                dos.writeFloat(sim.toFloat)
                println(f"${v1.quote} -- ${v2.quote} : $sim%g")
                val e = SimEdge(v1, v2, sim)
                Some(e)
              }
              loop(tail, edge ::: res, map)

            case _ => res
          }

        val res = loop(selection, Nil, Map.empty)
        println("Done.")
        res

      } finally {
        mapAf.valuesIterator.foreach(_.cleanUp())
        dos.close()
      }

      run()
    }

    t.map(normalizeEdges)
  }

  def mkAnalysisGraph(inFile: File, outFile: File, name: String, fftSize: Int = 1024, winStep: Int = 256,
                      melBands: Int = 64): Graph = {
    val g = Graph {
      import de.sciss.fscape._
      import graph._

      val spec   = AudioFile.readSpec(inFile)

      def in0  = AudioFileIn(inFile, numChannels = spec.numChannels)
      def mkIn = if (spec.numChannels == 1) in0 else (in0\0) + (in0\1)

      def mkEnergy(in: GE): GE = {
        val mx  = RunningSum(in.squared).last
        mx
      }

      def mkMFCC(in: GE): GE = {
        val lap  = Sliding(in, size = fftSize, step = winStep) * GenWindow(fftSize, GenWindow.Hann)
        val fft  = Real1FFT(lap, size = fftSize, mode = 1)
        val mag  = fft.complex.mag // .max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 86, maxFreq = 16512)
        mel // DCT_II(mel.log.max(-80), size = melBands, numCoeffs = numMFCC, zero = 0)
        //        val mag  = fft.complex.mag.log.max(-80)
        //        val mel  = MelFilter(mag, winStep, bands = melBands, minFreq = 50, maxFreq = 4000)
        //        DCT_II(mel, melBands, numMFCC, zero = 0)
      }

      val mfcc  = mkMFCC  (mkIn)
      val e     = mkEnergy(mkIn)
      val sig   = e ++ mfcc
      AudioFileOut(outFile, AudioFileSpec(numChannels = 1, sampleRate = 44100), in = sig)
    }
    g
  }

  def mkCorrelationGraph(fileA: File, fileB: File, melBands: Int = 64): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.fscape._
      import de.sciss.numbers.Implicits._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)

      val chunkA  = AudioFileIn(fileA, numChannels = 1)
      val chunkB  = AudioFileIn(fileB, numChannels = 1)
      val energyA = chunkA.head
      val energyB = chunkB.head
      val mfccA   = chunkA.tail
      val mfccB   = chunkB.tail

      val numMFCC   = melBands // 64 // 128 // 42 // 32

      val numWinA   = ((specA.numFrames - 1) / numMFCC).toInt
      val numWinB   = ((specB.numFrames - 1) / numMFCC).toInt

      val lenA2     = numWinA * numMFCC
      val lenB2     = numWinB * numMFCC

      val convLen: Int = numWinA + numWinB - 1

      val fftSize   = (convLen * numMFCC).nextPowerOfTwo
      val chunkAP   = mfccA ++ DC(0).take(fftSize - lenA2)
      val chunkBP   = mfccB ++ DC(0).take(fftSize - lenB2)

      val fftMode   = 1
      val fftA      = Real1FFT(in = chunkAP, size = fftSize, mode = fftMode)
      val fftB0     = Real1FFT(in = chunkBP, size = fftSize, mode = fftMode)
      val fftB      = fftB0.complex.conj

      val prod      = fftA.complex * fftB
      val corr0     = Real1IFFT(in = prod, size = fftSize, mode = fftMode)

      val energyT = (energyA + energyB)/2

      val corr      = corr0

      val max       = RunningMax(corr).last / energyT * (fftSize/2)

      Fulfill(max, p)
    }
    (g, p.future)
  }
}