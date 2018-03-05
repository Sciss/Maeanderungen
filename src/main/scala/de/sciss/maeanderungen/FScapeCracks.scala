/*
 *  FScapeCracks.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.swing.Swing

object FScapeCracks {
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    // coordinates in fPolesIn are already up-sampled to original image dimension
    val fPolesIn          = file("/data/projects/Maeanderungen/cracks/cracks2_poles.aif")
    val fImgIn            = file("/data/projects/Imperfect/cracks/two_gray/cracks2_19.jpg")
//    val fImgIn            = file("/data/projects/Imperfect/cracks/two_bw/cracks2_19bw.png")
    val fAudioOut         = file("/data/temp/test.aif")
    val imgWidth          = 5184
    val imgHeight         = 3020
    val audioBreadthStep  = 2

    val polesSpec         = AudioFile.readSpec(fPolesIn)
    println(s"num-poles = ${polesSpec.numFrames}")

    val g = Graph {
      import graph._
      val poles     = AudioFileIn(fPolesIn, numChannels = 4)
      val numCh     = 2 // 1
      val imgIn     = ImageFileIn(fImgIn  , numChannels = numCh)
      val x1        = poles \ 0
      val y1        = poles \ 1
      val x2        = poles \ 2
      val y2        = poles \ 3
      val dx        = x2 - x1
      val dy        = y2 - y1
      val len       = (dx.squared + dy.squared).sqrt
      val pixelStep = 1.0 / audioBreadthStep
      val numSteps0 = (len / pixelStep).floor
      val numSteps1 = numSteps0 + (1: GE) - (numSteps0 % 2) // odd
      val numSteps  = numSteps1 + (1: GE) // even
//      RepeatWindow(Frames(numSteps)).poll(2, "num-steps")

      val BUF = 44100
      val numStepsM = BufferMemory(numSteps, BUF)

      val x         = DEnvGen(levels = x1 zip x2, lengths = numSteps1 zip DC(1))
      val y         = DEnvGen(levels = y1 zip y2, lengths = numSteps1 zip DC(1))
//      Plot1D(x, size = 4000, "x")
//      Plot1D(y, size = 4000, "y")

      val STATIC_STEP = true // false
      val DC_EARLY    = true
      val USE_IFFT    = false

      def dcBlock(in: GE): GE =
        Biquad(in, b0 = 1, b1 = -1, a1 = -0.99) // dc-block: y(n) = x(n) - x(n-1) + 0.99 * y(n-1)

//      val x         = RepeatWindow(x1.elastic(2), num = len) // XXX TODO
//      val y         = RepeatWindow(y1.elastic(2), num = len) // XXX TODO
      val scan0     = ScanImage(imgIn, width = imgWidth, height = imgHeight, x = x, y = y, zeroCrossings = 0)
      val scan      = -scan0 + (1.0: GE)
      val step0     = numStepsM.sqrt
      val step      = if (STATIC_STEP)
        WhiteNoise(512) + (512: GE)
      else
        (step0 + WhiteNoise(4)).max(1)

      val scanHPF   = if (DC_EARLY) dcBlock(scan) else scan
      val fftSize   = 1024 // BufferDisk(numStepsM)
//      RepeatWindow(fftSize).poll(2, "fft-size")
//      val fftSize   = 32768
      val scanSpec0 = if (USE_IFFT) {
//        Real1IFFT(scanHPF, size = fftSize, mode = 0)
        DCT_II(BufferDisk(scanHPF), size = fftSize, numCoeffs = fftSize)
      } else scanHPF
//      val framesOut = Frames(scanSpec \ 0)
//      ((framesOut - 1) / 44100).poll(44100, "spec [s]")
      val scanSpec  = BufferDisk(scanSpec0)
      val stepM     = BufferDisk(step)
      val lap0      = OverlapAdd(scanSpec, size = numStepsM, step = stepM)
      val lap       = lap0.take(44100 * 30)
      val hpf       = dcBlock(lap)
      val framesOut = Frames(hpf \ 0)
      ((framesOut - 1) / 44100).poll(44100, "spec [s]")
      val max       = RunningMax(hpf).last
      val disk      = BufferDisk(hpf)
      val sigOut    = disk / max
      /* val framesOut = */ AudioFileOut(fAudioOut, AudioFileSpec(numChannels = numCh, sampleRate = 44100), in = sigOut)
//      ((framesOut - 1) / 44100).poll(44100, "out [s]")
//      x.poll(1000, "x")
//      y.poll(1000, "y")
//      lap.poll(1000, "scan")
    }

    val config = stream.Control.Config()
    config.useAsync = false
    val ctrl = stream.Control(config)

    ctrl.run(g)

    Swing.onEDT {
      SimpleGUI(ctrl)
    }

    println("Running.")
  }
}
