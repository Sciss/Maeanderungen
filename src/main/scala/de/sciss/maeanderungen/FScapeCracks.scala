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
    val fAudioOut         = file("/data/temp/test.aif")
    val imgWidth          = 5184
    val imgHeight         = 3020
    val audioBreadthStep  = 2

    val polesSpec         = AudioFile.readSpec(fPolesIn)
    println(s"num-poles = ${polesSpec.numFrames}")

    val g = Graph {
      import graph._
      val poles     = AudioFileIn(fPolesIn, numChannels = 4)
      val imgIn     = ImageFileIn(fImgIn  , numChannels = 1)
      val x1        = poles \ 0
      val y1        = poles \ 1
      val x2        = poles \ 2
      val y2        = poles \ 3
      val dx        = x2 - x1
      val dy        = y2 - y1
      val len       = (dx.squared + dy.squared).sqrt
      val pixelStep = 1.0 / audioBreadthStep
      val numSteps  = (len / pixelStep).floor + (1: GE)
//      RepeatWindow(Frames(numSteps)).poll(2, "num-steps")

      val BUF = 44100
      val numStepsM = BufferMemory(numSteps, BUF)
      val x         = RepeatWindow(x1.elastic(2), num = len) // XXX TODO
      val y         = RepeatWindow(y1.elastic(2), num = len) // XXX TODO
      val scan0     = ScanImage(imgIn, width = imgWidth, height = imgHeight, x = x, y = y, zeroCrossings = 0)
      val scan      = -scan0 + (1.0: GE)
      val step      = numStepsM.sqrt  // XXX TODO --- let's see if this makes sense
      val lap       = OverlapAdd(scan, size = numStepsM, step = step)
      val hpf       = OnePole(lap, -0.995)    // XXX TODO --- we need a TwoPole and LeakDC
      val max       = RunningMax(hpf).last
      val disk      = BufferDisk(hpf)
      val sigOut    = disk / max
      val framesOut = AudioFileOut(fAudioOut, AudioFileSpec(numChannels = 1, sampleRate = 44100), in = sigOut)
      ((framesOut - 1) / 44100).poll(44100, "out [s]")
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
