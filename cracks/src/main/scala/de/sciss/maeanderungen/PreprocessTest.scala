package de.sciss.maeanderungen

import de.sciss.file.file
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.{Graph, stream}

import scala.swing.Swing

object PreprocessTest {
  def any2stringadd: Nothing = sys.error("WTF")

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    val fIn     = file("/data/temp/foo2.jpg")
    val fOut    = file("/data/temp/foo2bw2.jpg")
    val thresh  = 0.85    // 0 to 1, larger = more dark

    val g = Graph {
      import de.sciss.fscape.graph._
      def mkLum() = {
        val imgIn = ImageFileIn(fIn, numChannels = 3)
        val red   = imgIn out 0
        val green = imgIn out 1
        val blue  = imgIn out 2
        red*0.2126 + green*0.7152 + blue*0.0722
      }

      val w         = 3280
      val h         = 2464
      val frameSize = w * h
      val lum1      = mkLum()
      val histo     = SortWindow(lum1, lum1, frameSize)
//      Length(histo).poll(0, "histo-len")
      val threshSz  = thresh * frameSize
      val threshVal = histo.drop(threshSz).head
      threshVal.poll(0, "thresh-val")
      val lum2      = mkLum()
      val sig       = lum2 > threshVal

      val specOut = ImageFile.Spec(ImageFile.Type.JPG, width = w, height = h, numChannels = 1)
      ImageFileOut(file = fOut, spec = specOut, in = sig)
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
