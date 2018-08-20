package de.sciss.maeanderungen

import java.awt.Color
import java.awt.image.BufferedImage

import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.{Graph, graph, stream}
import javax.imageio.ImageIO

import scala.swing.Swing

object WalkingPiTest {

  val wIn       : Int     = 3280
  val hIn       : Int     = 2464

  case class Config(startFrame: Int, endFrame: Int, numRows: Int, numCols: Int, site: Int, qualify: String = "",
                    decim: Int = 8) {
    val numFrames : Int     = endFrame - startFrame + 1

    val hOut      : Int     = hIn / decim
    val wOut      : Int     = wIn / decim
    val sx        : Double  = 1.0 / decim
    val sy        : Double  = 1.0 / decim
    val wOut2     : Int     = wOut * numCols
    val hOut2     : Int     = hOut * numRows

    assert(numRows * numCols == numFrames)
  }

  val cfg1 = Config(startFrame =   3, endFrame = 138, numRows = 17, numCols = 8, site = 13)
  val cfg2 = Config(startFrame =   2, endFrame = 137, numRows = 17, numCols = 8, site = 14)
  val cfg3 = Config(startFrame =   2, endFrame = 137, numRows = 17, numCols = 8, site = 14)
  val cfg4 = Config(startFrame = 141, endFrame = 276, numRows = 17, numCols = 8, site = 14, qualify = "f")

  def tempIn  (site: Int): File = file(s"/data/projects/Maeanderungen/exposure/site-$site/frame-%d.jpg")
  def tempOut (site: Int, qualify: String): File = file(s"/data/temp/exposure-$site$qualify-test/frame-%d.png")

  def main(args: Array[String]): Unit =
    run2(cfg4)

  def run2(config: Config): Unit = {
    import config._
    val img = new BufferedImage(wOut2, hOut2, BufferedImage.TYPE_INT_ARGB)
    val g = img.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, wOut2, hOut2)
    val tempOutF = tempOut(site = site, qualify = qualify)
    tempOutF.parent.mkdirs()
    for (i <- 0 until numFrames) {
      val fIn   = tempOutF.replaceName(tempOutF.name.format(i + 1))
      val imgI  = ImageIO.read(fIn)
      require(imgI != null, fIn.toString)
      val tx    = (i % numCols) * wOut
      val ty    = (i / numCols) * hOut
      g.drawImage(imgI, tx, ty, Color.black, null)
      imgI.flush()
    }
    g.dispose()
    val fOut = file(s"/data/temp/exposure-$site$qualify-test.png")
    ImageIO.write(img, "png", fOut)
    img.flush()
  }

  def run1(config: Config): Unit = {
    import config._
    val tempInF     = tempIn (site = site)
    val tempOutF    = tempOut(site = site, qualify = qualify)
    tempOutF.parent.mkdirs()
    val g = Graph {
      import graph._
      val indicesIn   = ArithmSeq(start = startFrame, length = numFrames)
      val imgIn       = ImageFileSeqIn(tempInF, numChannels = 3, indices = indicesIn)
      val small       = AffineTransform2D.scale(imgIn,
        widthIn   = wIn , heightIn  = hIn,
        widthOut  = wOut, heightOut = hOut,
        sx = sx, sy = sy)
//      val rot         = AffineTransform2D.rotate(small,
//        widthIn   = wOut, heightIn  = hOut,
//        widthOut  = wOut, heightOut = hOut,
//        theta = math.Pi, ax = 0.5 * wOut, ay = 0.5 * hOut,
//        zeroCrossings = 0, wrap = 0
//      )
      val rot = ReverseWindow(ReverseWindow(small, size = wOut * hOut, clump = wOut),
        size = wOut, clump = 1
      )

      val indicesOut  = ArithmSeq(start = 1, length = numFrames)
      val specOut     = ImageFile.Spec(width = wOut, height = hOut, numChannels = 3)
      ImageFileSeqOut(template = tempOutF, spec = specOut, indices = indicesOut, in = rot)

//      def i           = ArithmSeq(0, length = numFrames)
//      val tx          = i % numCols
//      val ty          = (i / numRows).floor
//      val winSize     = wOut2 * hOut2
//      val txv         = RepeatWindow(tx * wOut, size = 1, num = winSize)
//      val tyv         = RepeatWindow(ty * hOut, size = 1, num = winSize)
//
//      val placed      = AffineTransform2D.translate(small, widthIn = wOut, heightIn = hOut,
//        widthOut = wOut2, heightOut = hOut2, tx = txv, ty = tyv, zeroCrossings = 0)
//      val placed1     = ResizeWindow(placed, size = winSize, stop = 1)
//      val lap         = OverlapAdd(placed1, winSize + 1, step = 1)
//
//      val fOut        = file("/data/temp/exposure-test.png")
//      val specOut2    = ImageFile.Spec(width = wOut2, height = hOut2, numChannels = 3)
//      ImageFileOut(fOut, specOut2, in = lap)
    }

    val cfg       = stream.Control.Config()
    cfg.useAsync  = false
    cfg.blockSize = wIn/decim * hIn/decim
    val ctrl      = stream.Control(cfg)

    ctrl.run(g)

    Swing.onEDT {
      SimpleGUI(ctrl)
    }

    println("Running.")
  }
}
