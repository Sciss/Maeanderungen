/*
 *  Util.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import java.awt.{Color, Font}

import de.sciss.chart.{Chart, XYChart}
import de.sciss.desktop.Desktop
import de.sciss.file.File
import de.sciss.kollflitz.Vec
import de.sciss.numbers
import de.sciss.optional.Optional
import de.sciss.span.Span
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.TimeRef
import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer, XYStepAreaRenderer}

object Util {
  def readAudioFileData1D(in: File): Vec[Double] = {
    val af = AudioFile.openRead(in)
    try {
      require (af.numFrames < 0x7FFFFFFF)
      require (af.numChannels == 1)
      val n = af.numFrames.toInt
      val b = af.buffer(n)
      af.read(b)
      b(0).iterator.map(_.toDouble).toVector

    } finally {
      af.close()
    }
  }

  def mkHistogram[A](data: Vec[A], numBins: Int, min: Optional[A] = None, max: Optional[A] = None)
                    (implicit num: Fractional[A]): Vec[Int] = {
    import num.{fromInt, mkNumericOps, mkOrderingOps}
    val min1      = min getOrElse data.min
    val max1      = max getOrElse data.max
    val binWidth  = (max1 - min1) / fromInt(numBins)
    val accum     = Vector.tabulate(numBins) { i =>
      val low : A = fromInt(i    ) * binWidth + min1
      val high: A = fromInt(i + 1) * binWidth + min1
      val isFirst = i == 0
      val isLast  = i == numBins - 1
      data.count(x => (isFirst || x >= low) && (isLast || x < high)) // IntelliJ highlight bug
    }
    accum
  }

  def mkHistogramChart(histo: Vec[Int], xMin: Double, xMax: Double, title: String): XYChart = {
    import numbers.Implicits._

    import de.sciss.chart.module.Charting._
    val data: Vec[(Int, Int)] = histo.iterator.zipWithIndex.map { case (num, i) =>
      val bla = (i + 0.5).linLin(0, histo.length, xMin, xMax)
      bla.toInt -> num
    } .toVector
    val dataCol = data.toXYSeriesCollection(title)
    val chart   = XYLineChart(dataCol)  // IntelliJ highlight bug
    chart.title = title
    //    chart.legend = false
    mkNiceChart(chart)
    val plot    = chart.plot
    val renderer = new XYStepAreaRenderer()
    plot.setRenderer(renderer)
    renderer.setSeriesPaint(0, Color.darkGray)
    chart
  }

  private val defaultFontFace =
    if      (Desktop.isLinux  ) "Liberation Sans"
    else if (Desktop.isWindows) "Arial"
    else                        "Helvetica"

  def mkNiceChart(chart: Chart): Unit = {
    val plot = chart.plot

    val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
      case p: XYPlot       =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
      case p: CategoryPlot =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
    }

    val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
    val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
    xAxis.setLabelFont(fnt1)
    xAxis.setTickLabelFont(fnt2)
    yAxis.setLabelFont(fnt1)
    yAxis.setTickLabelFont(fnt2)
  }

  private lazy val tf = new TimeFormat(Span.All, clip = false, sampleRate = TimeRef.SampleRate)

  def framesToTime(n: Long): String = tf.format(n)

  def spanToTime(sp: Span): String = {
    val start = tf.format(sp.start)
    val stop  = tf.format(sp.stop )
    s"[$start - $stop]"
  }
}
