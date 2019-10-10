/*
 *  TimeFormat.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import java.text.ParseException

import de.sciss.audiowidgets.{AxisFormat, ParamFormat, UnitView}
import de.sciss.icons.raphael
import de.sciss.span.SpanLike
import javax.swing.JFormattedTextField.AbstractFormatter
import javax.swing.text.MaskFormatter

import scala.util.Try

final class TimeFormat(var span: SpanLike, clip: Boolean, sampleRate: Double) extends ParamFormat[Long] {
  outer =>

  override def toString = s"TimeField.TimeFormat($span, sampleRate = $sampleRate)@${hashCode.toHexString}"

  private[this] val axis = AxisFormat.Time(hours = true, millis = true)

  private def millisToFrames(m: Double): Long = (m * sampleRate / 1000 + 0.5).toLong

  val unit = UnitView("HH:MM:SS.mmm", raphael.Icon(20, raphael.DimPaint)(raphael.Shapes.WallClock))

  val formatter: AbstractFormatter = new MaskFormatter("*#:##:##.###") {
    override def toString = s"$outer.formatter"

    // setAllowsInvalid(true)
    setPlaceholderCharacter('_')
    // the next line is crucial because retarded MaskFormatter calls into super.stringToValue
    // from other methods, and DefaultFormatter finds a Long with a static constructor method
    // that takes a String, and blindly tries to feed that crap into it. Setting an explicit
    // value-class that does _not_ have a string constructor will bypass that bull crap.
    setValueClass(classOf[AnyRef])

    override def stringToValue(value: String): AnyRef = {
      val v0 = super.stringToValue(value)
      val s0 = v0.toString
      val res = tryParse(s0)
      // println(s"res = $res")
      res.asInstanceOf[AnyRef]
    }

    override def valueToString(value: Any): String = {
      val s0 = format(value.asInstanceOf[Long])
      super.valueToString(s0)
    }
  }

  def adjust(in: Long, inc: Int): Long = {
    val incM  = millisToFrames(inc)
    val out   = in + incM
    if (clip) span.clip(out) else out
  }

  private def tryParse(s: String): Long = {
    // HH:MM:SS.mmm
    val arr = s.replace(' ', '0').split(':')
    if (arr.length != 3) throw new ParseException(s, 0)
    try {
      val hours   = arr(0).toLong
      val minutes = arr(1).toLong
      val secs    = arr(2).toDouble
      val millis  = (secs * 1000).toLong
      val allMS   = (hours * 60 + minutes) * 60000 + millis
      span.clip(millisToFrames(allMS))
    } catch {
      case _: NumberFormatException => throw new ParseException(s, 0)
    }
  }

  def parse(s: String): Option[Long] = Try(tryParse(s)).toOption

  def format(value: Long): String = axis.format(value / sampleRate, pad = 12)
}