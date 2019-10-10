/*
 *  RhizomePos.scala
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

import scala.util.Random

object RhizomePos {
  val sequence: Seq[Int] = Seq(
    14, 15,  2,  3, 18, 40, 26,  5,  4,  1,
     8,  7, 38, 33, 37, 39, 28, 23, 10,  9,
    16, 13, 21, 22, 17, 19, 35,  6, 34, 36,
    20, 32, 11, 12, 31, 25, 29, 24, 27, 41,
  )

  val totalDur: Double  = 25 * 60.0
  val center  : Double  = 2.0/3 * totalDur

  def formatTimeSecs(value: Double, decimals: Int = 3, pad: Int = 0, hours: Boolean = false): String = {
    val dec     = decimals > 0
    val neg     = value < 0
    val m       = (value * 1000).toInt
    val mil0    = if (neg) -m else m
    val mil     = mil0 % 1000
    val secs0   = mil0 / 1000
    val secs    = secs0 % 60
    val mins0   = secs0 / 60
    val mins    = (if (hours) mins0 % 60 else mins0).toString
    val ml      = if (hours && mins.length < 2) 2 else mins.length
    val h       = if (hours) (mins0 / 60).toString else ""
    val hl      = h.length
    val hl1     = if (neg) hl + 1 else hl
    val sz      = (if (hours) hl1 + 1 else 0) + ml + 3 + (if (dec) decimals + 1 else 0)
    // println(s"sz = $sz")
    val sb      = new java.lang.StringBuilder(if (pad > sz) pad else sz)

    if (neg) sb.append('-')
    if (pad > sz) {
      var i = pad - sz
      while (i > 0) {
        sb.append(' ')
        i -= 1
      }
    }
    if (pad == 0 || pad >= sz) {
      if (hours) {
        sb.append(h)
        sb.append(':')
        if (mins.length == 1) sb.append('0')
        sb.append(mins)
      } else {
        sb.append(mins)
      }
    } else {
      sb.append('*')
      val i = sz - pad - sb.length + 2 // sb.length
      if (hours) {
        sb.append(h, i, hl)
        sb.append(':')
        if (mins.length == 1) sb.append('0')
        sb.append(mins)
      } else {
        sb.append(mins, i, ml)
      }
    }

    sb.append(':')
    sb.append(((secs / 10) + 48).toChar)
    sb.append(((secs % 10) + 48).toChar)
    if (dec) {
      sb.append('.')
      sb.append(( (mil / 100) + 48).toChar)
      if (decimals > 1) {
        sb.append((((mil /  10) % 10) + 48).toChar)
        if (decimals > 2) sb.append(( (mil %  10) + 48).toChar)
      }
    }

    sb.toString
  }

  def main(args: Array[String]): Unit = {
    var timesS = Seq(-1.0)
    while (timesS.exists(t => t < 90.0 || t > totalDur - 90.0) || timesS.forall(_ > 180.0) || timesS.forall(_ < totalDur - 180.0)) {
      implicit val r: Random = new Random()
      val times = Seq.fill(sequence.size)(genCauchy(gamma = 0.6667 /* 0.33333 */ * (totalDur/8), x0 = center))
      timesS = times.sorted
    }
    (timesS zip sequence).foreach { case (time, idx) =>
      println(f"- at $time%gs = ${formatTimeSecs(time, pad = 9)}, no. $idx%2s")
    }
  }

  def genCauchy(gamma: Double = 1.0, x0: Double = 0.0)(implicit r: Random): Double = {
    val y = r.nextDouble()
    math.tan((y - 0.5) * math.Pi) * gamma + x0
  }
}
