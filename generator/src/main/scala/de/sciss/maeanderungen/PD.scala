/*
 *  PD.scala
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

import de.sciss.lucre.stm.Random

import scala.language.implicitConversions

object PD {
  implicit def const[Tx](d: Double): PD[Tx] = Const(d)

  def rangeRand[Tx](lo: Double, hi: Double)(implicit r: Random[Tx]): PD[Tx] = {
    val lo1     = math.min(lo, hi)
    val hi1     = math.max(lo, hi)
    val span    = hi1 - lo1
    new RangeRand(lo1, hi1, span)
  }

  def expRand[Tx](lo: Double, hi: Double)(implicit r: Random[Tx]): PD[Tx] = {
    val lo1     = math.min(lo, hi)
    val hi1     = math.max(lo, hi)
    val ratio0  = hi1 / lo1
    val ratio   = if (ratio0 > 0) ratio0 else 1
    new ExpRand(lo1, hi1, ratio)
  }

  private final case class Const[Tx](d: Double) extends PD[Tx] {
    def sample()(implicit tx: Tx): Double = d
  }

  private final class RangeRand[Tx](lo: Double, hi: Double, span: Double)(implicit r: Random[Tx])
    extends PD[Tx] {

    def sample()(implicit tx: Tx): Double = r.nextDouble() * span + lo
  }

  private final class ExpRand[Tx](lo: Double, hi: Double, ratio: Double)(implicit r: Random[Tx])
    extends PD[Tx] {

    def sample()(implicit tx: Tx): Double =
      lo * math.exp(math.log(ratio) * r.nextDouble())
  }
}

trait PD[-Tx] {
  def sample()(implicit tx: Tx): Double
}
