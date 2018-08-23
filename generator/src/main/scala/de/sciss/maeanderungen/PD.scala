/*
 *  PD.scala
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

import de.sciss.lucre.stm.{Random, TxnLike}

import scala.language.implicitConversions

object PD {
  implicit def const(d: Double): PD = Const(d)

  def rangeRand(lo: Double, hi: Double)(implicit r: Random[TxnLike]): PD = {
    val lo1     = math.min(lo, hi)
    val hi1     = math.max(lo, hi)
    val span    = hi1 - lo1
    new RangeRand(lo1, hi1, span)
  }

  def expRand(lo: Double, hi: Double)(implicit r: Random[TxnLike]): PD = {
    val lo1     = math.min(lo, hi)
    val hi1     = math.max(lo, hi)
    val ratio0  = hi1 / lo1
    val ratio   = if (ratio0 > 0) ratio0 else 1
    new ExpRand(lo1, hi1, ratio)
  }

  private final case class Const(d: Double) extends PD {
    def sample()(implicit tx: TxnLike): Double = d
  }

  private final class RangeRand(lo: Double, hi: Double, span: Double)(implicit r: Random[TxnLike])
    extends PD {

    def sample()(implicit tx: TxnLike): Double = r.nextDouble() * span + lo
  }

  private final class ExpRand(lo: Double, hi: Double, ratio: Double)(implicit r: Random[TxnLike])
    extends PD {

    def sample()(implicit tx: TxnLike): Double =
      lo * math.exp(math.log(ratio) * r.nextDouble())
  }
}

trait PD {
  def sample()(implicit tx: TxnLike): Double
}
