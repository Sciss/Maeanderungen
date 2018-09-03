/*
 *  Ops.scala
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

object Ops {
  implicit class MaeanderungenTraversableOps[A](private val xs: Traversable[A]) extends AnyVal {
    def chooseWeighted[Tx <: TxnLike](weight: A => Double)(implicit tx: Tx, r: Random[Tx]): A = {
      val i    = r.nextDouble
      var sum  = 0.0
      xs.find { e => sum += weight(e); sum >= i } .getOrElse(xs.last)
    }
  }
}
