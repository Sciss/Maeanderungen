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

import de.sciss.kollflitz.Vec
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.stm.{Obj, Random, Sys, TxnLike}
import de.sciss.span.Span
import de.sciss.synth.proc.Timeline

object Ops {
  implicit final class MaeanderungenIteratorIps[A](private val it: Iterator[A]) extends AnyVal {
    def headOption: Option[A] = if (it.hasNext) Some(it.next()) else None
  }

  implicit final class MaeanderungenTraversableOps[A](private val xs: Traversable[A]) extends AnyVal {
    def chooseWeighted[Tx <: TxnLike](weight: A => Double)(implicit tx: Tx, r: Random[Tx]): A = {
      require (xs.nonEmpty)
      val i    = r.nextDouble
      var sum  = 0.0
      xs.find { e => sum += weight(e); sum >= i } .getOrElse(xs.last)
    }
  }

  implicit final class MaeanderungenVecOps[A](private val xs: Vec[A]) extends AnyVal {
    def choose[Tx <: TxnLike]()(implicit tx: Tx, r: Random[Tx]): A = {
      require (xs.nonEmpty)
      val i   = r.nextInt(xs.size)
      xs(i)
    }
  }

  implicit final class MaenderungenTimelineOps[S <: Sys[S]](private val tl: Timeline[S]) extends AnyVal {
    def globalObjects(implicit tx: S#Tx): Iterator[Obj[S]] =
      tl.rangeSearch(Span.Until(BiGroup.MinCoordinate + 1), Span.From(BiGroup.MaxCoordinate - 1))
        .flatMap(_._2.map(_.value))
  }
}
