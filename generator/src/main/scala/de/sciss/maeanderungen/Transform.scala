/*
 *  Transform.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.maeanderungen.Layer.Context
import de.sciss.synth.proc.AudioCue

import scala.concurrent.Future

trait Transform {
  def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]]
}