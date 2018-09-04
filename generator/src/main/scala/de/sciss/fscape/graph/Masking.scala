/*
 *  Masking.scala
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

package de.sciss.fscape
package graph

import de.sciss.fscape.UGenSource.unwrap
import de.sciss.fscape.stream.{StreamIn, StreamOut}
import de.sciss.kollflitz.Vec

final case class Masking(fg: GE, bg: GE, rows: GE, columns: GE, threshNoise: GE, threshMask: GE,
                         blurRows: GE, blurColumns: GE)
  extends UGenSource.SingleOut {

  protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
    unwrap(this, Vector(fg.expand, bg.expand, rows.expand, columns.expand,
      threshNoise.expand, threshMask.expand, blurRows.expand, blurColumns.expand))

  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGen.SingleOut =
    UGen.SingleOut(this, inputs = args)

  private[fscape] def makeStream(args: Vec[StreamIn])(implicit b: stream.Builder): StreamOut = {
    val Vec(fg, bg, rows, columns, threshNoise, threshMask, blurRows, blurColumns) = args
    stream.Masking(fg = fg.toDouble, bg = bg.toDouble, rows = rows.toInt, columns = columns.toInt,
      threshNoise = threshNoise.toDouble, threshMask = threshMask.toDouble,
      blurRows = blurRows.toInt, blurColumns = blurColumns.toInt)
  }
}
