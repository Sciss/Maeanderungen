/*
 *  Layer.scala
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

import de.sciss.lucre.expr.{DoubleObj, LongObj}
import de.sciss.lucre.stm.{Folder, Random, Sys, TxnRandom}
import de.sciss.maeanderungen.Builder._
import de.sciss.maeanderungen.Generator.Config
import de.sciss.synth.proc.Implicits._
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.synth.proc.{AudioCue, TimeRef, Timeline, Workspace}

import scala.concurrent.stm.Ref

object Layer {
  final val attrSeed = "seed"

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S], config: Config): Unit = {
    val root          = workspace.root
    val fRender       = mkFolder(root, "renderings")
    val now           = System.currentTimeMillis()
    val tlOpt         = fRender.iterator.collect {
      case tlm: Timeline.Modifiable[S] => tlm
    } .toList.lastOption

    val tl = tlOpt.getOrElse {
      val _tl   = Timeline[S]
      _tl.name  = s"Rendering-${now.toHexString}"
      fRender.addLast(_tl)
      _tl
    }

    val seed0 = tl.attr.$[LongObj](attrSeed).getOrElse {
      val _seed = LongObj.newVar[S](now)
      tl.attr.put(attrSeed, _seed)
      _seed
    }

    implicit val rnd: Random[S#Tx] = TxnRandom[S]()
    rnd.setSeed(seed0.value)
    val rndUsed = Ref(false)

    val durObj = mkObjIn(tl, "dur", -1) {
      val pdDur   = PD.rangeRand(config.minDur, config.maxDur)
      rndUsed()   = true
      val durSec  = pdDur.sample()
      println(f"dur = $durSec%gs")
      DoubleObj.newVar(durSec)
    }

    val numFrames = (durObj.value * TimeRef.SampleRate).toLong
    val c         = Category.choose()
    val fMat      = root.![Folder]("material")

    fMat.iterator.collect {
      case cue: AudioCue.Obj[S] if cue.name.startsWith(c.abbrev) => cue
    }

    ???

    /*

        val (span, obj) = ProcActions.mkAudioRegion(time = tlSpan,
          audioCue = audioCue, gOffset = drag.selection.start)

     */
  }
}
