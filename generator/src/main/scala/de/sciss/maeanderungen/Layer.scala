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

import de.sciss.lucre.expr.{BooleanObj, DoubleObj, LongObj}
import de.sciss.lucre.stm.{Folder, Random, Sys, TxnRandom}
import de.sciss.maeanderungen.Builder._
import de.sciss.maeanderungen.Ops._
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, TimeRef, Timeline, Workspace}

object Layer {
  final val attrSeed = "seed"

  final class Context[S <: Sys[S]](
      val tl            : Timeline.Modifiable[S],
      val numFrames     : Long,
      val category      : Category,
      val material      : AudioCue.Obj[S],
      val intelligible  : Boolean,
      val complete      : Boolean,
      val sequential    : Boolean,
    )(
      implicit val rnd: Random[S#Tx]
    )

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S], config: Config): Unit = {
    val root          = workspace.root
    val fRender       = mkFolder(root, "renderings")
    val now           = System.currentTimeMillis()
    val tlOpt         = fRender.iterator.collect {
      case tlm: Timeline.Modifiable[S] => tlm
    } .toList.lastOption

    val tl: Timeline.Modifiable[S] = tlOpt.getOrElse {
      val _tl   = Timeline[S]
      _tl.name  = s"Rendering-${now.toHexString}"
      fRender.addLast(_tl)
      _tl
    }

    val seed0: LongObj.Var[S] = tl.attr.$[LongObj](attrSeed) match {
      case Some(LongObj.Var(vr)) => vr
      case _ =>
        val _seed = LongObj.newVar[S](now)
        tl.attr.put(attrSeed, _seed)
        _seed
    }

    implicit val rnd: Random[S#Tx] = TxnRandom[S]()
    rnd.setSeed(seed0.value)
//    val rndUsed = Ref(false)

    val durObj = mkObjIn(tl, "dur", -1) {
      val pdDur   = PD.rangeRand(config.minDur, config.maxDur) // ; rndUsed() = true
      val durSec  = pdDur.sample()
      println(f"dur = $durSec%gs")
      DoubleObj.newVar(durSec)
    }

    val numFrames = (durObj.value * TimeRef.SampleRate).toLong
    val c         = Category.choose()
    val fMat      = root.![Folder]("material")

    val allMat = fMat.iterator.collect {
      case cue: AudioCue.Obj[S] if cue.name.startsWith(c.abbrev) => cue
    } .toVector

    val mat           = allMat.choose() // ; rndUsed() = true
    val mAttr         = mat.attr
    val intelligible  = mAttr.$[BooleanObj]("TV").fold(c.defaultIntelligible)(_.value)
    val complete      = mAttr.$[BooleanObj]("K" ).fold(c.defaultComplete    )(_.value)
    val sequential    = mAttr.$[BooleanObj]("H" ).fold(c.defaultSequential  )(_.value)
    val transformable = false // !sequential // this is currently synonymous
    val transform     = transformable && {
      val prob = if (c.isText) config.probTransformText else config.probTransformSound
      val coin = rnd.nextDouble() < prob // ; rndUsed() = true
      coin
    }

    implicit val ctx: Context[S] = new Context(
      tl            = tl,
      numFrames     = numFrames,
      category      = c,
      material      = mat,
      intelligible  = intelligible,
      complete      = complete,
      sequential    = sequential,
    )

    if (transform) {
      ???
    } else {
      if (c.isText) {
        putPlainText()
      } else {
        putPlainSound()
      }
    }

    ???

    /*

        val (span, obj) = ProcActions.mkAudioRegion(time = tlSpan,
          audioCue = audioCue, gOffset = drag.selection.start)

     */

    /* if (rndUsed()) */ {
      val seed1 = rnd.nextLong()
      seed0.update(seed1)
    }
  }

  def putPlainText[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._

    /*

      - (1) we determine a tentative length:
            - if the text is complete, using its total duration
            - otherwise, we select a span

      - (2) we determine the initial position based on the tentative length

      - (3) we attempt placement, segment by segment (*); if we find an obstacle,
            we try to insert pauses; if that's not possible, we return to
            step (2) using an earlier position; if there is no earlier position,
            we give up

      (*) we use a bounce of the previous version of the TL for comparison

     */

    if (complete || !(rnd.nextDouble() < config.probShorten) ) {
      putPlainTextFull()
    } else {  // shortened
      if (rnd.nextDouble() < config.probShortenFade) {
        putPlainTextFaded()
      } else {
        putPlainTextCut()
      }
    }
  }

  def putPlainTextFull[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    // if the text must be intelligible,
    // we compare against the background layer audio.
    // if not, we go ahead.

    // comparison against background layer:
    // we create a "smeared" and "max'ed" spectrogram
    // of the text layer, segment by segment.
    // for each segment of smeared data, we compare
    // against the background spectrogram and a masking
    // threshold. if the condition holds, we proceed to
    // the next segment. if it does not hold, we check
    // the underlying regions; if there are any regions
    // that can be dimmed (e.g. no texts that must be
    // intelligible), we create a working timeline with
    // tentative dimming, repeat the spectrogram; if
    // condition holds, we proceed to the next segment,
    // if not we may want to increase dimming until a
    // certain maximum amount or number of iterations has been reached.

    // dimming strategies:
    // - lower gain
    // - frequency filter (indeed that could be the "negative
    //   of the foreground")

    if (ctx.intelligible) {
      ???
    }
  }

  def putPlainTextFaded[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    ???
  }

  def putPlainTextCut[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    ???
  }

  def putPlainSound[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Unit = {
    ???
  }
}
