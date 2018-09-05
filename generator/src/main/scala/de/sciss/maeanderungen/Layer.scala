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

import de.sciss.equal.Implicits._
import de.sciss.kollflitz.Vec
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, SpanLikeObj}
import de.sciss.lucre.stm.{Folder, Random, Sys, TxnRandom}
import de.sciss.maeanderungen.Builder._
import de.sciss.maeanderungen.Ops._
import de.sciss.numbers.Implicits._
import de.sciss.mellite.ProcActions
import de.sciss.span.Span
import de.sciss.synth.Curve
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, FadeSpec, ObjKeys, Proc, TimeRef, Timeline, Workspace}

import scala.annotation.tailrec

object Layer {
  final val attrSeed    = "seed"
  final val attrPauses  = "pauses"
  final val attrBreak   = "break"
  final val attrIntel   = "TV"
  final val attrLoud95  = "loud-95"

  final case class Pause(span: Span, break: Boolean)

  final case class RegionAt(frame: Long, span: Span, pause: Option[Pause])

  final case class PlacedRegion(posTL: Long, region: RegionAt)

  final class Context[S <: Sys[S]](
                                    val tl            : Timeline.Modifiable[S],
                                    val tlNumFrames   : Long,
                                    val category      : Category,
                                    val material      : AudioCue.Obj[S],
                                    val matNumFrames  : Long,
                                    val loud95        : Double,
                                    val pauses        : Vec[Pause],
                                    val intelligible  : Boolean,
                                    val complete      : Boolean,
                                    val sequential    : Boolean,
    )(
      implicit val rnd: Random[S#Tx]
    )

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S], config: Config): Unit = {
    val root          = workspace.root
    val fRender       = mkFolder(root, "renderings")
    val now           = 0L // XXX TODO System.currentTimeMillis()
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
    val intelligible  = mAttr.$[BooleanObj](attrIntel).fold(c.defaultIntelligible)(_.value)
    val complete      = mAttr.$[BooleanObj]("K" ).fold(c.defaultComplete    )(_.value)
    val sequential    = mAttr.$[BooleanObj]("H" ).fold(c.defaultSequential  )(_.value)
    val transformable = false // !sequential // this is currently synonymous
    val transform     = false /* XXX TODO */ && transformable && {
      val prob = if (c.isText) config.probTransformText else config.probTransformSound
      val coin = rnd.nextDouble() < prob // ; rndUsed() = true
      coin
    }

    val pauses: Vec[Pause] = {
      val tl = mat.attr.![Timeline](attrPauses)
      val it = tl.iterator.flatMap {
        case (span: Span, vec) =>
          vec.collect {
            case BiGroup.Entry(_, i: IntObj[S]) =>
              val break = i.attr.$[BooleanObj](attrBreak).exists(_.value)
              Pause(span = span, break = break)
          }

        case _ => Nil
      }
      it.toVector
    }

    val matV          = mat.value
    val matNumFrames  = (matV.numFrames.toDouble / matV.sampleRate * TimeRef.SampleRate).toLong
    val loud95        = mat.attr.![DoubleObj](attrLoud95).value

    implicit val ctx: Context[S] = new Context(
      tl            = tl,
      tlNumFrames   = numFrames,
      category      = c,
      material      = mat,
      loud95        = loud95,
      matNumFrames  = matNumFrames,
      pauses        = pauses,
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

    if (true /* XXX TODO */ || complete || !(rnd.nextDouble() < config.probShorten) ) {
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

    if (true /* XXX TODO */ || ctx.intelligible) {
      putPlainTextFullIntel()
    } else {
      ???
    }
  }

  def matRegionAt[S <: Sys[S]](frame: Long, includePause: Boolean = false, padRight: Long = 0L)
                              (implicit ctx: Context[S]): Option[RegionAt] = {
    import ctx._
    require (frame >= 0L && frame < matNumFrames, s"0 <= $frame < $matNumFrames")
    val i = pauses.indexWhere(_.span.start > frame)
    if (i < 0) {
      val j     = pauses.lastIndexWhere(_.span.stop <= frame)
      val start = if (j >= 0) pauses(j).span.stop else 0L
      val stop  = matNumFrames
      Some(RegionAt(frame, Span(start, stop), None))
    } else {
      val start     = if (i === 0) 0L else pauses(i - 1).span.stop
      val pause     = pauses(i)
      val stop      = math.min(matNumFrames, (if (includePause) pause.span.stop else pause.span.start) + padRight)
      Some(RegionAt(frame, Span(start, stop), Some(pause)))
    }
  }

  def getIntelOverlapping[S <: Sys[S]](span: Span)
                                      (implicit tx: S#Tx, ctx: Context[S]): Option[(SpanLikeObj[S], Proc[S])] =
    getRegionOverlapping[S](span)((_, pr) => pr.attr.$[BooleanObj](attrIntel).exists(_.value))


  def existsRegionOverlapping[S <: Sys[S]](span: Span)(implicit tx: S#Tx, ctx: Context[S]): Boolean =
    getRegionOverlapping[S](span)((_, _) => true).isDefined

  def getRegionOverlapping[S <: Sys[S]](span: Span)(p: (SpanLikeObj[S], Proc[S]) => Boolean)
                                       (implicit tx: S#Tx, ctx: Context[S]): Option[(SpanLikeObj[S], Proc[S])] = {
    import ctx._
    val it = tl.intersect(span).flatMap {
      case (_: Span, vec) =>
        vec.collectFirst {
          case BiGroup.Entry(sp, pr: Proc[S]) if p(sp, pr) => (sp, pr)
        }

      case _ => None
    }
    it.headOption
  }

  // puts a plain (non-transformed) text in full, retaining intelligibility
  def putPlainTextFullIntel[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    val placementOpt  = tryPlacePlainTextFullIntel[S]()
    val gainVal       = DoubleObj .newConst[S]((65.0 - loud95).dbAmp)
    val intelObj      = BooleanObj.newVar[S](intelligible)
    placementOpt.foreach { placement =>
      val vec = placement.toVector
      for (i <- vec.indices) {
        val placed = vec(i)
        val fadeIn = if (i == 0) 0L else {
          val pred = vec(i - 1)
          if (placed.posTL === pred.posTL + pred.region.span.length) 0L
          else {
            pred.region.pause.fold(0L)(pause => pause.span.length / 2)
          }
        }
        val fadeOut = if (i == vec.size - 1) 0L else {
          val succ = vec(i + 1)
          if (placed.posTL + placed.region.span.length === succ.posTL) 0L
          else {
            placed.region.pause.fold(0L)(pause => pause.span.length / 2)
          }
        }
        val startTL = placed.posTL - fadeIn
        val stopTL  = startTL + fadeIn + fadeOut + placed.region.span.length
        val gOffset = placed.region.span.start - fadeIn
        val spanTL  = Span(startTL, stopTL)
        val (spanObjTL, pr)  = ProcActions.mkAudioRegion(time = spanTL, audioCue = material, gOffset = gOffset)
        val prAttr = pr.attr
        if (fadeIn > 0L) {
          val spec  = FadeSpec(fadeIn, Curve.lin)
          val fd    = FadeSpec.Obj.newVar[S](spec)
          prAttr.put(ObjKeys.attrFadeIn, fd)
        }
        if (fadeOut > 0L) {
          val spec  = FadeSpec(fadeOut, Curve.lin)
          val fd    = FadeSpec.Obj.newVar[S](spec)
          prAttr.put(ObjKeys.attrFadeOut, fd)
        }
        val gainObj = DoubleObj.newVar[S](gainVal)
        prAttr.put(ObjKeys.attrGain, gainObj)
        prAttr.put(attrIntel, intelObj)

        tl.add(spanObjTL, pr)
      }
    }
  }

    // puts a plain (non-transformed) text in full, retaining intelligibility
  def tryPlacePlainTextFullIntel[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S],
                                                config: Config): Option[List[PlacedRegion]] = {
    import ctx._

    val pos0TLTry = (rnd.nextDouble() * (tlNumFrames - matNumFrames)).toLong
    val reg0Opt = matRegionAt(0L, includePause = true)
    if (reg0Opt.isEmpty) return None

    val reg0 = reg0Opt.get

    @tailrec
    def findLeft(reg: RegionAt, posTry: Long): Option[Long] = {
      val shifted = Span(posTry, posTry + reg.span.length)
      getIntelOverlapping(shifted) match {
        case None => Some(posTry)
        case Some((sp, _)) =>
          sp.value match {
            case sp: Span =>
              val posNext = sp.start - shifted.length
              if (posNext >= 0L) findLeft(reg, posNext)
              else None

            case _ => None
          }
      }
    }

    @tailrec
    def findRight(reg: RegionAt, posTry: Long): Option[Long] = {
      val shifted = Span(posTry, posTry + reg.span.length)
      getIntelOverlapping(shifted) match {
        case None => Some(posTry)
        case Some((sp, _)) =>
          sp.value match {
            case sp: Span =>
              val posNext = sp.stop
              if (posNext + (matNumFrames - reg.span.start) <= tlNumFrames) findRight(reg, posNext)
              else None

            case _ => None
          }
      }
    }

    val pos0TLOpt = findLeft(reg0, pos0TLTry) orElse findRight(reg0, pos0TLTry)
    if (pos0TLOpt.isEmpty) return None

    val placement = List.newBuilder[PlacedRegion]

    val pos0TL    = pos0TLOpt.get
    var posCurrTL = pos0TL
    placement += PlacedRegion(pos0TL, reg0)
    var currReg   = reg0
    while (currReg.span.stop < matNumFrames) {
      val posNextTLTry  = posCurrTL + currReg.span.length
      val nextReg       = matRegionAt(currReg.span.stop + 1, includePause = true).get
      val posNextTLOpt  = findRight(nextReg, posNextTLTry)
      if (posNextTLOpt.isEmpty) return None
      val posNextTL     = posNextTLOpt.get
      val shift         = posNextTL - posNextTLTry
      val shiftOk       = shift === 0L || currReg.pause.forall(_.break) || shift < currReg.pause.fold(0L)(_.span.length)
      if (!shiftOk) return None

      placement += PlacedRegion(posNextTL, nextReg)
      currReg           = nextReg
      posCurrTL         = posNextTL
    }

    val res = placement.result()
    Some(res)
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
