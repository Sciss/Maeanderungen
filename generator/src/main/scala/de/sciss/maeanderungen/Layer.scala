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

import java.text.SimpleDateFormat
import java.util.Locale

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.kollflitz.{ISeq, Vec}
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj}
import de.sciss.lucre.stm.{Cursor, Folder, Random, Sys, TxnRandom}
import de.sciss.maeanderungen.Builder._
import de.sciss.maeanderungen.LayerUtil._
import de.sciss.maeanderungen.Ops._
import de.sciss.mellite.gui.ObjView
import de.sciss.numbers.Implicits._
import de.sciss.span.Span
import de.sciss.synth.Curve
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, FadeSpec, Proc, TimeRef, Timeline, Workspace}

import scala.annotation.tailrec

object Layer {
  final val DEFAULT_VERSION = 1

  final val attrSeed    = "seed"
  final val attrPauses  = "pauses"
  final val attrBreak   = "break"
  final val attrIntel   = "TV"
  final val attrLoud95  = "loud-95"

  final case class Pause(span: Span, break: Boolean)

  final case class RegionAt(frame: Long, span: Span, pause: Option[Pause],
                            fadeIn: FadeSpec = FadeSpec(0L), fadeOut: FadeSpec = FadeSpec(0L))

  final case class PlacedRegion(posTL: Long, region: RegionAt, relativeGain: Double = 1.0)

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
                                    val pan           : Double,
                                    val pTape1        : Proc[S],
                                    val pTape2        : Proc[S],
                                    val pMain         : Proc[S],
    )(
      implicit val rnd: Random[S#Tx], val workspace: Workspace[S], val cursor: Cursor[S]
    )

  def log(what: => String): Unit =
    println(s"[log] $what")

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S], config: Config): Unit = {
    implicit val cursor: Cursor[S] = workspace.cursor
    val root          = workspace.root
    val fRender       = mkFolder(root, "renderings")
    val fAux          = mkFolder(root, "aux")
    val pTape1        = mkObj[S, Proc](fAux, "tape-1", DEFAULT_VERSION)(mkTapeGraph(1))
    val pTape2        = mkObj[S, Proc](fAux, "tape-2", DEFAULT_VERSION)(mkTapeGraph(2))
    val now           = System.currentTimeMillis()
    val tlOpt         = fRender.iterator.collect {
      case tlm: Timeline.Modifiable[S] => tlm
    } .toList.lastOption

    val tl: Timeline.Modifiable[S] = tlOpt.getOrElse {
      val _tl   = Timeline[S]
      addDefaultGlobalProc(_tl)
      val format  = new SimpleDateFormat("'Rendering-'yyMMdd'_'HHmmss", Locale.US)
      _tl.name    = format.format(new java.util.Date(now))
      fRender.addLast(_tl)
      _tl
    }
    val pMain = tl.globalObjects.next() match {
      case p: Proc[S] => p
      case _ => sys.error("Could not find main proc")
    }

    val seed0: LongObj.Var[S] = tl.attr.$[LongObj](attrSeed) match {
      case Some(LongObj.Var(vr)) => vr
      case _ =>
        val _seed = LongObj.newVar[S](config.seed.getOrElse(now): Long)
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
    val c         = {
      val rat     = config.textSoundRatio
      val prob    = rat / (rat + 1.0)
      val isText  = prob.coin()
      if (isText) Category.chooseText() else Category.chooseSound()
    }
    log(s"Category: $c")

    val fMat      = root.![Folder]("material")

    val catMat = fMat.iterator.collect {
      case cue: AudioCue.Obj[S] if cue.name.contains /* startsWith */(c.abbrev) => cue
    } .toVector

    val mat           = catMat.choose()
    val matV          = mat.value

    log(s"Material: ${matV.artifact.base}")

    val mAttr         = mat.attr
    val intelligible  = mAttr.$[BooleanObj](attrIntel).fold(c.defaultIntelligible)(_.value)
    val complete      = mAttr.$[BooleanObj]("K" ).fold(c.defaultComplete    )(_.value)
    val sequential    = mAttr.$[BooleanObj]("H" ).fold(c.defaultSequential  )(_.value)
    val transformable = !sequential // this is currently synonymous
    val transform     = transformable && {
      val prob = if (c.isText) config.probTransformText else config.probTransformSound
      prob.coin()
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

    val matNumFrames  = (matV.numFrames.toDouble / matV.sampleRate * TimeRef.SampleRate).toLong
    val loud95        = mat.attr.![DoubleObj](attrLoud95).value

    val pan = if ((matV.numChannels !== 1) || config.maxPan <= 0) 0.0 else {
      val x = rnd.nextDouble()
      x.linLin(0.0, 1.0, -config.maxPan, +config.maxPan)
    }

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
      pan           = pan,
      pTape1        = pTape1,
      pTape2        = pTape2,
      pMain         = pMain
    )

    if (transform) {
      if (c.isText) {
        putTransformedText()
      } else {
        putTransformedSound()
      }
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

  def putTransformedText[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    ???
  }

  def putTransformedSound[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    ???
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
      putPlainTextFullIntel()
    } else {
      ???
    }
  }

  // puts a plain (non-transformed) text in full, retaining intelligibility
  def putPlainTextFullIntel[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    val matSpan = Span(0L, ctx.matNumFrames)
    putPlainTextIntel(matSpan)
  }

  def executePlacement[S <: Sys[S]](placement: ISeq[PlacedRegion], gain: Double)
                                   (implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    val vec       = placement.toVector
    val intelObj  = BooleanObj.newVar[S](intelligible)
    val colorVal  = mkColor(material)
    for (i <- vec.indices) {
      val placed = vec(i)
      val fadeIn = if (placed.region.fadeIn.numFrames > 0 || i == 0) placed.region.fadeIn else {
        val pred = vec(i - 1)
        val len = if (placed.posTL === pred.posTL + pred.region.span.length) 0L
        else {
          pred.region.pause.fold(0L)(pause => pause.span.length / 2)
        }
        FadeSpec(len)
      }
      val fadeOut = if (placed.region.fadeOut.numFrames > 0 || i == vec.size - 1) placed.region.fadeOut else {
        val succ = vec(i + 1)
        val len = if (placed.posTL + placed.region.span.length === succ.posTL) 0L
        else {
          placed.region.pause.fold(0L)(pause => pause.span.length / 2)
        }
        FadeSpec(len)
      }
      val startTL = placed.posTL - fadeIn.numFrames
      // Note: fade-out implies we have a successive pause which is included
      // in region.span; to avoid plops at the end, we stop after half the
      // pause when fading; therefore, do not add the fade-out time here
      val stopTL  = startTL + fadeIn.numFrames + /* fadeOut.numFrames + */ placed.region.span.length
      val gOffset = placed.region.span.start - fadeIn.numFrames
      val spanTL  = Span(startTL, stopTL)
      val gainVal = gain * placed.relativeGain
      val (_, pr)  = mkAudioRegion(tl, time = spanTL, audioCue = material, gOffset = gOffset,
        fadeIn = fadeIn, fadeOut = fadeOut, gain = gainVal)
      val prAttr = pr.attr
      prAttr.put(attrIntel, intelObj)
      prAttr.put(ObjView.attrColor, /* Color.Obj.newVar( */ colorVal /* ) */)
    }
  }

    // puts a plain (non-transformed) text in full, retaining intelligibility
  def putPlainTextIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    val placementOpt  = tryPlacePlainIntel[S](matSpan = matSpan)
    val gainVal       = (65.0 - loud95).dbAmp
    placementOpt.foreach { placement =>
      executePlacement(placement, gain = gainVal)
    }
  }

    // puts a plain (non-transformed) object with pauses, retaining intelligibility
  def tryPlacePlainIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S],
                                                     config: Config): Option[List[PlacedRegion]] = {
    import ctx._

    val pos0TLTry = (rnd.nextDouble() * (tlNumFrames - matSpan.length)).toLong
    val reg0Opt = matRegionAt(matSpan.start, includePause = true)
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
              if (posNext + (matSpan.length - reg.span.start) <= tlNumFrames) findRight(reg, posNext)
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
    while (currReg.span.stop < matSpan.length) {
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
    import ctx._
    val startPos0     = (rnd.nextDouble() * matNumFrames).toLong
    val cutLen0       = math.max(TimeRef.SampleRate, rnd.nextDouble() * matNumFrames).toLong
    val breakPauses   = pauses.filter(_.break)
    val i             = breakPauses.indexWhere(_.span.start >= startPos0)
    val startPos      = if (i < 0) 0L else breakPauses(i).span.stop
    val stopPos0      = startPos + cutLen0
    val j             = breakPauses.indexWhere(_.span.start >= stopPos0, i + 1)
    val stopPos       = if (j < 0) matNumFrames - startPos else breakPauses(j).span.start
    val matSpan = Span(startPos, stopPos)
    putPlainTextIntel(matSpan)
  }

  def putPlainSound[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    if (complete) {
      putPlainSoundFull[S]()
    } else {  // shortened
      putPlainSoundCut()
    }
  }

  def putPlainSoundFull[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    val matSpan = Span(0L, ctx.matNumFrames)
    putPlainSoundIntel(matSpan)
  }

  def putPlainSoundCut[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    val startPos0     = (rnd.nextDouble() * matNumFrames).toLong
    val cutLen0       = math.max(TimeRef.SampleRate, rnd.nextDouble() * matNumFrames).toLong
    val breakPauses   = pauses.filter(_.break)
    if (breakPauses.nonEmpty && 0.5.coin()) {
      val i             = breakPauses.indexWhere(_.span.start >= startPos0)
      val startPos      = if (i < 0) 0L else breakPauses(i).span.stop
      val stopPos0      = startPos + cutLen0
      val j             = breakPauses.indexWhere(_.span.start >= stopPos0, i + 1)
      val stopPos       = if (j < 0) matNumFrames - startPos else breakPauses(j).span.start
      val matSpan = Span(startPos, stopPos)
      putPlainTextIntel(matSpan)
    } else {
      val gainVal0      = (65.0 - loud95).dbAmp
      val gainVal       = rnd.nextDouble().linLin(0.0, 1.0, -18.0, 0.0).dbAmp * gainVal0
      val startPos      = startPos0
      val stopPos       = startPos + cutLen0
      val matSpan       = Span(startPos, stopPos)
      val matDur        = matSpan.length / TimeRef.SampleRate
      val fadeInS       = rnd.nextDouble().linExp(0.0, 1.0, 1.0, matDur - 1.0)
      val fadeOutS      = math.min(matDur - fadeInS, rnd.nextDouble().linExp(0.0, 1.0, 1.0, matDur - 1.0))
      val fadeIn        = FadeSpec((fadeInS  * TimeRef.SampleRate).toLong, Curve.sine)
      val fadeOut       = FadeSpec((fadeOutS * TimeRef.SampleRate).toLong, Curve.sine)
      val posTL         = (rnd.nextDouble() * (tlNumFrames - matSpan.length)).toLong
      val region        = RegionAt(frame = startPos, span = matSpan, pause = None,
        fadeIn = fadeIn, fadeOut = fadeOut)
      val placed        = PlacedRegion(posTL = posTL, region = region)
      val placement     = placed :: Nil

      executePlacement(placement, gain = gainVal)
    }
  }

  def putPlainSoundIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx._
    val placementOpt  = tryPlacePlainIntel[S](matSpan = matSpan)
    val gainVal       = (65.0 - loud95).dbAmp // XXX TODO --- we should probably take loud50 into account as well
    placementOpt.foreach { placement =>
      executePlacement(placement, gain = gainVal)
    }
  }
}
