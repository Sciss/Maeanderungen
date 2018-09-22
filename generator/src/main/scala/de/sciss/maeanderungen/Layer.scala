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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Folder, Random, TxnRandom}
import de.sciss.lucre.synth.Sys
import de.sciss.maeanderungen.Builder._
import de.sciss.maeanderungen.LayerUtil._
import de.sciss.maeanderungen.Ops._
import de.sciss.maeanderungen.Util.{framesToTime, spanToTime}
import de.sciss.mellite.gui.ObjView
import de.sciss.mellite.gui.edit.Edits
import de.sciss.numbers.Implicits._
import de.sciss.span.Span
import de.sciss.synth.Curve
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, FadeSpec, Proc, SoundProcesses, TimeRef, Timeline, Workspace}

import scala.annotation.tailrec
import scala.concurrent.Future

object Layer {
  final val DEFAULT_VERSION = 1

  final val attrSeed    = "seed"
  final val attrPauses  = "pauses"
  final val attrBreak   = "break"
  final val attrIntel   = "TV"
  final val attrLoud80  = "loud-80"
  final val attrLoud95  = "loud-95"

  final case class Pause(span: Span, break: Boolean)

  final case class RegionAt(frame: Long, span: Span, pause: Option[Pause],
                            fadeIn: FadeSpec = FadeSpec(0L), fadeOut: FadeSpec = FadeSpec(0L)) {

    override def toString: String =
      s"$productPrefix(frame = $frame (${framesToTime(frame)}), span = $span (${spanToTime(span)}), pause = $pause, " +
        s"fadeIn = $fadeIn, fadeOut = $fadeOut)"
  }

  final case class PlacedRegion(posTL: Long, region: RegionAt, relativeGain: Double = 1.0) {
    override def toString: String =
      s"$productPrefix(posTL = $posTL (${framesToTime(posTL)}), region = $region, relativeGain = $relativeGain)"
  }

  final case class PartialContext[S <: Sys[S]](
                                                material      : AudioCue.Obj[S],
                                                matNumFrames  : Long,
                                                loud80        : Double,
                                                loud95        : Double,
                                                pauses        : Vec[Pause],
                                                intelligible  : Boolean,
                                                complete      : Boolean,
                                                sequential    : Boolean,
                                              ) {

    def copyTo(context: Context[S]): Context[S] = {
      import context.{cursor, rnd, workspace}
      context.copy(
        material      = material,
        matNumFrames  = matNumFrames,
        loud95        = loud95,
        pauses        = pauses,
        intelligible  = intelligible,
        complete      = complete,
        sequential    = sequential
      )
    }
  }

  final case class Context[S <: Sys[S]](
                                    tl            : Timeline.Modifiable[S],
                                    tlNumFrames   : Long,
                                    category      : Category,
                                    material      : AudioCue.Obj[S],
                                    matNumFrames  : Long,
                                    loud80        : Double,
                                    loud95        : Double,
                                    pauses        : Vec[Pause],
                                    intelligible  : Boolean,
                                    complete      : Boolean,
                                    sequential    : Boolean,
                                    pan           : Double,
                                    chanOff       : Int,
                                    pTape1        : Proc[S],
                                    pTape2        : Proc[S],
                                    pTapeAll      : Proc[S],
                                    pMain         : Proc[S],
                                    folderTemp    : Folder[S],
    )(
      implicit val rnd: Random[S#Tx], val workspace: Workspace[S], val cursor: Cursor[S]
    )

  def log(what: => String): Unit =
    println(s"[log] $what")

  def mkPartialContext[S <: Sys[S]](c: Category, mat: AudioCue.Obj[S])(implicit tx: S#Tx): PartialContext[S] = {
    val mAttr         = mat.attr
    val intelligible  = mAttr.$[BooleanObj](attrIntel ).fold(c.defaultIntelligible)(_.value)
    val complete      = mAttr.$[BooleanObj]("K"       ).fold(c.defaultComplete    )(_.value)
    val sequential    = mAttr.$[BooleanObj]("H"       ).fold(c.defaultSequential  )(_.value)

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
    val loud80        = mat.attr.![DoubleObj](attrLoud80).value
    val loud95        = mat.attr.![DoubleObj](attrLoud95).value

    PartialContext(
      material      = mat,
      matNumFrames  = matNumFrames,
      loud80        = loud80,
      loud95        = loud95,
      pauses        = pauses,
      intelligible  = intelligible,
      complete      = complete,
      sequential    = sequential
    )
  }

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S], config: Config): Future[Unit] = {
    implicit val cursor: Cursor[S] = workspace.cursor
    val root          = workspace.root
    val fRender       = mkFolder(root, "renderings")
    val fTemp         = mkFolder(root, "temp")
    val fAux          = mkFolder(root, "aux")
    val pTape1        = mkObj[S, Proc](fAux, "tape-1"   , DEFAULT_VERSION)(mkTapeGraph(1))
    val pTape2        = mkObj[S, Proc](fAux, "tape-2"   , DEFAULT_VERSION)(mkTapeGraph(2))
    val pTapeAll      = mkObj[S, Proc](fAux, "tape-all" , DEFAULT_VERSION)(mkTapeGraphAll())
    val now           = System.currentTimeMillis()
    val tlOpt         = fRender.iterator.collect {
      case tlm: Timeline.Modifiable[S] => tlm
    } .toList.lastOption

    val tl: Timeline.Modifiable[S] = tlOpt.getOrElse {
      val _tl     = Timeline[S]
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
      case Some(LongObj.Var(vr)) if !config.forceSeed => vr
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

    val mat     = catMat.choose()
    val partial = mkPartialContext(c, mat)
    val matV    = mat.value

    log(s"Material: ${matV.artifact.base}")

    val transformable = !partial.sequential // this is currently synonymous
    val transform     = transformable && {
      val prob = if (c.isText) config.probTransformText else config.probTransformSound
      prob.coin()
    }

    val pan = if ((matV.numChannels !== 1) || config.maxPan <= 0) 0.0 else {
      val x = rnd.nextDouble()
      x.linLin(0.0, 1.0, -config.maxPan, +config.maxPan)
    }

    val chanOff = rangeRand(0, config.numChannels - 2)
    println(s"chanOff = $chanOff")

    implicit val ctx: Context[S] = Context(
      tl            = tl,
      tlNumFrames   = numFrames,
      category      = c,
      material      = partial.material,
      loud80        = partial.loud80,
      loud95        = partial.loud95,
      matNumFrames  = partial.matNumFrames,
      pauses        = partial.pauses,
      intelligible  = partial.intelligible,
      complete      = partial.complete,
      sequential    = partial.sequential,
      pan           = pan,
      chanOff       = chanOff,
      pTape1        = pTape1,
      pTape2        = pTape2,
      pTapeAll      = pTapeAll,
      pMain         = pMain,
      folderTemp    = fTemp,
    )

    val futRender = if (transform) {
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

    mapTx[S, Unit, Unit](futRender) { implicit tx => _ =>
      val seed1 = rnd.nextLong()
      seed0.update(seed1)
    }
  }

  def putTransformedText[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx.{cursor, rnd}
    val t = TextTransforms.choose()
    log(s"putTransformedText - $t")
    val futT = t.make[S]()
    flatMapTx[S, stm.Source[S#Tx, AudioCue.Obj[S]], Unit](futT) { implicit tx =>cueH =>
      val cue     = cueH()
      val partial = mkPartialContext(Category.HybridSound, cue)
      val ctxNew  = partial.copyTo(ctx)

      if (ctx.complete) {
        putPlainSoundFull ()(tx, ctxNew, config)
      } else {
        putPlainSoundCut  ()(tx, ctxNew, config)
      }
    }
  }

  def putTransformedSound[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx.{cursor, rnd}
    val t = SoundTransforms.choose()
    log(s"putTransformedSound - $t")
    val futT = t.make[S]()
    flatMapTx[S, stm.Source[S#Tx, AudioCue.Obj[S]], Unit](futT) { implicit tx =>cueH =>
      val cue     = cueH()
      val partial = mkPartialContext(Category.HybridSound, cue)
      val ctxNew  = partial.copyTo(ctx)

      if (ctx.complete) {
        putPlainSoundFull ()(tx, ctxNew, config)
      } else {
        putPlainSoundCut  ()(tx, ctxNew, config)
      }
    }
  }

  def putPlainText[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
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
      if (!Util.alwaysTrue /* XXX TODO */ && rnd.nextDouble() < config.probShortenFade) {
        putPlainTextFaded()
      } else {
        putPlainTextCut()
      }
    }
  }

  def putPlainTextFull[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
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

    if (Util.alwaysTrue /* XXX TODO */ || ctx.intelligible) {
      putPlainTextFullIntel()
    } else {
      ??? // XXX TODO
    }
  }

  // puts a plain (non-transformed) text in full, retaining intelligibility
  def putPlainTextFullIntel[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    val matSpan = Span(0L, ctx.matNumFrames)
    putPlainTextIntel(matSpan)
  }

  case class NamedSpan(span: Span, name: String)

  def groupWithinProximity[S <: Sys[S]](tl: Timeline[S], maxDistanceSec: Double)
                                       (implicit tx: S#Tx): Vec[NamedSpan] = {
    val b           = Vec.newBuilder[NamedSpan]
    val maxDistFr   = (maxDistanceSec * TimeRef.SampleRate).toLong
    var current     = Option.empty[NamedSpan]

    def flush(): Unit =
      current.foreach(b += _)

    tl.iterator.foreach {
      case (sp: Span, vec) =>
        val name = vec.head.value.name
        current match {
          case None =>
            current   = Some(NamedSpan(sp, name))
          case Some(NamedSpan(sp2, name2)) =>
            if (sp2.shift(maxDistFr).touches(sp)) {
              current = Some(NamedSpan(sp union sp2, Util.unite(name2, name)))
            } else {
              flush()
              current = Some(NamedSpan(sp, name))
            }
        }

      case _ =>
    }

    flush()
    b.result()
  }

  def executeReplacement[S <: Sys[S]](replace: Replacement[S], cueBg: AudioCue.Obj[S], offCue: Long)
                                     (implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx.cursor
    replace.map.foreach {
      case (oldEntry, newList) =>
        oldEntry match {
          case pOut: Proc[S] =>
            ctx.tl.globalObjects.foreach {
              case pIn: Proc[S] =>
                Edits.findLink(out = pOut, in = pIn).foreach { lnk =>
                  Edits.removeLink(lnk)
                }
              case _ =>
            }
          case _ =>
        }
        ctx.tl.remove(oldEntry.span, oldEntry.value)
        newList.foreach {
          case (spanNew, objNew) =>
            ctx.tl.add(spanNew, objNew)
            objNew match {
              case pNew: Proc[S] =>
                val out = pNew.outputs.add(Proc.mainOut)
                Edits.addLink(source = out, sink = ctx.pMain, key = Proc.mainIn)
              case _ =>
            }
        }
    }

    val cueVal = cueBg.value
    val numFramesTL = (cueVal.numFrames * TimeRef.SampleRate / cueVal.sampleRate).toLong
    val spanTL = Span(offCue, offCue + numFramesTL)
    val (_, pBg) = mkAudioRegion[S](tl = ctx.tl, time = spanTL, audioCue = cueBg, pMain = ctx.pMain,
      gOffset = 0L, gain = 1.0 /* 0.5 */) // XXX TODO where does the gain factor come from?
    replace.map.headOption.foreach {
      case (e, _) =>
        val pOld = e.value
        pOld.attr.iterator.foreach {
          case (key, value) if key === "color" => pBg.attr.put(key, value)
          case _ =>
        }
    }
  }

  def executePlacementAndMask[S <: Sys[S]](placement: ISeq[PlacedRegion], gain: Double)
                                          (implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    log("executePlacementAndMask")
    import SoundProcesses.executionContext
    val tlFg = Timeline[S]
    tlFg.name = s"foreground ${new java.util.Date}"
    if (!config.deleteTempFiles) ctx.folderTemp.addLast(tlFg)
    val pMain = addDefaultGlobalProc(tlFg)
    executePlacement[S](placement, gain = gain, target = tlFg, pMain = pMain)
    val tlFgSpans = groupWithinProximity(tlFg, maxDistanceSec = 1.0)
    val futAll: Vec[Future[Unit]] = tlFgSpans.map { case NamedSpan(tlFgSpan, nameFg) =>
      import ctx.cursor
      val spanFg    = widen(tlFgSpan, durSec = 0.5)
      val replace   = copyForReplacementBounce(ctx.tl, spanFg)
      import replace.{nameBg, tlBg}
      tlBg.name = s"background ${new java.util.Date}"
      if (!config.deleteTempFiles) ctx.folderTemp.addLast(tlBg)
      val futRender = tlBg.span.nonEmptyOption.fold(Future.successful(())) { spanBg =>
        val futBncFg  = bounceTemp[S](tlFg, spanFg )
        val futBncBg  = bounceTemp[S](tlBg, spanBg)
        val union     = spanFg union spanBg
        val offFg     = (((spanFg.start - union.start) * config.sampleRate) / TimeRef.SampleRate).toLong
        val offBg     = (((spanBg.start - union.start) * config.sampleRate) / TimeRef.SampleRate).toLong
        val numFrames = ((union.length * config.sampleRate) / TimeRef.SampleRate).toLong
        log(s"spanFg = $spanFg (${spanToTime(spanFg)}), spanBg = $spanBg (${spanToTime(spanBg)}), numFrames = $numFrames / ${framesToTime(union.length)}")
        val dirOut    = config.baseDir / "audio_work" / "rendered"
        dirOut.mkdirs()
        val nameOut   = s"${Util.shorten(s"$nameFg-mask-$nameBg")}.aif"

        val futMask   = futBncFg.flatMap { fInFg =>
          flatMapTx[S, File, CueSource[S]](futBncBg) { implicit tx => fInBg =>
            Mask.run[S](fInFg = fInFg, offFg = offFg, fInBg = fInBg, offBg = offBg,
              numFrames = numFrames, nameOut = nameOut)
          }
        }

        mapTx[S, CueSource[S], Unit](futMask) { implicit tx => cueBgH =>
          executeReplacement(replace, cueBgH(), offCue = union.start)
        }
      }
      futRender
    }

    val futOne: Future[Vec[Unit]] = Future.sequence(futAll)
    import ctx.cursor
    mapTx[S, Vec[Unit], Unit](futOne) { implicit tx => _ =>
      executePlacement[S](placement, gain = gain, target = ctx.tl, pMain = ctx.pMain)
    }
  }

  def executePlacement[S <: Sys[S]](placement: ISeq[PlacedRegion], gain: Double, target: Timeline.Modifiable[S],
                                    pMain: Proc[S])
                                   (implicit tx: S#Tx, ctx: Context[S], config: Config): Unit = {
    import ctx.{pMain => _, _}
    val vec       = placement.toVector
    val intelObj  = BooleanObj.newVar[S](intelligible)
    val colorVal  = mkColor[S](material)
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
      val mat: AudioCue.Obj[S] = material // help IntelliJ, WTF
      val (_, pr)  = mkAudioRegion[S](target, time = spanTL, audioCue = mat, pMain = pMain,
        gOffset = gOffset, fadeIn = fadeIn, fadeOut = fadeOut, gain = gainVal)
      val prAttr = pr.attr
      prAttr.put(attrIntel, intelObj)
      prAttr.put(ObjView.attrColor, /* Color.Obj.newVar( */ colorVal /* ) */)
    }
  }

    // puts a plain (non-transformed) text in full, retaining intelligibility
  def putPlainTextIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx._
    val placementOpt  = tryPlacePlainIntel[S](matSpan = matSpan)
    val gainVal       = (65.0 - loud95).dbAmp
    placementOpt.fold(Future.successful(())) { placement =>
      executePlacementAndMask(placement, gain = gainVal)
    }
  }

    // puts a plain (non-transformed) object with pauses, retaining intelligibility
  def tryPlacePlainIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S]): Option[List[PlacedRegion]] = {
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

    val pos0TLOpt = if (0.5.coin()) {
      findLeft(reg0, pos0TLTry) orElse findRight(reg0, pos0TLTry)
    } else {
      findRight(reg0, pos0TLTry) orElse findLeft(reg0, pos0TLTry)
    }
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

  def putPlainTextFaded[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    ???   // XXX TODO
  }

  def putPlainTextCut[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx._
    val startPos0     = math.min((matNumFrames - TimeRef.SampleRate).toLong, (rnd.nextDouble() * matNumFrames).toLong)
    val maxNumFrames  = math.min(matNumFrames - startPos0, (config.maxTextDur * TimeRef.SampleRate).toLong)
    val cutLen        = math.max(TimeRef.SampleRate, (rnd.nextDouble() * maxNumFrames).toLong).toLong
    val breakPauses   = pauses.filter(_.break)
    val i             = breakPauses.indexWhere(_.span.start >= startPos0)
    val startPos      = if (i < 0) 0L else breakPauses(i).span.stop
    val stopPos0      = startPos + cutLen
    val j             = breakPauses.indexWhere(_.span.start >= stopPos0, i + 1)
    val stopPos       = if (j < 0) matNumFrames - startPos else breakPauses(j).span.start
    val matSpan = Span(startPos, stopPos)
    putPlainTextIntel(matSpan)
  }

  def putPlainSound[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx._
    if (complete) {
      putPlainSoundFull[S]()
    } else {  // shortened
      putPlainSoundCut()
    }
  }

  def putPlainSoundFull[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    log("putPlainSoundFull")
    val matSpan = Span(0L, ctx.matNumFrames)
    log(s"matSpan = $matSpan / ${spanToTime(matSpan)}")
    putPlainSoundIntel(matSpan)
  }

  def putPlainSoundCut[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    log("putPlainSoundCut")
    import ctx._
    val startPos0     = (rnd.nextDouble() * matNumFrames).toLong
    val maxNumFrames  = math.min(matNumFrames - startPos0, (config.maxSoundDur * TimeRef.SampleRate).toLong)
    val cutLen        = math.max(TimeRef.SampleRate, (rnd.nextDouble() * maxNumFrames).toLong).toLong

    log(s"startPos0 = $startPos0 / ${framesToTime(startPos0)}, cutLen0 = $cutLen / ${framesToTime(cutLen)}")
    val breakPauses   = pauses.filter(_.break)
    if (breakPauses.nonEmpty && 0.5.coin()) {
      val i             = breakPauses.indexWhere(_.span.start >= startPos0)
      val startPos      = if (i < 0) 0L else breakPauses(i).span.stop
      val stopPos0      = startPos + cutLen
      val j             = breakPauses.indexWhere(_.span.start >= stopPos0, i + 1)
      val stopPos       = if (j <= i) math.min(startPos + cutLen, matNumFrames) else breakPauses(j).span.start
      val matSpan = Span(startPos, stopPos)
      log(s"use breakPauses; matSpan = $matSpan / ${spanToTime(matSpan)}")
      putPlainTextIntel(matSpan)
    } else {
      val gainVal0      = (65.0 - loud95).dbAmp
      val gainVal       = rnd.nextDouble().linLin(0.0, 1.0, -18.0, 0.0).dbAmp * gainVal0
      val startPos      = startPos0
      val stopPos       = math.min(matNumFrames, startPos + cutLen)
      val matSpan       = Span(startPos, stopPos)
      log(s"use random cut; matSpan = $matSpan / ${spanToTime(matSpan)}")
      val matDur        = matSpan.length / TimeRef.SampleRate
      val fadeInS       = rnd.nextDouble().linExp(0.0, 1.0, 1.0, matDur - 1.0)
      val fadeOutS      = math.min(matDur - fadeInS, rnd.nextDouble().linExp(0.0, 1.0, 1.0, matDur - 1.0))
      val fadeIn        = FadeSpec((fadeInS  * TimeRef.SampleRate).toLong, Curve.sine)
      val fadeOut       = FadeSpec((fadeOutS * TimeRef.SampleRate).toLong, Curve.sine)
      val posTL         = (rnd.nextDouble() * (tlNumFrames - matSpan.length)).toLong
      val region        = RegionAt(frame = startPos, span = matSpan, pause = None,
        fadeIn = fadeIn, fadeOut = fadeOut)
      val placed        = PlacedRegion(posTL = posTL, region = region)
      log(s"placed = $placed")
      val placement     = placed :: Nil

      executePlacementAndMask(placement, gain = gainVal)
    }
  }

  def putPlainSoundIntel[S <: Sys[S]](matSpan: Span)(implicit tx: S#Tx, ctx: Context[S], config: Config): Future[Unit] = {
    import ctx._
    val placementOpt  = tryPlacePlainIntel[S](matSpan = matSpan)
    log(s"placementOpt = $placementOpt")
//    val gainVal       = (65.0 - loud95).dbAmp // XXX TODO --- we should probably take loud50 into account as well
    val gainVal       = (65.0 - loud80).dbAmp
    placementOpt.fold(Future.successful(())) { placement =>
      executePlacementAndMask(placement, gain = gainVal)
    }
  }
}
