/*
 *  LayerUtil.scala
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
import de.sciss.file._
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, LongObj, SpanLikeObj}
import de.sciss.lucre.stm.Sys
import de.sciss.maeanderungen.Layer.{Context, RegionAt, attrIntel}
import de.sciss.maeanderungen.Ops._
import de.sciss.mellite.gui.edit.Edits
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.proc.{AudioCue, Code, Color, FadeSpec, ObjKeys, Proc, Timeline}
import de.sciss.synth.{Curve, SynthGraph, proc}
import de.sciss.synth.proc.Implicits._

object LayerUtil {
  def any2stringadd: Any = ()

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

  def mkAudioRegion[S <: Sys[S]](tl       : Timeline.Modifiable[S],
                                 time     : Span,
                                 audioCue : AudioCue.Obj[S],
                                 gOffset  : Long,
                                 fadeIn   : FadeSpec  = FadeSpec(0L),
                                 fadeOut  : FadeSpec  = FadeSpec(0L),
                                 gain     : Double    = 1.0)
                                (implicit tx: S#Tx, ctx: Context[S] /*, config: Config */): (SpanLikeObj[S], Proc[S]) = {
    val spanV   = time
    val span    = SpanLikeObj.newVar[S](spanV)
    val p       = Proc[S]
    val out     = p.outputs.add(Proc.mainOut)
    import proc.Ops._
    // XXX TODO -- cheese louise, the heuristics will get nasty
    val audioCueOff = if (gOffset == 0L) audioCue else {
      audioCue match {
        case AudioCue.Obj.Shift(peer, amt) =>
          AudioCue.Obj.Shift(peer , LongObj.newVar[S](amt + gOffset))
        case other =>
          AudioCue.Obj.Shift(other, LongObj.newVar[S](      gOffset))
      }
    }
    val prAttr = p.attr
    prAttr.put(Proc.graphAudio, audioCueOff)
    val numInChans = audioCue.numChannels
    require (numInChans == 1 || numInChans == 2)
    val pTape = if (numInChans == 1) ctx.pTape1 else ctx.pTape2
    val sourceOpt = pTape.attr.get(Proc.attrSource)
    sourceOpt.foreach(source => p.attr.put(Proc.attrSource, source))
    p.graph() = pTape.graph()

    if (fadeIn.numFrames > 0L) {
      val fd    = FadeSpec.Obj.newVar[S](fadeIn)
      prAttr.put(ObjKeys.attrFadeIn, fd)
    }
    if (fadeOut.numFrames > 0L) {
      val fd    = FadeSpec.Obj.newVar[S](fadeOut)
      prAttr.put(ObjKeys.attrFadeOut, fd)
    }
    if (gain !== 1.0) {
      val gainObj = DoubleObj.newVar[S](gain)
      prAttr.put(ObjKeys.attrGain, gainObj)
    }
    if (numInChans == 1 && ctx.pan != 0.0) {
      val panObj = DoubleObj.newVar[S](ctx.pan)
      prAttr.put("pan", panObj)
    }

    tl.add(span, p)
    import ctx.cursor
    Edits.addLink(source = out, sink = ctx.pMain, key = Proc.mainIn)

    (span, p)
  }


  def mkColor[S <: Sys[S]](cue: AudioCue.Obj[S])(implicit tx: S#Tx): Color.Obj[S] = {
    val hash  = cue.value.artifact.base.hashCode
    //    val c     = hash & 0xFFFFFF
    val c     = hash | 0xFF000000
    Color.Obj.newConst(Color.User(c))
  }

  def addDefaultGlobalProc[S <: Sys[S]](tl: Timeline.Modifiable[S])(implicit tx: S#Tx, config: Config): Proc[S] = {
    val p = Proc[S]
    import synth.proc.graph.Ops._
    import synth.proc.graph._
    import synth.ugen._

    val source = s"""val in      = ScanInFix(${config.numChannels})
                    |val gain    = "gain".kr(1f)
                    |val chGain  = "ch-gain".kr(Vector.fill(${config.numChannels})(1f))
                    |val mute    = "mute".kr(0f)
                    |val bus     = "bus" .kr(0f)
                    |val amp     = gain * chGain * (1 - mute)
                    |val mul     = in * amp
                    |val sig     = mul
                    |Out.ar(bus, sig)
                    |""".stripMargin

    p.graph() = SynthGraph {
      val in      = ScanInFix(config.numChannels)
      val gain    = "gain".kr(1f)
      val chGain  = "ch-gain".kr(Vector.fill(config.numChannels)(1f))
      val mute    = "mute".kr(0f)
      val bus     = "bus" .kr(0f)
      val amp     = gain * chGain * (1 - mute)
      val mul     = in * amp
      val sig     = mul
      Out.ar(bus, sig)
    }
    p.name = "main"
    p.attr.put(Proc.attrSource, Code.Obj.newVar(Code.SynthGraph(source)))

    tl.add(SpanLikeObj.newConst(Span.All), p)
    p
  }

  def mkTapeGraph[S <: Sys[S]](numInChannels: Int)(implicit tx: S#Tx, config: Config): Proc[S] = {
    val p = Proc[S]
    import synth.proc.graph.Ops._
    import synth.proc.graph._
    import synth.ugen.{VDiskIn => _, _}

    val sourcePan = if (numInChannels == 1)
      """{
        |  val pan = "pan".kr(0.0)
        |  Pan2.ar(disk, pan)
        |}"""
    else "disk"

    val source = s"""val disk  = VDiskIn.ar("sig")
                    |val gain  = "gain".kr(1.0)
                    |val mute  = "mute".kr(0.0)
                    |val env   = FadeInOut.ar
                    |val amp   = env * ((1 - mute) * gain)
                    |val sig   = $sourcePan
                    |val sigA  = sig * amp
                    |val ch1   = "ch-1".kr(0.0)
                    |val ch2   = "ch-2".kr(1.0)
                    |val sig1  = sigA.out(0)
                    |val sig2  = sigA.out(1)
                    |val out   = Vector.tabulate(${config.numChannels}) { ch =>
                    |  sig1 * (ch1 sig_== ch) + sig2 * (ch2 sig_== ch)
                    |}
                    |ScanOut(out)
                    |""".stripMargin

    p.graph() = SynthGraph {
      val disk  = VDiskIn.ar("sig")
      val gain  = "gain".kr(1.0)
      val mute  = "mute".kr(0.0)
      val env   = FadeInOut.ar
      val amp   = env * ((1 - mute) * gain)
      val sig  = if (numInChannels == 1) {
        val pan = "pan".kr(0.0)
        Pan2.ar(disk, pan)
      } else {
        require (numInChannels == 2)
        disk
      }
      val sigA  = sig * amp
      val ch1   = "ch-1".kr(0.0)
      val ch2   = "ch-2".kr(1.0)
      val sig1  = sigA.out(0)
      val sig2  = sigA.out(1)
      val out   = Vector.tabulate(config.numChannels) { ch =>
        sig1 * (ch1 sig_== ch) + sig2 * (ch2 sig_== ch)
      }
      ScanOut(out)
    }

    p.attr.put(Proc.attrSource, Code.Obj.newVar(Code.SynthGraph(source)))

    p
  }
}
