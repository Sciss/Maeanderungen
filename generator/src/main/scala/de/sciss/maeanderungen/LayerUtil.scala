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
import de.sciss.fscape.{Graph, stream}
import de.sciss.kollflitz.Vec
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, LongObj, SpanLikeObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.maeanderungen.Layer.{Context, RegionAt, attrIntel}
import de.sciss.maeanderungen.Ops._
import de.sciss.mellite.ProcActions
import de.sciss.mellite.gui.ActionBounceTimeline
import de.sciss.mellite.gui.edit.Edits
import de.sciss.mellite.util.Gain
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.io.{AudioFile, SampleFormat}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, Code, Color, FadeSpec, ObjKeys, Proc, SoundProcesses, TimeRef, Timeline}
import de.sciss.synth.{SynthGraph, proc}

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

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
                                 pMain    : Proc[S],
                                 gOffset  : Long,
                                 fadeIn   : FadeSpec  = FadeSpec(0L),
                                 fadeOut  : FadeSpec  = FadeSpec(0L),
                                 gain     : Double    = 1.0)
                                (implicit tx: S#Tx, ctx: Context[S], config: Config): (SpanLikeObj[S], Proc[S]) = {
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
    require (numInChans == 1 || numInChans == 2 || numInChans == config.numChannels)
    val pTape = if (numInChans == 1) ctx.pTape1 else if (numInChans == 2) ctx.pTape2 else ctx.pTapeAll
    val sourceOpt = pTape.attr.get(Proc.attrSource)
    sourceOpt.foreach(source => p.attr.put(Proc.attrSource, source))
    p.graph() = pTape.graph()
    p.name    = audioCue.value.artifact.base

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
    Edits.addLink(source = out, sink = /* ctx. */ pMain, key = Proc.mainIn)

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

  case class Replacement[S <: Sys[S]](nameBg: String, tlBg: Timeline.Modifiable[S],
                                      map: Map[BiGroup.Entry[S, Obj[S]], List[(SpanLikeObj[S], Obj[S])]])

  def copyForReplacementBounce[S <: Sys[S]](tl: Timeline[S], span: Span)
                                           (implicit tx: S#Tx, cursor: stm.Cursor[S]): Replacement[S] = {
    val res         = Timeline[S]
    val global      = tl.globalObjects.collect { case p: Proc[S] => p } .toList
    var globalMap   = Map.empty[Proc[S], Proc[S]]
    var name        = "unknown"   // we collect the name of the longest region, representing the background
    var nameSpan    = Span(0, 0)
    var replaceMap  = Map.empty[BiGroup.Entry[S, Obj[S]], List[(SpanLikeObj[S], Obj[S])]]

    tl.intersect(span).foreach {
      case (eSpan @ Span(start, stop), entries) =>
        entries.foreach { entry =>
          var sect      = eSpan.intersect(span).nonEmptyOption.get
          val obj       = entry.value
          val fdIn      = obj.attr.$[FadeSpec.Obj](ObjKeys.attrFadeIn ).fold(0L)(_.value.numFrames)
          val fdOut     = obj.attr.$[FadeSpec.Obj](ObjKeys.attrFadeOut).fold(0L)(_.value.numFrames)
          val fdInSpan  = Span(start, start + fdIn)
          val fdOutSpan = Span(stop - fdOut, stop)
          val fdInSect  = span.intersect(fdInSpan).length
          if (fdInSect > 0 && fdInSect < fdIn) {
            sect = sect.union(fdInSpan)
          }
          val fdOutSect = span.intersect(fdOutSpan).length
          if (fdOutSect > 0 && fdOutSect < fdOut) {
            sect = sect.union(fdOutSpan)
          }

          val (spanOut, objOut) = if (sect == eSpan) {  // copy entire object
            val objC  = ProcActions.copy(obj)
            val spanC = SpanLikeObj.newVar[S](eSpan)
            replaceMap += entry -> Nil
            (spanC, objC)

          } else {  // split
            var objC  = obj
            var spanC = entry.span

            if (sect.start > eSpan.start) {
              val (leftSpan, leftObj, rightSpan, rightObj) = splitCopyObject(sect.start, spanC, objC)
              val rpl0 = replaceMap.getOrElse(entry, Nil)
              val rpl1 = (leftSpan, leftObj) :: rpl0
              replaceMap += entry -> rpl1
              spanC = rightSpan
              objC  = rightObj
            }
            if (sect.stop < eSpan.stop) {
              val (leftSpan, leftObj, rightSpan, rightObj) = splitCopyObject(sect.stop, spanC, objC)
              val rpl0 = replaceMap.getOrElse(entry, Nil)
              val rpl1 = (rightSpan, rightObj) :: rpl0
              replaceMap += entry -> rpl1
              spanC = leftSpan
              objC = leftObj
            }
            assert(objC != obj)

            (spanC, objC)
          }

          res.add(spanOut, objOut)
          spanOut.value match {
            case spanVal: Span if spanVal.length > nameSpan.length =>
              name      = objOut.name
              nameSpan  = spanVal
            case _ =>
          }

          (obj, objOut) match {
            case (objP: Proc[S], objOutP: Proc[S]) =>
              global.foreach { objIn =>
                Edits.findLink(out = objP, in = objIn).foreach { lnk =>
                  val out = objOutP.outputs.add(lnk.source.key)
                  val glob = globalMap.getOrElse(lnk.sink, {
                    val cpy = ProcActions.copy(lnk.sink).asInstanceOf[Proc[S]]
                    globalMap += lnk.sink -> cpy
                    res.add(Span.All, cpy)
                    cpy
                  })
                  Edits.addLink(out, glob, lnk.key)
                }
              }

            case _ =>
          }
        }

      case _ =>    // global proc
    }

    Replacement(name, res, replaceMap)
  }

  def splitCopyObject[S <: Sys[S]](time: Long, oldSpan: SpanLikeObj[S], obj: Obj[S])
                                  (implicit tx: S#Tx): (SpanLikeObj.Var[S], Obj[S], SpanLikeObj.Var[S], Obj[S]) = {
    val leftObj   = ProcActions.copy[S](obj, connectInput = true)
    val rightObj  = ProcActions.copy[S](obj, connectInput = true)
    leftObj .attr.remove(ObjKeys.attrFadeOut)
    rightObj.attr.remove(ObjKeys.attrFadeIn )

    val oldVal    = oldSpan.value
    val rightSpan = oldVal match {
      case Span.HasStart(leftStart) =>
        val _rightSpan  = SpanLikeObj.newVar(oldVal)
        val resize      = ProcActions.Resize(time - leftStart, 0L)
        val minStart    = 0L // timelineModel.bounds.start
        ProcActions.resize(_rightSpan, rightObj, resize, minStart = minStart)
        _rightSpan

      case Span.HasStop(rightStop) =>
        SpanLikeObj.newVar[S](Span(time, rightStop))
    }

    val leftSpan = oldVal match {
      case Span.HasStop(rightStop) =>
        val _leftSpan = SpanLikeObj.newVar(oldVal)
        val minStart  = 0L // timelineModel.bounds.start
        val resize    = ProcActions.Resize(0L, time - rightStop)
        ProcActions.resize(_leftSpan, leftObj, resize, minStart = minStart)
        _leftSpan

      case Span.HasStart(leftStart) =>
        SpanLikeObj.newVar[S](Span(leftStart, time))
    }

    (leftSpan, leftObj, rightSpan, rightObj)
  }

  def widen(span: Span, durSec: Double): Span = {
    val frames = (durSec * TimeRef.SampleRate).toLong
    Span(span.start - frames, span.stop + frames)
  }

  def tryCompleteWith[A](p: Promise[A])(body: => Future[A]): Unit =
    try {
      val fut = body
      p.tryCompleteWith(fut)
    } catch {
      case NonFatal(ex) =>
        p.tryFailure(ex)
    }

  def bounceTemp[S <: Sys[S]](tl: Timeline[S], span: Span)
                             (implicit tx: S#Tx, ctx: Context[S], config: Config): Future[File] = {
    val p = Promise[File]
    val tlH                 = tx.newHandle(tl)
    val sCfg                = Server.Config()
    sCfg.outputBusChannels  = config.numChannels
    sCfg.inputBusChannels   = 0
    sCfg.sampleRate         = config.sampleRate
    tx.afterCommit {
      import ctx.cursor
      val dirTmp          = config.baseDir / "audio_work" / "temp"
      dirTmp.mkdirs()
      val fTmp            = File.createTempIn(dirTmp, suffix = ".aif", deleteOnExit = config.deleteTempFiles)
      sCfg.nrtOutputPath  = fTmp.path
      val settings        = ActionBounceTimeline.PerformSettings[S](
        gain        = Gain.immediate(0f),
        realtime    = false,
        fileFormat  = ActionBounceTimeline.FileFormat.PCM(sampleFormat = SampleFormat.Float),
        group       = tlH :: Nil,
        server      = sCfg,
        span        = span,
        channels    = Vec(0 to (config.numChannels - 1))
      )
      tryCompleteWith(p)(ActionBounceTimeline.perform(ctx.workspace, settings))
    }
    p.future
  }

  type CueSource[S <: Sys[S]] = stm.Source[S#Tx, AudioCue.Obj[S]]
  type FutCue   [S <: Sys[S]] = Future[CueSource[S]]

  def mkTransform[S <: Sys[S]](nameOut: String)(run: File => Future[Unit])
                              (implicit tx: S#Tx, ctx: Context[S]): FutCue[S] = {
    mkTransformOrRender[S](folderName = "transform", nameOut = nameOut, unique = false)(run)
  }

  def mkRender[S <: Sys[S]](nameOut: String)(run: File => Future[Unit])
                           (implicit tx: S#Tx, ctx: Context[S]): FutCue[S] = {
    mkTransformOrRender[S](folderName = "render", nameOut = nameOut, unique = true)(run)
  }

  private def mkTransformOrRender[S <: Sys[S]](folderName: String, nameOut: String, unique: Boolean)
                                              (run: File => Future[Unit])
                                              (implicit tx: S#Tx, ctx: Context[S]): FutCue[S] = {
    import SoundProcesses.executionContext
    import ctx.{cursor, workspace}
    val root      = workspace.root
    val folderOut = Builder.mkFolder(root, folderName)
    val loc       = root.![ArtifactLocation]("base")
    val dirOut    = loc.directory / "audio_work" / folderName
    dirOut.mkdirs()
    val fOut0     = dirOut / nameOut
    val fOut      = if (unique) Util.mkUnique(fOut0) else fOut0
    val artOut    = Artifact(loc, fOut)

    def done()(implicit tx: S#Tx): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
      val spec  = AudioFile.readSpec(fOut)
      val cue   = AudioCue.Obj(artOut, spec, offset = 0L, gain = 1.0)
      cue.name  = nameOut
      folderOut.addLast(cue)
      val cueH  = tx.newHandle(cue)
      val fut   = Builder.createMetaData[S](cue)
      fut.map(_ => cueH)
    }

    if (fOut.isFile) {
      done()

    } else {
      val fut = run(fOut)
      Builder.flatMapTx[S, Unit, CueSource[S]](fut) { implicit tx => _ =>
        done()
      }
    }
  }

  def renderFsc[S <: Sys[S]](g: Graph)(implicit tx: S#Tx): Future[Unit] = {
    val res = Promise[Unit]()

    tx.afterCommit {
      val config = stream.Control.Config()
      config.useAsync = false
      var lastProg = 0
      println("_" * 100)
      config.progressReporter = { rep =>
        val prog = (rep.total * 100).toInt
        while (lastProg < prog) {
          print('#')
          lastProg += 1
        }
      }

      implicit val ctrl: stream.Control = stream.Control(config)
//      Swing.onEDT {
//        SimpleGUI(ctrl)
//      }

      ctrl.status.foreach(_ => println())(config.executionContext)

      try {
        ctrl.run(g)
        res.tryCompleteWith(ctrl.status)
      } catch {
        case NonFatal(ex) =>
          res.failure(ex)
          // throw ex
      }
    }

    res.future
  }
}
