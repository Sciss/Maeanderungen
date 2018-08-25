/*
 *  Preparation.scala
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

import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.lucre.MacroImplicits.FScapeMacroOps
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{IntObj, SpanLikeObj}
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.maeanderungen.Builder._
import de.sciss.span.Span
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{AudioCue, GenContext, TimeRef, Timeline, Workspace}
import de.sciss.synth.{io, proc}

object Preparation {
  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val r             = workspace.root
    val fAna          = mkFolder(r, "analysis")
    mkObj[S, proc.Action](fAna, "find-pauses", 1)(mkActionFindPauses[S]())
  }

  def mkFScapeFindPauses[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      val threshLoud    = 15.0
      val in            = AudioFileIn("in")
      val sampleRate    = in.sampleRate

      // loudness
      val winLoud       = (0.2 * sampleRate).floor
      val stepLoud      = (winLoud/4).floor
      val framesLoud    = ((in.numFrames + stepLoud - 1) / stepLoud).floor
      val slidLoud      = Sliding(in, size = winLoud, step = stepLoud)
      val loud          = Loudness(slidLoud, sampleRate = sampleRate, size = winLoud, spl = 70, diffuse = 1)

      // pauses
      val medianLoud    = SlidingPercentile(loud, len = 5)
      val foreground    = medianLoud > threshLoud
      val fgDif         = Differentiate(foreground)
      val toBack        = fgDif sig_== -1
      val toFront       = fgDif sig_== +1
      val offLoud       = Frames(fgDif)
      val pauseStart    = FilterSeq(offLoud, toBack) // .dropRight(1) -- not yet implemented
      val pauseStop     = FilterSeq(offLoud, toFront  ).drop(1)
      val spans         = ((pauseStart zip pauseStop) - 1) * stepLoud
      val srLoud        = sampleRate / stepLoud

      // pitch
      val minPitch      = 50  // Hz
      val maxPitch      = 200 // Hz

      // XXX TODO
      val pitch   = loud
      val srPitch = srLoud

      // write
      val writtenLoud   = AudioFileOut("loud"   , loud , sampleRate = srLoud  )
      val writtenPitch  = AudioFileOut("pitch"  , pitch, sampleRate = srPitch )
      val writtenSpans  = AudioFileOut("pauses" , spans)

      Progress(writtenLoud / framesLoud, Metro(srLoud))

      Action(Done(writtenSpans), "done")
    }
    f
  }

  def mkActionFindPauses[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
    val act = proc.Action.apply[S] { universe =>
      import de.sciss.fscape.lucre.FScape
      //----crop
      import universe._
      println("----ACTION ALL")

      val fMat = root.![Folder]("material")

      val nextOpt = fMat.iterator.collectFirst {
        case a: AudioCue.Obj[S] if a.attr.$[Timeline]("pauses").isEmpty => a
      }
      nextOpt.fold[Unit] {
        println("No more materials to analyze.")
      } { cue =>
        val fsc       = self.attr.![FScape]("fsc")
        val loc       = root.![ArtifactLocation]("base")
        val aFsc      = fsc.attr
        aFsc.put("in", cue)
        val cueName   = cue.value.artifact.base
        val dirAna    = loc.directory / "analysis"
        dirAna.mkdirs()
        val fLoud     = dirAna / s"$cueName-loud.aif"
        val fPitch    = dirAna / s"$cueName-pitch.aif"
        val fPauses   = dirAna / s"$cueName-pauses.aif"
        val artLoud   = Artifact(loc, fLoud   )
        val artPitch  = Artifact(loc, fPitch  )
        val artPauses = Artifact(loc, fPauses )
        aFsc.put("loud"   , artLoud   )
        aFsc.put("pitch"  , artPitch  )
        aFsc.put("pauses" , artPauses )
        implicit val gen: GenContext[S] = GenContext[S]
        fsc.run()
      }
    }

    val fsc     = mkObjIn[S, FScape     ](act, "fsc" , 1)(mkFScapeFindPauses[S]())
    val actDone = mkObjIn[S, proc.Action](act, "done", 1)(mkActionFindPausesDone[S]())
    fsc.attr.put("done", actDone)
    act
  }

  def mkActionFindPausesDone[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
    import proc.Action
    val act = proc.Action.apply[S] { universe =>
      import de.sciss.fscape.lucre.FScape
      //----crop
      import universe._
      println("----ACTION DONE")

      try {
        val Some(fsc: FScape[S]) = invoker  // self.attr.![FScape]("fsc")
        val cue       = fsc.attr.![AudioCue.Obj]("in")
        val srIn      = cue.value.sampleRate
        val srRatio   = TimeRef.SampleRate / srIn
        // ---- pauses ----
        val artPauses = fsc.attr.![Artifact]("pauses")
        val af = io.AudioFile.openRead(artPauses.value)
        val spans = try {
          val numSpans = af.numFrames.toInt
          val buf = af.buffer(numSpans)
          af.read(buf)
          buf(0).grouped(2).collect {
            case Array(startF, stopF) =>
              val start = (startF * srRatio).toLong
              val stop  = (stopF  * srRatio).toLong
              Span(start, stop)
          } .toList
        } finally {
          af.close()
        }
        val ca = cue.attr
        val tl = ca.$[Timeline]("pauses").getOrElse {
          val tl = Timeline[S]
          tl.name = "pauses"
          cue.attr.put("pauses", tl)
          tl
        }
        val tlm = tl.modifiableOption.get
        spans.foreach { span =>
          val v = IntObj.newVar[S](0)
          v.name = "pause"
          val va = v.attr
          va.put("track-height", IntObj.newVar(2))
          tlm.add(SpanLikeObj.newVar(span), v)
        }
        // ---- loud, pitch ----
        val artLoud   = fsc.attr.![Artifact]("loud")
        val artPitch  = fsc.attr.![Artifact]("pitch")
        val specLoud  = io.AudioFile.readSpec(artLoud .value)
        val specPitch = io.AudioFile.readSpec(artPitch.value)
        val cueLoud   = AudioCue.Obj(artLoud  , specLoud  , 0L, 1.0)
        val cuePitch  = AudioCue.Obj(artPitch , specPitch , 0L, 1.0)
        ca.put("loud" , cueLoud  )
        ca.put("pitch", cuePitch )

        // ----
        println(s"Analysis done for ${cue.value.artifact.name}")

      } catch {
        case scala.util.control.NonFatal(ex) =>
          ex.printStackTrace()
          throw ex
      }

      val actIter = root.![Folder]("analysis").![Action]("find-pauses")
      val uIter   = Action.Universe(actIter, workspace)
      actIter.execute(uIter)
    }
    act
  }
}
