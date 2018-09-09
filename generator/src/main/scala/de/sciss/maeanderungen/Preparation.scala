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
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, SpanLikeObj}
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.maeanderungen.Builder._
import de.sciss.span.Span
import de.sciss.synth.proc.Action.attrSource
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{AudioCue, Color, GenContext, TimeRef, Timeline, Workspace}
import de.sciss.synth.{io, proc}

object Preparation {
  val DEFAULT_VERSION = 12

  def process[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val r             = workspace.root
    val fAna          = mkFolder(r, "analysis")
    mkObj[S, proc.Action](fAna, "find-pauses", DEFAULT_VERSION)(mkActionFindPauses[S]())
    mkObj[S, proc.Action](fAna, "remove-meta", DEFAULT_VERSION)(mkActionRemoveMeta[S]())
  }

  def mkFScapeFindPauses[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      val threshLoud    = 15.0
      val in0           = AudioFileIn("in")
      val in            = Mix.MonoEqP(in0)
      val sampleRate    = in0.sampleRate
      val inFrames      = in0.numFrames

      // loudness
      val winLoud       = (0.2 * sampleRate).floor
      val stepLoud      = (winLoud/4).floor
      val framesLoud    = ((inFrames + stepLoud - 1) / stepLoud).floor
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
      val isMale        = "is-male".attr(0) // determine pitch tracking register

      val (pitch, srPitch, framesPitch) = {
        val minPitch            = 100.0 - (isMale *  40.0) // if (isMale)  60.0 else 100.0 // 100.0
        val maxPitch            = 320.0 - (isMale * 120.0) // if (isMale) 200.0 else 320.0 // 1000.0
        val voicingThresh       = 0.45
        val silenceThresh       = 0.03
        val octaveCost          = 0.01
        val octaveJumpCost      = 0.35
        val voicedUnvoicedCost  = 0.14
        val numTracks           = 15

        val _pch = PitchAC(in, sampleRate = sampleRate, pitchMin = minPitch, pitchMax = maxPitch,
          voicingThresh = voicingThresh, silenceThresh = silenceThresh, octaveCost = octaveCost,
          octaveJumpCost = octaveJumpCost, voicedUnvoicedCost = voicedUnvoicedCost,
          numCandidates = numTracks)

        val stepPitch   = _pch.stepSize
        val _frames     = ((inFrames + stepPitch - 1) / stepPitch).floor

        val _sr = sampleRate / stepPitch
        (_pch, _sr, _frames)
      }

      // write
      val writtenLoud   = AudioFileOut("loud"   , loud , sampleRate = srLoud  )
      val writtenPitch  = AudioFileOut("pitch"  , pitch, sampleRate = srPitch )
      /* val writtenSpans  = */ AudioFileOut("pauses" , spans)
      // val writtenAll    = writtenLoud ++ writtenPitch ++ writtenSpans

      Progress(writtenLoud  / framesLoud  , Metro(srLoud  ), "loudness")
      Progress(writtenPitch / framesPitch , Metro(srPitch ), "pitch"   )

      // Action(Done(writtenAll), "done")
    }
    f
  }

  def mkActionFindPauses[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
    import proc.Action
    val act0 = proc.Action.apply[S] { universe =>
      import de.sciss.fscape.lucre.FScape
      import universe._
      //----crop
      println("----FIND PAUSES----")

      val fMat = root.![Folder]("material")

      val nextOpt = fMat.iterator.collectFirst {
        case a: AudioCue.Obj[S] if a.attr.$[Timeline  ]("pauses"  ).isEmpty ||
                                   a.attr.$[DoubleObj ]("loud-50" ).isEmpty => a
      }
      nextOpt.fold[Unit] {
        println("No more materials to analyze.")
      } { cue =>
        val cueName   = cue.value.artifact.base
        println(s"Preparing $cueName...")
        val hasPauses = cue.attr.$[Timeline]("pauses").isDefined
        val fsc       = self.attr.![FScape]("fsc")
        val loc       = root.![ArtifactLocation]("base")
        val aFsc      = fsc.attr
        aFsc.put("in", cue)
        val dirAna    = loc.directory / "analysis"
        dirAna.mkdirs()
        val isMale    = cueName.contains("_HH")
        val fLoud     = dirAna / s"$cueName-loud.aif"
        val fPitch    = dirAna / s"$cueName-pitch.aif"
        val fPauses   = dirAna / s"$cueName-pauses.aif"
        val artLoud   = Artifact(loc, fLoud   )
        val artPitch  = Artifact(loc, fPitch  )
        val artPauses = Artifact(loc, fPauses )
        aFsc.put("loud"   , artLoud   )
        aFsc.put("pitch"  , artPitch  )
        aFsc.put("pauses" , artPauses )
        aFsc.put("is-male", BooleanObj.newConst(isMale))

        def done()(implicit tx: S#Tx): Unit = {
          val actDone   = self.attr.![Action]("done")
          val uDone     = Action.Universe(actDone, workspace, invoker = Some(fsc))
          actDone.execute(uDone)
        }

        if (!hasPauses) {
          implicit val gen: GenContext[S] = GenContext[S]
          val r = fsc.run()
          r.reactNow { implicit tx => state =>
            if (state.isComplete) {
              r.result.get.get
              done()
            }
          }

        } else {
          done()
        }
      }
    }

    val act     = wrapAction(act0)
    val fsc     = mkObjIn[S, FScape     ](act, "fsc" , DEFAULT_VERSION)(mkFScapeFindPauses    [S]())
    val actDone = mkObjIn[S, proc.Action](act, "done", DEFAULT_VERSION)(mkActionFindPausesDone[S]())
    fsc.attr.put("done", actDone)
    act
  }

  def mkActionRemoveMeta[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
    val act0 = proc.Action.apply[S] { universe =>
      import universe._
      //----crop
      println("----REMOVE META----")

      val fMat = root.![Folder]("material")

      fMat.iterator.foreach {
        case a: AudioCue.Obj[S] if a.attr.$[Timeline]("pauses").nonEmpty =>
          println(s"Removing meta data from ${a.name}")
          a.attr.remove("pauses")
        case _ =>
      }
    }

    val act = wrapAction(act0)
    act
  }

  private def wrapAction[S <: Sys[S]](value: proc.Action[S])(implicit tx: S#Tx): proc.Action[S] =
    value match {
      case proc.Action.Var(vr) => vr
      case _ =>
      val act = proc.Action.Var(value)
      value.attr.get(attrSource).foreach { src =>
        act.attr.put(attrSource, src)
      }
      act
    }

  def mkActionFindPausesDone[S <: Sys[S]]()(implicit tx: S#Tx): proc.Action[S] = {
    import proc.Action
    val act0 = proc.Action.apply[S] { universe =>
      import de.sciss.fscape.lucre.FScape
      import universe._
      //----crop
      println("----ACTION DONE")

      val shouldIter: Boolean = try {
        val Some(fsc: FScape[S]) = invoker  // self.attr.![FScape]("fsc")
        val cue       = fsc.attr.![AudioCue.Obj]("in")
        val ca        = cue.attr
        val srIn      = cue.value.sampleRate
        val srRatio   = TimeRef.SampleRate / srIn

        // ---- loudness ----

        val cueLoud = ca.$[AudioCue.Obj]("loud").getOrElse {
          val artLoud   = fsc.attr.![Artifact]("loud")
          val specLoud  = io.AudioFile.readSpec(artLoud .value)
          val _cueLoud  = AudioCue.Obj(artLoud, specLoud, 0L, 1.0)
          ca.put("loud", _cueLoud)
          _cueLoud
        }

        if (!ca.contains("pitch") || !ca.contains("pauses")) {

          // ---- pitch ----

          val artPitch  = fsc.attr.![Artifact]("pitch")
          val specPitch = io.AudioFile.readSpec(artPitch.value)
          val cuePitch  = AudioCue.Obj(artPitch , specPitch , 0L, 1.0)
          ca.put("pitch", cuePitch )

          val afPitch = io.AudioFile.openRead(artPitch.value)
          val pitches = try {
            val numPitches = afPitch.numFrames.toInt
            println(s"  numPitches $numPitches")
            val buf = afPitch.buffer(numPitches)
            afPitch.read(buf)
            buf(0)
          } finally {
            afPitch.close()
          }

          // ---- pauses ----

          val artPauses = fsc.attr.![Artifact]("pauses")

          val afPauses = io.AudioFile.openRead(artPauses.value)
          val spansPauses = try {
            val numSpans = afPauses.numFrames.toInt
            val buf = afPauses.buffer(numSpans)
            afPauses.read(buf)
            buf(0).grouped(2).collect {
              case Array(startF, stopF) =>
                val start = (startF * srRatio).toLong
                val stop  = (stopF  * srRatio).toLong
                Span(start, stop)
            } .toList
          } finally {
            afPauses.close()
          }

          val srRatioPch  = specPitch.sampleRate / TimeRef.SampleRate
          val pitchDirNum = (specPitch.sampleRate * 0.7).toInt

          def isPitchDown(frame: Long): Boolean = {
            val stopI: Int = math.min(pitches.length, (frame * srRatioPch).toInt)
            var stop0 = stopI
            var voiced = false
            while (stop0 > 0 && !voiced) {
              stop0 -= 1
              val v = pitches(stop0)
              voiced = v > 0
            }
            val stop  = stop0 + 1
            val start = stop - pitchDirNum
            // println(s"isPitchDown($frame) -> start = $start, stop = $stop")
            println(s"  isPitchDown($frame) -> start = $start, stop = $stop ($stopI)")
            (start >= 0) && {
              val h = (stop - start) / 2

              def avg(i: Int, j: Int): Double = {
                var k = i
                var acc = 0.0
                while (k < j) {
                  val v = pitches(k)
                  if (v > 0) {
                    acc = if (acc == 0.0) v else acc * 0.95 + v * 0.05
                  }
                  k += 1
                }
                if (acc > 0) acc else 1.0
              }

              val init = avg(start    , start + h)
              val tail = avg(start + h, stop     )
              println(s"  ... dir ${tail / init}")
              tail < init
            }
          }

          val colrBreak = Color.Obj.newVar[S](Color.Palette(5))

          val tlPauses = ca.$[Timeline]("pauses").getOrElse {
            val tl = Timeline[S]
            tl.name = "pauses"
            cue.attr.put("pauses", tl)
            tl
          }
          val tlPausesM = tlPauses.modifiableOption.get
          spansPauses.foreach { span =>
            val v = IntObj.newVar[S](0)
            v.name = "pause"
            val va = v.attr
            val dn = isPitchDown(span.start)
            va.put("track-height", IntObj.newVar(2))
            va.put("break", BooleanObj.newVar(dn))
            if (dn) va.put("color", colrBreak)
            tlPausesM.add(SpanLikeObj.newVar(span), v)
          }

        } // if not contains pitch or pauses

        // ---- loudness percentiles ----

        if (!ca.contains("loud-50") || !ca.contains("loud-95")) {
          val vLoud = cueLoud.value
          val afLoud = io.AudioFile.openRead(vLoud.artifact)
          val (median, perc80, perc95) = try {
            val num = afLoud.numFrames.toInt
            val buf = afLoud.buffer(num)
            afLoud.read(buf)
            val sorted = buf(0).sorted.dropWhile(_ < 10)

            def percentile(n: Int) =
              sorted((sorted.length * n - 50) / 100).toDouble

            (percentile(50), percentile(80), percentile(95))

          } finally {
            afLoud.close()
          }

          ca.put("loud-50", DoubleObj.newVar(median))
          ca.put("loud-80", DoubleObj.newVar(perc80))
          ca.put("loud-95", DoubleObj.newVar(perc95))
        }

        // ----
        val name = cue.value.artifact.name
        println(s"Analysis done for $name")
        !name.contains("Trns")

      } catch {
        case scala.util.control.NonFatal(ex) =>
          ex.printStackTrace()
          throw ex
      }

      if (shouldIter) {
        val actIter = root.![Folder]("analysis").![Action]("find-pauses")
        val uIter   = Action.Universe(actIter, workspace)
        actIter.execute(uIter)
      }
    }

    val act = wrapAction(act0)
    act
  }
}
