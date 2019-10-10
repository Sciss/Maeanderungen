/*
 *  TextTransforms.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.graph._
import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Random, Source, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.maeanderungen.Layer.Context
import de.sciss.maeanderungen.Ops._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.AudioCue

import scala.concurrent.Future

object TextTransforms {
  val weighted: Vec[(Double, Transform)] = Util.normalizeWeights(Vec(
    0.8 -> Transform.RemovePitch,
    0.2 -> Transform.RemoveLoud,
    0.1 -> SoundTransforms.Transform.Bleach,
    0.1 -> SoundTransforms.Transform.Filter,
  ))

  def choose[Tx <: TxnLike]()(implicit tx: Tx, r: Random[Tx]): Transform = {
    import Ops._
    weighted.chooseWeighted(_._1)._2
  }

  object Transform {
    case object RemovePitch extends Transform {
      def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[Source[S#Tx, AudioCue.Obj[S]]] = {
        import ctx.rnd
        val inverse = 0.2.coin()
        mkRemovePitch[S](inverse = inverse)
      }
    }

    case object RemoveLoud extends Transform {
      def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[Source[S#Tx, AudioCue.Obj[S]]] = {
        import ctx.rnd
        val inverse = 0.15.coin()
        val thresh  = if (0.5.coin()) ctx.loud80 else ctx.loud95
        mkRemoveLoud[S](inverse = inverse, thresh = thresh)
      }
    }
  }

  def mkRemovePitch[S <: Sys[S]](inverse: Boolean)
                                (implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val inverseS  = if (inverse) "Only" else "No"
    val nameOut   = s"$base-Trns-$inverseS-Pitch.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runRemovePitch(fOut = fOut, inverse = inverse)
    }
  }

  def mkRemoveLoud[S <: Sys[S]](inverse: Boolean, thresh: Double)
                                (implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val inverseS  = if (inverse) "Only" else "No"
    val nameOut   = s"$base-Trns-$inverseS-Loud-${thresh.toInt}.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runRemoveLoud(fOut = fOut, inverse = inverse, thresh = thresh)
    }
  }

  private def runRemovePitch[S <: Sys[S]](fOut: File, inverse: Boolean)
                                         (implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal              = ctx.material.value
    val fIn                 = matVal.artifact
    val specIn              = AudioFile.readSpec(fIn)
    import specIn.sampleRate

    val MinimumPitch        = 60.0
    val MaximumPitch        = 320.0
    val VoicingThreshold    = 0.6
    val SilenceThreshold    = 0.03
    val OctaveCost          = 0.01
    val OctaveJumpCost      = 0.35
    val VoicedUnvoicedCost  = 0.14
    val NumCandidates       = 15

    val g = Graph {
      def mkIn() = AudioFileIn(file = fIn, numChannels = 1)

      val in = mkIn()
      val pch = PitchAC(in, sampleRate = sampleRate, pitchMin = MinimumPitch, pitchMax = MaximumPitch,
        voicingThresh = VoicingThreshold, silenceThresh = SilenceThreshold, octaveCost = OctaveCost,
        octaveJumpCost = OctaveJumpCost, voicedUnvoicedCost = VoicedUnvoicedCost,
        numCandidates = NumCandidates)
      import pch.stepSize

      val cond          = if (inverse) pch sig_== 0 else pch > 0
      val hpDif         = Differentiate(cond)
      val trigStart     = hpDif sig_== -1
      val trigStop      = hpDif sig_== +1
      val off           = Frames(hpDif) * stepSize
      val spanStart0    = FilterSeq(off, trigStart)
      val spanStop0     = FilterSeq(off, trigStop )
      val inverseOrder  = spanStart0.take(1) > spanStop0.take(1)
      val spanStart     = spanStart0
      val spanStop      = spanStop0.drop(inverseOrder)

      val spans         = spanStart zip spanStop
      val spanLengths   = spanStop - spanStart

      val slices        = Slices(mkIn(), BufferDisk(spans))
      val win           = GenWindow(spanLengths, GenWindow.Hann).pow(1.0/12)
      val windowed      = slices * win // BufferDisk(win)
      val lap           = OverlapAdd(windowed, size = spanLengths, step = spanLengths * 0.9)
      val sigOut        = lap

      AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
    }

    LayerUtil.renderFsc[S](g)
  }

  private def runRemoveLoud[S <: Sys[S]](fOut: File, inverse: Boolean, thresh: Double)
                                         (implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal              = ctx.material.value
    val fIn                 = matVal.artifact
    val specIn              = AudioFile.readSpec(fIn)
    import specIn.sampleRate // , numFrames => inFrames

    val g = Graph {
      def mkIn() = AudioFileIn(file = fIn, numChannels = 1)

      val in = mkIn()

      val winLoud       = (0.2 * sampleRate).floor
      val stepLoud      = (winLoud/4).floor
//      val framesLoud    = ((inFrames + stepLoud - 1) / stepLoud).floor
      val slidLoud      = Sliding(in, size = winLoud, step = stepLoud)
      val loud          = Loudness(slidLoud, sampleRate = sampleRate, size = winLoud, spl = 70, diffuse = 1)

      val cond          = if (inverse) loud <= thresh else loud > thresh
      val hpDif         = Differentiate(cond)
      val trigStart     = hpDif sig_== -1
      val trigStop      = hpDif sig_== +1
      val off           = Frames(hpDif) * stepLoud
      val spanStart0    = FilterSeq(off, trigStart)
      val spanStop0     = FilterSeq(off, trigStop )
      val inverseOrder  = spanStart0.take(1) > spanStop0.take(1)
      val spanStart     = spanStart0
      val spanStop      = spanStop0.drop(inverseOrder)

      val spans         = spanStart zip spanStop
      val spanLengths   = spanStop - spanStart

      val slices        = Slices(mkIn(), BufferDisk(spans))
      val win           = GenWindow(spanLengths, GenWindow.Hann).pow(1.0/12)
      val windowed      = slices * win // BufferDisk(win)
      val lap           = OverlapAdd(windowed, size = spanLengths, step = spanLengths * 0.9)
      val sigOut        = lap

      AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
    }

    LayerUtil.renderFsc[S](g)
  }}
