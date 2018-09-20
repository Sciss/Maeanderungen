/*
 *  SoundTransforms.scala
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
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Random, Source, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.maeanderungen.Layer.Context
import de.sciss.maeanderungen.Ops._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.AudioCue

import scala.concurrent.Future

object SoundTransforms {
  val weighted: Vec[(Double, Transform)] = Util.normalizeWeights(Vec(
    0.8 -> Transform.Bleach,
    0.2 -> Transform.Fourier,
    0.2 -> Transform.Filter,
  ))

  def choose[Tx <: TxnLike]()(implicit tx: Tx, r: Random[Tx]): Transform = {
    import Ops._
    weighted.chooseWeighted(_._1)._2
  }

  object Transform {
    case object Bleach extends Transform {
      def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[Source[S#Tx, AudioCue.Obj[S]]] = {
        import ctx.rnd
        val inverse   = 0.5.coin()
        val fltLen    = rangeRand(294, 661).toInt
        mkBleach[S](inverse = inverse, fltLen = fltLen)
      }
    }

    case object Fourier extends Transform {
      def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[Source[S#Tx, AudioCue.Obj[S]]] = {
        import ctx.rnd
        val inverse = 0.5.coin()
        mkFourier[S](inverse = inverse)
      }
    }

    case object Filter extends Transform {
      def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[Source[S#Tx, AudioCue.Obj[S]]] = {
        import ctx.rnd
        val isLow   = 0.5.coin()
        val f1      = expRand(160, 16000)
        val f2      = f1 * expRand(2.0/3.0, 3.0/2.0)
        mkFilter[S](isLowPass = isLow, f1 = f1, f2 = f2)
      }
    }
  }
  sealed trait Transform {
    def make[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]]
  }

  def mkBleach[S <: Sys[S]](inverse: Boolean, fltLen: Int)(implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val inverseS  = if (inverse) "I" else ""
    val nameOut   = s"$base-Trns-Bleach$inverseS-$fltLen.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runBleach(fOut = fOut, inverse = inverse, fltLen = fltLen)
    }
  }

  private def runBleach[S <: Sys[S]](fOut: File, inverse: Boolean, fltLen: Int)
                                    (implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal  = ctx.material.value
    val fIn     = matVal.artifact
    val specIn  = AudioFile.readSpec(fIn)
    import specIn.{numFrames, sampleRate}

    val g = Graph {
      import graph._
      def mkIn()      = AudioFileIn(file = fIn, numChannels = 1)
      val in          = mkIn()
      val feedback    = -50.0.dbAmp
      val clip        =  18.0.dbAmp
      val sig0        = Bleach(in, filterLen = fltLen, feedback = feedback, filterClip = clip)
      val sig1        = if (!inverse) sig0 else in.elastic() - sig0
      val sigOut      = normalize(sig1)
      val writtenFlt  = AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
      val prog        = writtenFlt / numFrames
      val progT       = Metro(sampleRate)
      Progress(prog, progT)
    }

    LayerUtil.renderFsc[S](g)
  }

  def mkFourier[S <: Sys[S]](inverse: Boolean)(implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val inverseS  = if (inverse) "I" else ""
    val nameOut   = s"$base-Trns-Fourier${inverseS}.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runFourier(fOut = fOut, inverse = inverse)
    }
  }

  private def runFourier[S <: Sys[S]](fOut: File, inverse: Boolean)
                                     (implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal  = ctx.material.value
    val fIn     = matVal.artifact
    val specIn  = AudioFile.readSpec(fIn)
    import specIn.{numFrames, sampleRate}

    val g = Graph {
      import graph._
      def mkIn()      = AudioFileIn(file = fIn, numChannels = 1)
      val in          = mkIn()
      val co          = in zip DC(0.0)
      val f           = Fourier(in = co, size = numFrames << 1, dir = if (inverse) -1 else 1, mem = 524288)
      val sig1        = f.take(numFrames)
      val sigOut      = normalize(sig1)
      val writtenFlt  = AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
      val prog        = writtenFlt / numFrames
      val progT       = Metro(sampleRate)
      Progress(prog, progT)
    }

    LayerUtil.renderFsc[S](g)
  }

  def mkFilter[S <: Sys[S]](isLowPass: Boolean, f1: Double, f2: Double)
                           (implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val filterS   = if (isLowPass) "LPF" else "HPF"
    val nameOut   = s"$base-Trns-$filterS-${f1.toInt}-${f2.toInt}.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runFilter(fOut = fOut, isLowPass = isLowPass, f1 = f1, f2 = f2)
    }
  }

  private def normalize(in: GE): GE = {
    import graph._
    val b     = BufferDisk(in)
    val max   = RunningMax(in.abs).last.max(-320.dbAmp)
    val gain  = max.reciprocal
    b * gain
  }

  private def runFilter[S <: Sys[S]](fOut: File, isLowPass: Boolean, f1: Double, f2: Double)
                                     (implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal  = ctx.material.value
    val fIn     = matVal.artifact
    val specIn  = AudioFile.readSpec(fIn)
    import specIn.{numFrames, sampleRate}

    val g = Graph {
      import graph._
      def mkIn()      = AudioFileIn(file = fIn, numChannels = 1)
      val in          = mkIn()
      val freqHz      = Line(f1, f2, numFrames)
      val freqN       = freqHz / sampleRate
      val f           = if (isLowPass) LPF(in, freqN) else HPF(in, freqN)
      val sig1        = f // .take(numFrames)
      val sigOut      = normalize(sig1)
      val writtenFlt  = AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
      val prog        = writtenFlt / numFrames
      val progT       = Metro(sampleRate)
      Progress(prog, progT)
    }

    LayerUtil.renderFsc[S](g)
  }
}