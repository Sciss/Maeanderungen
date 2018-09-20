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

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.maeanderungen.Layer.Context
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.AudioCue

import scala.concurrent.Future

object SoundTransforms {
  def mkMask[S <: Sys[S]](fInFg: File, offFg: Long, fInBg: File, offBg: Long, numFrames: Long, nameOut: String /* fOut: File */)
                          (implicit tx: S#Tx, ctx: Context[S], config: Config): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    LayerUtil.mkRender(nameOut) { fOut =>
      runMask[S](fInFg = fInFg, offFg = offFg, fInBg = fInBg, offBg = offBg, numFrames = numFrames, fOut = fOut)
    }
  }

  private def runMask[S <: Sys[S]](fInFg: File, offFg: Long, fInBg: File, offBg: Long, numFrames: Long, fOut: File)
                          (implicit tx: S#Tx, config: Config): Future[Unit] = {

    val specFg = AudioFile.readSpec(fInFg)
    val specBg = AudioFile.readSpec(fInBg)

    assert(specFg.numFrames + offFg <= numFrames)
    assert(specBg.numFrames + offBg <= numFrames)

    println(s"runMask: fInFg: ${fInFg.name}, fInBg: ${fInBg.name}, fg: $specFg, bg: $specBg, offFg $offFg, offBg $offBg, numFrames $numFrames")

    val g = Graph {
      import de.sciss.fscape.graph._

      def mkIn(f: File, spec: AudioFileSpec, off: Long): GE = {
        val in0   = AudioFileIn(f, numChannels = config.numChannels)
        val in1   = if (off + spec.numFrames === numFrames) {
          in0
        } else {
          val pad = numFrames - (off + spec.numFrames)
          in0 ++ DC(0.0).take(pad)
        }
        val in2 = if (off === 0L) in1 else DC(0.0).take(off) ++ in1
        in2 // .elastic()
      }

      def mkFgIn(): GE = mkIn(fInFg, specFg, offFg)
      val inFg = mkFgIn()

      def mkBgIn(): GE = mkIn(fInBg, specBg, offBg)

      val inBg      = mkBgIn()
      val fMin      = 50.0
      val sr        = config.sampleRate.toDouble // 48000.0
      val winSizeH  = ((sr / fMin) * 3).ceil.toInt / 2
      val winSize   = winSizeH * 2
      val stepSize  = winSize / 2
      val fftSizeH  = winSize.nextPowerOfTwo
      val fftSize   = fftSizeH * 2

      val in1S      = Sliding(inFg, size = winSize, step = stepSize)
      val in2S      = Sliding(inBg, size = winSize, step = stepSize)
      val in1W      = in1S * GenWindow(winSize, shape = GenWindow.Hann)
      val in2W      = in2S * GenWindow(winSize, shape = GenWindow.Hann)

      val in1F      = Real1FFT(in1W, size = winSize, padding = fftSize - winSize)
      val in2F      = Real1FFT(in2W, size = winSize, padding = fftSize - winSize)

      val in1Mag    = in1F.complex.mag
      val in2Mag    = in2F.complex.mag

      val blurTime  = ((2.0 * sr) / stepSize) .ceil.toInt
      val blurFreq  = (200.0 / (sr / fftSize)).ceil.toInt
      val columns   = blurTime * 2 + 1
      def post      = DC(0.0).take(blurTime * fftSizeH)
      val in1Pad    = in1Mag ++ post
      val in2Pad    = in2Mag ++ post

      // println(s"blurTime $blurTime, blurFreq $blurFreq, winSize $winSize, stepSize $stepSize, fftSize $fftSize")

      val mask      = Masking(fg = in1Pad, bg = in2Pad, rows = fftSizeH, columns = columns,
        threshNoise = -56.0.dbAmp /* 0.5e-3 */, threshMask = -6.0.dbAmp /* 0.5 */, blurRows = blurFreq, blurColumns = blurTime)

      //    Plot1D(mask.drop(fftSizeH * 16).ampDb, size = fftSizeH)

      // RunningMax(mask < 1.0).last.poll(0, "has-filter?")

      val maskC     = mask zip DC(0.0)
      val fltSym    = (Real1IFFT(maskC, size = fftSize) / fftSizeH).drop(blurTime * fftSize)

      //    Plot1D(flt.drop(fftSize * 16), size = fftSize)

      val fftSizeCep  = fftSize * 2
      val fltSymR     = {
        val r   = RotateWindow(fltSym, fftSize, fftSizeH)
        val rr  = ResizeWindow(r  , fftSize, stop = fftSize)
        val rrr = RotateWindow(rr, fftSizeCep, -fftSizeH)
        rrr
      }
      //    val fltF        = Real1FullFFT(in = fltSymR, size = fftSize, padding = fftSize)
      val fltF        = Real1FullFFT(in = fltSymR, size = fftSizeCep, padding = 0)
      val fltFLogC    = fltF.complex.log.max(-320) // (-80)

      val cep         = Complex1IFFT(in = fltFLogC, size = fftSizeCep) / fftSize
      val cepOut      = FoldCepstrum(in = cep, size = fftSizeCep,
        crr = +1, cri = +1, clr = 0, cli = 0,
        ccr = +1, cci = -1, car = 0, cai = 0)

      val fltMinF     = Complex1FFT(in = cepOut, size = fftSizeCep) * fftSize
      val fltMinFExpC = fltMinF.complex.exp
      val fltMin0     = Real1FullIFFT (in = fltMinFExpC, size = fftSizeCep)
      val fltMin      = ResizeWindow(fltMin0, fftSizeCep, stop = -fftSize)

      // val writtenMin = AudioFileOut(fltMin, fOutMin, AudioFileSpec(numChannels = 1, sampleRate = sr))
      // writtenMin.poll(sr * 10, "frames-min")

      val bg        = mkBgIn() // .take(numFrames)
      val bgS       = Sliding(bg, size = winSize, step = stepSize)
      val bgW       = bgS * GenWindow(winSize, shape = GenWindow.Hann)
      val convSize  = (winSize + fftSize - 1).nextPowerOfTwo
      val bgF       = Real1FFT(bgW /* RotateWindow(bgW, winSize, -winSizeH) */, winSize, convSize - winSize)
      val fltMinFF  = Real1FFT(fltMin, fftSize, convSize - fftSize)
      val convF     = bgF.complex * fltMinFF
      val conv      = Real1IFFT(convF, convSize) * convSize
      val convLap   = OverlapAdd(conv, convSize, stepSize).take(numFrames)
      val sigOut    = convLap // .elastic()

      val writtenFlt = AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = config.numChannels, sampleRate = sr))
      // writtenFlt.poll(sr * 10, "frames-flt")
      val prog  = writtenFlt / numFrames
      val progT = Metro(specFg.sampleRate)
//      prog.poll(progT, "prog")
      Progress(prog, progT)
    }

    LayerUtil.renderFsc[S](g)
  }

  def mkBleach[S <: Sys[S]]()(implicit tx: S#Tx, ctx: Context[S]): Future[stm.Source[S#Tx, AudioCue.Obj[S]]] = {
    import ctx._
    val matVal    = material.value
    val base      = matVal.artifact.base
    val nameOut   = s"$base-Trns-Bleach.aif"
    LayerUtil.mkTransform(nameOut) { fOut =>
      runBleach(fOut = fOut)
    }
  }

  private def runBleach[S <: Sys[S]](fOut: File)(implicit tx: S#Tx, ctx: Context[S]): Future[Unit] = {
    val matVal              = ctx.material.value
    val fIn                 = matVal.artifact
    val specIn              = AudioFile.readSpec(fIn)
    import specIn.{sampleRate, numFrames}

    val g = Graph {
      import graph._
      def mkIn() = AudioFileIn(file = fIn, numChannels = 1)

      val in          = mkIn()
      val fltLen      = 441
      val feedback    = -50.0.dbAmp
      val clip        =  18.0.dbAmp
      val sigOut      = Bleach(in, filterLen = fltLen, feedback = feedback, filterClip = clip)
      val writtenFlt  = AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
      val prog        = writtenFlt / numFrames
      val progT       = Metro(sampleRate)
      Progress(prog, progT)
    }

    LayerUtil.renderFsc[S](g)
  }
}