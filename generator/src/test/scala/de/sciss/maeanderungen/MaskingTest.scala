package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MaskingTest extends App {
  val fIn1      = file("/data/projects/Maeanderungen/audio_work/edited/SV_2_NC_T167.wav")
  val fIn2      = file("/data/projects/Maeanderungen/wolke/AUFNAHMEN/Piezo/PZ_03.wav")
  val fOut      = file("/data/temp/masking-ir.aif")
  val specIn1   = AudioFile.readSpec(fIn1)
  val specIn2   = AudioFile.readSpec(fIn2)
  val numFrames = math.min(specIn1.numFrames, specIn2.numFrames)

  val g = Graph {
    import de.sciss.fscape.graph._

    val in1       = AudioFileIn(fIn1, numChannels = 1) // .drop(29363)
//    val in2       = AudioFileIn(fIn2, numChannels = 2).out(0) // .drop(40000) * 0.7
    val in2       = AudioFileIn(fIn2, numChannels = 1) * 2
    val fMin      = 50.0
    val sr        = 48000.0
    val winSize   = ((sr / fMin) * 3).ceil.toInt
    val stepSize  = winSize / 2
    val fftSizeH  = winSize.nextPowerOfTwo
    val fftSize   = fftSizeH * 2

    val in1S      = Sliding(in1, size = winSize, step = stepSize)
    val in2S      = Sliding(in2, size = winSize, step = stepSize)
    val in1W      = in1S * GenWindow(winSize, shape = GenWindow.Hann)
    val in2W      = in2S * GenWindow(winSize, shape = GenWindow.Hann)

    val in1F      = Real1FFT(in1W, size = winSize, padding = fftSize - winSize)
    val in2F      = Real1FFT(in2W, size = winSize, padding = fftSize - winSize)

    val in1Mag    = in1F.complex.mag
    val in2Mag    = in2F.complex.mag

    val blurTime  = ((0.5 * sr) / stepSize).ceil.toInt
    val blurFreq  = (150.0 / (sr / fftSize)).ceil.toInt
    val columns   = blurTime * 2 + 1
    def post      = DC(0).take(blurTime * fftSizeH)
    val in1Pad    = in1Mag ++ post
    val in2Pad    = in2Mag ++ post

    println(s"blurTime $blurTime, blurFreq $blurFreq, winSize $winSize, stepSize $stepSize, fftSize $fftSize")

    val mask      = Masking(fg = in1Pad, bg = in2Pad, rows = fftSizeH, columns = columns,
      threshNoise = 0.5e-3, threshMask = 0.5, blurRows = blurFreq, blurColumns = blurTime)

//    Plot1D(mask.drop(fftSizeH * 16).ampDb, size = fftSizeH)

    val maskC     = mask zip DC(0)
    val fltSym    = (Real1IFFT(maskC, size = fftSize) / fftSizeH).drop(blurTime * fftSize)

//    Plot1D(flt.drop(fftSize * 16), size = fftSize)

    val fftSizeCep  = fftSize * 2
    val fltSymR     = {
      RotateWindow(fltSym, fftSize, fftSizeH)
    }
    val fltF        = Real1FullFFT(in = fltSymR, size = fftSize, padding = fftSize)
    val fltFLogC    = fltF.complex.log.max(-160) // (-80)

    val cep         = Complex1IFFT(in = fltFLogC, size = fftSizeCep) / fftSize
    val cepOut      = FoldCepstrum(in = cep, size = fftSizeCep,
      crr = +1, cri = +1, clr = 0, cli = 0,
      ccr = +1, cci = -1, car = 0, cai = 0)

    val fltMinF     = Complex1FFT(in = cepOut, size = fftSizeCep) * fftSize
    val fltMinFExpC = fltMinF.complex.exp
    val fltMin0     = Real1FullIFFT (in = fltMinFExpC, size = fftSizeCep)
    val fltMin      = ResizeWindow(fltMin0, fftSizeCep, stop = -fftSize)

    val framesWritten = AudioFileOut(fltMin, fOut, AudioFileSpec(numChannels = 1, sampleRate = sr))
    framesWritten.poll(sr * 10, "frames")
  }

  val config = stream.Control.Config()
  config.useAsync = false
  implicit val ctrl: stream.Control = stream.Control(config)
  ctrl.run(g)
  Await.result(ctrl.status, Duration.Inf)
  println("Done.")
}