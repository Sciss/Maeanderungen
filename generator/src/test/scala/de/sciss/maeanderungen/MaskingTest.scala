package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.numbers.Implicits._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MaskingTest extends App {
  val fIn1 = file("/data/projects/Maeanderungen/audio_work/edited/SV_2_NC_T167.wav")
  val fIn2 = file("/data/projects/Maeanderungen/audio_work/edited/SV_2_NC_T167.wav")

  val g = Graph {
    import de.sciss.fscape.graph._

    val in1       = AudioFileIn(fIn1, numChannels = 1).drop(29363)
    val in2       = AudioFileIn(fIn2, numChannels = 1).drop(40000) * 0.7
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

    println(s"blurTime $blurTime, blurFreq $blurFreq")

    val mask      = Masking(fg = in1Pad, bg = in2Pad, rows = fftSizeH, columns = columns,
      threshNoise = 0.5e-3, threshMask = 0.5, blurRows = blurFreq, blurColumns = blurTime)

//    Plot1D(mask.drop(fftSizeH * 16).ampDb, size = fftSizeH)

    val maskC     = mask zip DC(0)
    val flt       = Real1IFFT(maskC, size = fftSize)

    Plot1D(flt.drop(fftSize * 16), size = fftSize)
  }

  val config = stream.Control.Config()
  config.useAsync = false
  implicit val ctrl: stream.Control = stream.Control(config)
  ctrl.run(g)
  Await.result(ctrl.status, Duration.Inf)
}
