package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PitchTest extends App {
  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/HB_0_HH_T168.wav")
  val fOut    = file("/data/temp/test.aif")
  val fOut2   = file("/data/temp/test2.aif")
  val specIn  = AudioFile.readSpec(fIn)
  import specIn.sampleRate

  def any2stringadd: Any = ()

  lazy val g = Graph {
    import de.sciss.fscape.graph._
    val in          = AudioFileIn(file = fIn, numChannels = 1)
    val numFrames   = specIn.numFrames
//    val numFrames   = sampleRate * 10
//    val f0N         = 200.0/sampleRate
//    val DROP = 3
//    val in         = (SinOsc(f0N) * 0.5 + SinOsc(2*f0N) * 0.25 + SinOsc(3*f0N) * 0.125).drop(DROP).take(numFrames)
    val fftSize     = 4096 // 2048  // c. 40 ms
    val winStep     = fftSize / 4
    val inW0        = Sliding (in = in , size = fftSize, step = winStep)
    val inW1        = inW0 * GenWindow(fftSize, shape = GenWindow.Hann)
    val inW         = RotateWindow(inW1, fftSize, fftSize/2)
    val fft         = Real1FullFFT(in = inW, size = fftSize)
    val logC        = fft.complex.log.max(-160)
    val cep0        = Real1FullIFFT(logC, size = fftSize) / fftSize
    val cep1        = Biquad(cep0, b0 = 0.3333, b1 = 0.3333, b2 = 0.3333) // OnePole(cep0, 0.95)
    val foo = LFSaw(1.0 / fftSize).linLin(-1, 1, 1, 0)
    val cep = cep1 * foo // .elastic()
    cep.poll(0, "cep")
//    val cep         = Real1FFT(logC, size = fftSize, mode = 2)
//    val clip        = (cep - 0.03).max(0.0)
    val minFreq     =  40.0 // 100.0
    val maxFreq     = 200.0 // 1000.0
    val minDly      = (sampleRate / maxFreq).floor
    val maxDly      = (sampleRate / minFreq).ceil
    val lm          = DetectLocalMax(cep.elastic(), size = 9) & cep.elastic() > 0.01 // 3
    lm.poll(0, "lm")
    val fr          = Frames(DC(0)) - 1
    val winStart    = Latch(fr, Metro(fftSize))
//    val minFreq     =  40.0
    val winOff      = fr - winStart
    // winOff.poll(10, label = "winOff")
    // winStart.poll(10, label = "winStart")
//    val maxFreq     = 200.0
    // println(s"minDly $minDly, maxDly $maxDly")
    val frameOk     = winOff >= minDly & winOff <= maxDly
    val lmOk        = lm.elastic() & frameOk.elastic()
    lmOk.poll(0, "lmOk")

    // fr.poll(lmOk, "lm-ok")
    // val dly         = WindowIndexWhere(lmOk, size = fftSize)
    val dly         = WindowMaxIndex(cep.elastic(2) * lmOk, size = fftSize)
    dly.poll(0, "dly-0")
    val found       = dly > 0
    val freq0       = ((dly).max(1).reciprocal * sampleRate) * found.elastic()

    val loud        = Loudness(inW, sampleRate = sampleRate, size = fftSize, spl = 70, diffuse = 1)
    val freq1       = freq0 * (loud > 15)
    val freq        = SlidingPercentile(freq1, len = 3)
    val hasFreq     = freq > 0

    freq.poll(hasFreq, "pitch [Hz]")
    val sig         =  cep // freq
    AudioFileOut(sig, file = fOut, spec = AudioFileSpec(numChannels = 1, sampleRate = sampleRate / winStep))

    val f0N_0 = RepeatWindow(freq, size = 1, num = winStep) / sampleRate
    val f0N_ = OnePole(f0N_0, 0.9)
    val sin = (SinOsc(f0N_) * 0.5 + SinOsc(f0N_ * 2) * 0.4 + SinOsc(f0N_ * 3) * 0.3 + SinOsc(f0N_ * 4) * 0.2 + SinOsc(f0N_ * 5) * 0.1).take(numFrames) *
      OnePole(f0N_ > 0, 0.9) * 0.5
    AudioFileOut(sin, file = fOut2, spec = AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
  }

  val config = stream.Control.Config()
  config.useAsync = false
  implicit val ctrl: stream.Control = stream.Control(config)
  ctrl.run(g)
  Await.result(ctrl.status, Duration.Inf)
}
