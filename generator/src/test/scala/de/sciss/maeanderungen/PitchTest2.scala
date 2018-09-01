package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.{GE, Graph, stream}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PitchTest2 extends App {
  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/HB_0_HH_T168.wav")
//  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/SV_2_MR_T165.aif")
  val isMale  = fIn.name.contains("_HH_")
  val fOutPch = file("/data/temp/") / s"${fIn.base}-pitches.aif"
  val fOutDir = file("/data/temp/") / s"${fIn.base}-dir.aif"
//  val fOut2   = file("/data/temp/test2.aif")
  val specIn  = AudioFile.readSpec(fIn)
  import specIn.sampleRate

  def any2stringadd: Any = ()

  lazy val g = Graph {
    import de.sciss.fscape.graph._
    val in          = AudioFileIn(file = fIn, numChannels = 1) // .drop(24000)
    // val numFrames   = specIn.numFrames

    val MinimumPitch        = if (isMale)  60.0 else 100.0 // 100.0
    val MaximumPitch        = if (isMale) 200.0 else 320.0 // 1000.0
    val VoicingThreshold    = 0.45
    val SilenceThreshold    = 0.03
    val OctaveCost          = 0.01
    val OctaveJumpCost      = 0.35
    val VoicedUnvoicedCost  = 0.14
    val NumCandidates       = 15

    val pch = PitchAC(in, sampleRate = sampleRate, pitchMin = MinimumPitch, pitchMax = MaximumPitch,
      voicingThresh = VoicingThreshold, silenceThresh = SilenceThreshold, octaveCost = OctaveCost,
      octaveJumpCost = OctaveJumpCost, voicedUnvoicedCost = VoicedUnvoicedCost,
      numCandidates = NumCandidates)

    // RepeatWindow(pch).poll(2, "f")
    val srOut = {
      val maxLag      = (sampleRate / MinimumPitch).ceil.toInt
      val numPeriods  = 3
      val winSize     = maxLag * /* pch. */ numPeriods
      val stepSize    = winSize / 4
      sampleRate / /* pch. */ stepSize
    }
//    println(s"srOut = $srOut")
    AudioFileOut(pch, fOutPch, AudioFileSpec(numChannels = 1, sampleRate = srOut))

    val dlyLen  = 0.25 * srOut
    val pchDly  = DelayN(pch, dlyLen, dlyLen)
    val pchB    = BufferMemory(pch, dlyLen)
    def mkPole(in: GE) = {
      OnePole(Latch(in, in > 0), 0.95)
    }
    val dir0    = mkPole(pchB) - mkPole(pchDly)
    val hasFreq = pchB > 0
    val stopped = Biquad(hasFreq, -1, 1)
    val dir     = Gate(dir0, stopped)
    AudioFileOut(dir, fOutDir, AudioFileSpec(numChannels = 1, sampleRate = srOut))
  }

  val config = stream.Control.Config()
  config.useAsync = false
  implicit val ctrl: stream.Control = stream.Control(config)
  ctrl.run(g)
  Await.result(ctrl.status, Duration.Inf)
}
