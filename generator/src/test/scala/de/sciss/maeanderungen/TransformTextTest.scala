package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.graph._
import de.sciss.fscape.{Graph, stream}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TransformTextTest extends App {
//  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/MT-13_HH_T152.wav")
//  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/CN_7_HH_T046.wav")
  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/CN_1_RH_T014.wav")
//  val fIn     = file("/data/projects/Maeanderungen/audio_work/edited/HB_1_HH_T168.aif")
  val fOut    = file("/data/temp/text-transform-test.aif")
  val specIn  = AudioFile.readSpec(fIn)
  import specIn.sampleRate

  def transformNoPitch(): Graph = Graph {
//    val isMale              = true
    val MinimumPitch        = 60.0  // if (isMale)  60.0 else 100.0 // 100.0
    val MaximumPitch        = 320.0 // if (isMale) 200.0 else 320.0 // 1000.0
    val VoicingThreshold    = 0.6  // 0.45
    val SilenceThreshold    = 0.03
    val OctaveCost          = 0.01
    val OctaveJumpCost      = 0.35
    val VoicedUnvoicedCost  = 0.14
    val NumCandidates       = 15

    def mkIn() = AudioFileIn(file = fIn, numChannels = 1)

    val in = mkIn()
    val pch = PitchAC(in, sampleRate = sampleRate, pitchMin = MinimumPitch, pitchMax = MaximumPitch,
      voicingThresh = VoicingThreshold, silenceThresh = SilenceThreshold, octaveCost = OctaveCost,
      octaveJumpCost = OctaveJumpCost, voicedUnvoicedCost = VoicedUnvoicedCost,
      numCandidates = NumCandidates)
    import pch.stepSize

    val hasPitch      = pch > 0
    val hpDif         = Differentiate(hasPitch)
//    val trigStart     = hpDif sig_== +1
//    val trigStop      = hpDif sig_== -1
    val trigStart     = hpDif sig_== -1
    val trigStop      = hpDif sig_== +1
    val off           = Frames(hpDif) * stepSize
    val spanStart0    = FilterSeq(off, trigStart) // .dropRight(1) -- not yet implemented
    val spanStop0     = FilterSeq(off, trigStop) // .drop(1)
    val inverseOrder  = spanStart0.take(1) > spanStop0.take(1)
    val spanStart     = spanStart0
    val spanStop      = spanStop0.drop(inverseOrder)

    val spans         = spanStart zip spanStop
    val spanLengths   = spanStop - spanStart

    spanStart.poll(0, "start")
    spanStop .poll(0, "stop" )

    val slices        = Slices(mkIn(), BufferDisk(spans))
    val win           = GenWindow(spanLengths, GenWindow.Hann).pow(1.0/12)
    val windowed      = slices * win // BufferDisk(win)
    val lap           = OverlapAdd(windowed, size = spanLengths, step = spanLengths * 0.9)
    val sigOut        = lap

    AudioFileOut(sigOut, fOut, AudioFileSpec(numChannels = 1, sampleRate = sampleRate))
  }

  val g = transformNoPitch()
  val config = stream.Control.Config()
  config.useAsync = false
  implicit val ctrl: stream.Control = stream.Control(config)
  ctrl.run(g)
  Await.result(ctrl.status, Duration.Inf)
}
