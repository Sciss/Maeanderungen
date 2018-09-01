package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.synth.io.AudioFile

object PitchDirTest extends App {
  val fPitchIn      = file("/data/projects/Maeanderungen/analysis/HB_1_HH_T168-pitch.aif")
  val pauseStartSec = 14.304
  val specIn        = AudioFile.readSpec(fPitchIn)
  val frameStop     = (pauseStartSec * specIn.sampleRate).toInt
  val pitchDirSec   = 0.7
  val pitchDirNum   = (pitchDirSec * specIn.sampleRate).toInt
  println(s"frameStop $frameStop, testFrames $pitchDirNum")

  val afPitch = AudioFile.openRead(fPitchIn)
  val pitches = try {
    val numPitches = afPitch.numFrames.toInt
    println(s"  numPitches $numPitches")
    val buf = afPitch.buffer(numPitches)
    afPitch.read(buf)
    buf(0)
  } finally {
    afPitch.close()
  }

  def isPitchDown(frame: Int): Boolean = {
    val stopI: Int = math.min(pitches.length, frame)
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
      println(s"  ... dir $tail, $init -> ${tail / init}")
      tail < init
    }
  }

  val test = isPitchDown(frameStop)
  println(s"Result: $test")
}
