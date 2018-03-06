/*
 *  BridgePolesBinToAudio.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import java.io.{DataInputStream, FileInputStream}

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.maeanderungen.CracksAnalysis.POLE_COOKIE
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

// temporary program to convert old binary format files
object BridgePolesBinToAudio {
  def main(args: Array[String]): Unit = {
    val fIn   = file("/data/projects/Maeanderungen/cracks/cracks2_poles.bin")
    val fOut  = fIn.replaceExt("aif")
    require (fIn.isFile && !fOut.exists())
    run(fIn = fIn, fOut = fOut, imgScaleDown = 8)
  }

  def run(fIn: File, fOut: File, imgScaleDown: Int): Unit = {
    val sIn = new FileInputStream(fIn)
    try {
      val dIn = new DataInputStream(sIn)
      import dIn._
      val cookie = readInt()
      require(cookie === POLE_COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${POLE_COOKIE.toHexString}")
      val numPoles = available() / (4 * 4)
      val afOut = AudioFile.openWrite(fOut, AudioFileSpec(numChannels = 4, sampleRate = 44100))
      try {
        val buf = afOut.buffer(numPoles)
        for (i <- 0 until numPoles) {
          val x1 = readFloat() * imgScaleDown
          val y1 = readFloat() * imgScaleDown
          val x2 = readFloat() * imgScaleDown
          val y2 = readFloat() * imgScaleDown
          buf(0)(i) = x1
          buf(1)(i) = y1
          buf(2)(i) = x2
          buf(3)(i) = y2
        }
        afOut.write(buf)
      } finally {
        afOut.close()
      }
    } finally {
      sIn.close()
    }
  }
}