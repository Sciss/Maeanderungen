/*
 *  Old.scala
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
import javax.imageio.ImageIO

import de.sciss.equal.Implicits._
import de.sciss.file.File
import de.sciss.maeanderungen.CracksAnalysis.{Config, POLE_COOKIE, requireCanWrite}
import de.sciss.{kollflitz, numbers}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

object Old {
  def calcAudio(fPolesIn: File, fImgIn: File, fOut: File, audioBreadthStep: Int = 2)(implicit config: Config): Unit = {
    import config._

    requireCanWrite(fOut)
    val img   = ImageIO.read(fImgIn)
    val xMax  = img.getWidth  - 1
    val yMax  = img.getHeight - 1

    def getValue(x: Int, y: Int): Float = {
      val rgb = img.getRGB(x, y)
      val r   = ((rgb >> 16) & 0xFF) / 255f
      val g   = ((rgb >>  8) & 0xFF) / 255f
      val b   = ( rgb        & 0xFF) / 255f
      0.2126f * r + 0.7152f * g + 0.0722f * b
    }

    val sIn = new FileInputStream(fPolesIn)
    try {
      val dIn = new DataInputStream(sIn)
      import dIn._
      val cookie = readInt()
      require(cookie === POLE_COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${POLE_COOKIE.toHexString}")

      val afOut = AudioFile.openWrite(fOut, AudioFileSpec(numChannels = 1, sampleRate = 44100))
      try {
        val pixelStep = 1.0 / audioBreadthStep // / SCALE_DOWN
        val buf     = afOut.buffer()
        val buf0    = buf(0)
        val bufLen  = buf0.length
        var bufOff  = 0

        def flush(): Unit = if (bufOff > 0) {
          afOut.write(buf, 0, bufOff)
          bufOff = 0
        }

        var dcMem0 = 0f
        var dcMem1 = 1f

        while (available() > 0) {
          val x1        = readFloat() * imgScaleDown
          val y1        = readFloat() * imgScaleDown
          val x2        = readFloat() * imgScaleDown
          val y2        = readFloat() * imgScaleDown
          //          val ln        = new LineFloat2D(x1, y1, x2, y2)
          //          println(ln)
          val dx        = x2 - x1
          val dy        = y2 - y1
          val len       = math.sqrt(dx*dx + dy*dy)
          val numSteps  = math.round(len / pixelStep).toInt + 1

          import numbers.Implicits._

          val seq = for (step <- 0 until numSteps) yield {
            val xi: Float = step.linlin(0, numSteps, x1, x2)
            val yi: Float = step.linlin(0, numSteps, y1, y2)

            val xj  = xi.toInt.clip(0, xMax)
            val xk  = (xj + 1).min(xMax)
            val yj  = yi.toInt.clip(0, yMax)
            val yk  = (yj + 1).min(yMax)
            val xwk = xi % 1.0f
            val xwj = 1.0f - xwk
            val ywk = yi % 1.0f
            val ywj = 1.0f - ywk

            val v1  = getValue(xj, yj) * xwj * ywj
            val v2  = getValue(xk, yj) * xwk * ywj
            val v3  = getValue(xj, yk) * xwj * ywk
            val v4  = getValue(xk, yk) * xwk * ywk
            val v: Float = 1f - (v1 + v2 + v3 + v4)

            v
            //            buf0(bufOff) = v
            //            bufOff += 1
            //            if (bufOff == bufLen) flush()
          }


          import kollflitz.Ops._
          //          val x = seq.sum / seq.size
          val x = seq.variance.sqrt
          // dc-block: Y1 = X1-X0+Y0*gain
          val y = x-dcMem0+dcMem1*0.99f
          dcMem0 = x
          dcMem1 = y

          buf0(bufOff) = y
          bufOff += 1
          if (bufOff == bufLen) flush()
        }

        flush()

      } finally {
        afOut.close()
      }

    } finally {
      sIn.close()
    }
  }

}
