/*
 *  CamShot.scala
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

import java.util.Locale

import com.hopding.jrpicam.RPiCamera
import com.hopding.jrpicam.enums.{AWB, DRC, Encoding, MeteringMode, Exposure => PiExposure}
import de.sciss.file._
import scopt.OptionParser

import scala.util.Try

object CamShot {
  case class Config(
                    outputFile: File          = file("out.jpg"),
                    width     : Int           = 3280,
                    height    : Int           = 2464, // /2
                    shutter   : Int           = 0, // 10000,
                    iso       : Int           = 0, // 100,
                    exposure  : PiExposure    = PiExposure.AUTO,
                    awb       : AWB           = AWB.AUTO,
                    redGain   : Double        = 1.0,
                    greenGain : Double        = 1.0,
                    blueGain  : Double        = 1.0,
                    drc       : DRC           = DRC.OFF,
                    metering  : MeteringMode  = MeteringMode.AVERAGE,
                    flipH     : Boolean       = false,
                    flipV     : Boolean       = false,
                    encoding  : Encoding      = Encoding.JPG
                   )

  def main(args: Array[String]): Unit = {
    val default = Config()
    val parser = new OptionParser[Config]("CamShot") {
      opt[File  ]('o', "output")
        .required()
        .text("Output file")
        .action { (x, c) => c.copy(outputFile = x) }
      opt[Int   ]('w', "width")
        .text(s"Image width in pixels  (default: ${default.width})")
        .action { (x, c) => c.copy(width  = x) }
      opt[Int   ]('h', "height")
        .text(s"Image height in pixels (default: ${default.height})")
        .action { (x, c) => c.copy(height = x) }
      opt[Int   ]('i', "iso")
        .text(s"ISO value (default: ${default.iso})")
        .action { (x, c) => c.copy(iso = x) }
      opt[String]('e', "exposure")
        .text(s"Exposure type (${PiExposure.values().mkString(", ")}; default: ${default.exposure})")
        .validate(x => if (Try(PiExposure.valueOf(x.toUpperCase(Locale.US))).isSuccess) success else failure(s"Not a valid exposure type: $x"))
        .action { (x, c) =>
          c.copy(exposure = PiExposure.valueOf(x.toUpperCase(Locale.US)))
        }
      opt[String]('a', "awb")
        .text(s"Automatic white balance (${AWB.values().mkString(", ")}; default ${default.awb})")
        .validate(x => if (Try(AWB.valueOf(x.toUpperCase(Locale.US))).isSuccess) success else failure(s"Not a valid AWB type: $x"))
        .action { (x, c) =>
          c.copy(awb = AWB.valueOf(x.toUpperCase(Locale.US)))
        }
      opt[String]('c', "drc")
        .text(s"Dynamic range compression (${DRC.values().mkString(", ")}; default: ${default.drc})")
        .validate(x => if (Try(DRC.valueOf(x.toUpperCase(Locale.US))).isSuccess) success else failure(s"Not a valid DRC type: $x"))
        .action { (x, c) =>
          c.copy(drc = DRC.valueOf(x.toUpperCase(Locale.US)))
        }
      opt[String]('m', "metering")
        .text(s"Metering mode (${MeteringMode.values().mkString(", ")}; default: ${default.metering})")
        .validate(x => if (Try(MeteringMode.valueOf(x.toUpperCase(Locale.US))).isSuccess) success else failure(s"Not a valid metering mode: $x"))
        .action { (x, c) =>
          c.copy(metering = MeteringMode.valueOf(x.toUpperCase(Locale.US)))
        }
      opt[String]('f', "encoding")
        .text(s"Encoding (${Encoding.values().mkString(", ")}; default: ${default.encoding})")
        .validate(x => if (Try(Encoding.valueOf(x.toUpperCase(Locale.US))).isSuccess) success else failure(s"Not a valid encoding: $x"))
        .action { (x, c) =>
          c.copy(encoding = Encoding.valueOf(x.toUpperCase(Locale.US)))
        }
      opt[Unit  ]('x', "flip-h")
        .text("Flip horizontally")
        .action { (_, c) => c.copy(flipH = true) }
      opt[Unit  ]('y', "flip-v")
        .text("Flip vertically")
        .action { (_, c) => c.copy(flipV = true) }
      opt[Double]("red")
        .text(s"Red gain   (default: ${default.redGain})")
        .action { (x, c) => c.copy(redGain = x) }
      opt[Double]("green")
        .text(s"Green gain (default: ${default.greenGain})")
        .action { (x, c) => c.copy(greenGain = x) }
      opt[Double]("blue")
        .text(s"Blue gain  (default: ${default.blueGain})")
        .action { (x, c) => c.copy(blueGain = x) }
    }
    parser.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    import config._

    val siteDir = outputFile.parent // outputDir / s"site-$countSite"
    require(siteDir.isDirectory && siteDir.canWrite)

    val cam = new RPiCamera(siteDir.path)
    // cf. https://raspberrypi.stackexchange.com/questions/14047/
    if (shutter > 0) cam.setShutter(shutter) // 500000
    if (iso     > 0) cam.setISO(iso)
    cam.setExposure(exposure)
    cam.setAWB(awb)
    cam.setAWBGains(redGain / greenGain, blueGain / greenGain)
    cam.setMeteringMode(metering)
    if (flipH) cam.setHorizontalFlipOn()
    if (flipV) cam.setVerticalFlipOn()
    cam.setDRC(drc)
    cam.setEncoding(encoding)
    cam.turnOffPreview()
    val name = outputFile.name // s"frame-$count.$ext"
    cam.takeStill(name, width, height)
  }
}