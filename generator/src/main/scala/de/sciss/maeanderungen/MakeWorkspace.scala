/*
 *  MakeWorkspace.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc
import de.sciss.synth.proc.{Durable, SoundProcesses, Workspace}

import scala.util.control.NonFatal

object MakeWorkspace {
  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("MakeWorkspace") {
      opt[File]('w', "workspace")
        .required()
        .text(s"Input Mellite workspace that contains 'material' folder.")
        .action { (v, c) => c.copy(ws = v) }

      opt[File]('b', "base")
        .required()
        .text(s"Base directory.")
        .action { (v, c) => c.copy(baseDir = v) }

      opt[Double]("min-dur")
        .text(s"Minimum duration in seconds (default: ${default.minDur})")
        .validate { v => if (v > 1) success else failure("Must be > 1 seconds") }
        .action { (v, c) => c.copy(minDur = v) }

      opt[Double]("max-dur")
        .text(s"Maximum duration in seconds (default: ${default.maxDur})")
        .validate { v => if (v > 1) success else failure("Must be > 1 seconds") }
        .action { (v, c) => c.copy(maxDur = v) }

      opt[Unit]('r', "radio")
        .text("Version for radio instead of installation")
        .action { (_, c) => c.copy(radio = true) }

//      opt[Unit]("no-prepare")
//        .text("Do not run preparation stage")
//        .action { (_, c) => c.copy(prepare = false) }
//
//      opt[Unit]("no-render")
//        .text("Do not run rendering stage")
//        .action { (_, c) => c.copy(render = false) }

      opt[Double]("prob-transform-text")
        .text(s"Probability (0 to 1) for transformable text to be actually undergoing transformation (default: ${default.probTransformText})")
        .validate { v => if (v >= 0 && v <= 1) success else failure("Must be 0 to 1") }
        .action { (v, c) => c.copy(probTransformText = v) }

      opt[Double]("prob-transform-sound")
        .text(s"Probability (0 to 1) for sound materials to be undergoing transformation (default: ${default.probTransformSound})")
        .validate { v => if (v >= 0 && v <= 1) success else failure("Must be 0 to 1") }
        .action { (v, c) => c.copy(probTransformSound = v) }

      opt[Double]("prob-shorten")
        .text(s"Probability (0 to 1) for materials to be shorted if they allow this (default: ${default.probShorten})")
        .validate { v => if (v >= 0 && v <= 1) success else failure("Must be 0 to 1") }
        .action { (v, c) => c.copy(probShorten = v) }

      opt[Double]("prob-shorten-fade")
        .text(s"When shortening materials, probability (0 to 1) that we fade in/out instead of hard cut (default: ${default.probShortenFade})")
        .validate { v => if (v >= 0 && v <= 1) success else failure("Must be 0 to 1") }
        .action { (v, c) => c.copy(probShortenFade = v) }

      opt[Int]('c', "num-channels")
        .text(s"Number of output channels, >= 2 (default: ${default.numChannels})")
        .validate { v => if (v >= 2) success else failure("Must be >= 2") }
        .action { (v, c) => c.copy(numChannels = v) }

      opt[Long]("seed")
        .text("Random number generator seed value")
        .action { (v, c) => c.copy(seed = Some(v)) }

      opt[Double]("max-pan")
        .text(s"Maximum pan extent for mono signals (0 to 1) (default: ${default.maxPan})")
        .validate { v => if (v >= 0 && v <= 1) success else failure("Must be 0 to 1") }
        .action { (v, c) => c.copy(maxPan = v) }

      opt[Double]('t', "text-sound-ratio")
        .text(s"Probability ratio text to sound (1 = equal) (default: ${default.textSoundRatio})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(textSoundRatio = v) }

      opt[Unit]("keep-temp-files")
        .text("Do not delete temp files")
        .action { (_, c) => c.copy(deleteTempFiles = false) }

      opt[Double]("max-sound-dur")
        .text(s"Maximum sound (field recordings etc.) duration in seconds (default: ${default.maxSoundDur})")
        .validate { v => if (v >= 1.0) success else failure("Must >= 1") }
        .action { (v, c) => c.copy(maxSoundDur = v) }

      opt[Double]("max-text-dur")
        .text(s"Maximum text (when cut) duration in seconds (default: ${default.maxTextDur})")
        .validate { v => if (v >= 1.0) success else failure("Must >= 1") }
        .action { (v, c) => c.copy(maxTextDur = v) }

//      opt[Unit]("backup")
//        .text("Backup workspace before modification")
//        .action { (_, c) => c.copy(backupWorkspace = true) }

      opt[Unit]('f', "force-seed")
        .text("Force re-seeding of RNG")
        .action { (_, c) => c.copy(forceSeed = true) }

//      opt[Int]('i', "iterations")
//        .text(s"Number of iterations to run (default: ${default.iterations})")
//        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
//        .action { (v, c) => c.copy(iterations = v) }

      opt[Double]("mask-noise-level")
        .text(s"Masking noise level in decibels (default: ${default.maskThreshNoiseDb})")
        .validate { v => if (v <= 0.0) success else failure("Must be <= 0") }
        .action { (v, c) => c.copy(maskThreshNoiseDb = v) }

      opt[Double]("mask-distance-level")
        .text(s"Masking foreground-background distance in decibels (default: ${default.maskThreshDistanceDb})")
        .validate { v => if (v <= 6.0) success else failure("Must be <= 6") }
        .action { (v, c) => c.copy(maskThreshDistanceDb = v) }

      opt[Double]("mask-blue-time")
        .text(s"Masking blurring time in seconds (default: ${default.maskBlurTime})")
        .validate { v => if (v > 0.0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(maskBlurTime = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config => run() }
  }

  def run()(implicit config: Config): Unit = {
    //    Mellite.initTypes()
    SoundProcesses.init()
    FScape        .init()

    val store = BerkeleyDB.factory(config.ws, createIfNecessary = true)
    val d = Workspace.Durable.empty(config.ws, store)
    type S = Durable
    implicit val _d: Workspace.Durable = d

    try {
      implicit val system: S = _d.system
      system.step { implicit tx =>
        implicit val u: proc.Universe[S] = proc.Universe[S]()
        Preparation[S](config)
      }
      d.close()

    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
    }
    sys.exit()
  }
}
