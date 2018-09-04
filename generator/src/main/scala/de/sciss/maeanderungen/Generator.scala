/*
 *  Generator.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import de.sciss.file.File
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.{Durable, Workspace}

object Generator {
  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Preparation") {
      opt[File]('w', "workspace")
        .required()
        .text(s"Input Mellite workspace that contains 'material' folder.")
        .action { (v, c) => c.copy(ws = v) }

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

      opt[Unit]("no-prepare")
        .text("Do not run preparation stage")
        .action { (_, c) => c.copy(prepare = false) }

      opt[Unit]("no-render")
        .text("Do not run rendering stage")
        .action { (_, c) => c.copy(render = false) }

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
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config => run() }
  }

  def run()(implicit config: Config): Unit = {
    Mellite.initTypes()
    val store = BerkeleyDB.factory(config.ws, createIfNecessary = false)
    Workspace.read(config.ws, store) match {
      case d: Workspace.Durable =>
        type S = Durable
        implicit val _d: Workspace.Durable = d
        try {
          d.system.step { implicit tx =>
            if (config.prepare) Preparation .process[S]()
            if (config.render ) Layer       .process[S]()
          }
        } finally {
          d.close()
        }
        sys.exit()

      case other =>
        println(s"Cannot handle workspace $other")
        other.close()
        sys.exit(1)
    }
  }
}
