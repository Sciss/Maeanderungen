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

import de.sciss.file.{File, file}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.{Durable, Workspace}

object Generator {
  case class Config(ws      : File    = file("material.mllt"),
                    minDur  : Double  = 60.0 * 4,
                    maxDur  : Double  = 60.0 * 8,
                    radio   : Boolean = false,
                    prepare : Boolean = true,
                   )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Preparation") {
      opt[File]('w', "workspace")
        .required()
        .text(s"Input Mellite workspace that contains 'material' folder.")
        .action { (v, c) => c.copy(ws = v) }

      opt[Double]("min-dur")
        .text(s"Minimum duration in seconds (default: ${default.minDur})")
        .action { (v, c) => c.copy(minDur = v) }

      opt[Double]("max-dur")
        .text(s"Maximum duration in seconds (default: ${default.maxDur})")
        .action { (v, c) => c.copy(maxDur = v) }

      opt[Unit]('r', "radio")
        .text("Version for radio instead of installation")
        .action { (_, c) => c.copy(radio = true) }

      opt[Unit]("no-prepare")
        .text("Do not run preparation stage")
        .action { (_, c) => c.copy(prepare = false) }
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
            if (config.prepare) Preparation.process[S]()
            Layer.process[S]()
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
