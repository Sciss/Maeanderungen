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
  case class Config(ws: File = file("material.mllt"))

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Preparation") {
      opt[File]('w', "workspace")
        .required()
        .text(s"Input Mellite workspace that contains 'material' folder.")
        .action { (v, c) => c.copy(ws = v) }
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
            Preparation.process[S]()
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
