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

object Generator {

  /** @param pool   pointer to a Mellite workspace with materials to use
    * @param output pointer to target Mellite workspace to write
    */
  case class Config(
                   pool     : File,
                   output   : File,
                   duration : PD,
                   )

  def run(config: Config): Unit = {

  }
}
