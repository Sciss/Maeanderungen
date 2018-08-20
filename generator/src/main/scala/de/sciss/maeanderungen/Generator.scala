package de.sciss.maeanderungen

import de.sciss.file.File

object Generator {

  /** @param pool   pointer to a Mellite workspace with materials to use
    * @param output pointer to target Mellite workspace to write
    */
  case class Config(
                   pool   : File,
                   output : File
                   )

  def run(config: Config): Unit = {

  }
}
