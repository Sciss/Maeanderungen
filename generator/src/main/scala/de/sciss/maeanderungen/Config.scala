/*
 *  Config.scala
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

case class Config(ws                : File    = file("material.mllt"),
                  minDur            : Double  = 60.0 * 4,
                  maxDur            : Double  = 60.0 * 8,
                  radio             : Boolean = false,
                  prepare           : Boolean = true,
                  render            : Boolean = true,
                  probTransformText : Double  = 0.5,
                  probTransformSound: Double  = 0.5,
                  probShorten       : Double  = 0.5,
                  probShortenFade   : Double  = 0.25,
                  numChannels       : Int     = 2,
                  seed              : Option[Long] = None,
                  maxPan            : Double  = 0.5,
                  textSoundRatio    : Double  = 1.0
                 )
