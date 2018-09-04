/*
 *  Category.scala
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

import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm.{Random, TxnLike}

object Category {
  sealed trait Text extends Category {
    final val isText = true

//    def defaultIntelligible : Boolean
//    def defaultSequential   : Boolean
//    def defaultComplete     : Boolean
  }

  case object GettingLost extends Text {
    val name                = "Sich verlieren"
    val abbrev              = "SV"
    val defaultIntelligible = true
    val defaultSequential   = true
    val defaultComplete     = true
  }

  case object CountingTrees extends Text {
    val name                = "Bäume zählen"
    val abbrev              = "BZ"
    val defaultIntelligible = false
    val defaultSequential   = false
    val defaultComplete     = false
  }

  case object SkyObservations extends Text {
    val name                = "Himmelsbeobachtungen"
    val abbrev              = "HB"
    val defaultIntelligible = false
    val defaultSequential   = false
    val defaultComplete     = false
  }

  case object ChaoticEnumerations extends Text {
    val name                = "Chaotische Numerierungen"
    val abbrev              = "CN"
    val defaultIntelligible = true
    val defaultSequential   = true
    val defaultComplete     = true
  }

  case object Acrostics extends Text {
    val name                = "Akrosticha"
    val abbrev              = "AK"
    val defaultIntelligible = true
    val defaultSequential   = true
    val defaultComplete     = true
  }

  /*

  case object BannersInPublicSpace  extends Text
  case object CreeksAndPaths        extends Text
  case object Measurements          extends Text
  case object PowerOfHydrogen       extends Text

   */

  case object PoeticManuals extends Text {
    val name                = "Poetische Bedienungsanleitungen"
    val abbrev              = "PB"
    val defaultIntelligible = true
    val defaultSequential   = true
    val defaultComplete     = true
  }

  case object MetaText extends Text {
    val name                = "Metatext"
    val abbrev              = "MT"
    val defaultIntelligible = false
    val defaultSequential   = false
    val defaultComplete     = false
  }

  val text: Vec[Text] = Vec(
    GettingLost, CountingTrees, SkyObservations, ChaoticEnumerations, Acrostics, PoeticManuals, MetaText
  )

  val all: Vec[Category] = text // XXX TODO

  val abbrevMap: Map[String, Category] = all.iterator.map { c => c.abbrev -> c } .toMap

  private def normalizeWeights(tup: Vec[(Double, Category)]): Vec[(Double, Category)] = {
    val sum = tup.iterator.map(_._1).sum
    require (sum > 0.0)
    if (sum == 1.0) tup else tup.map {
      case (w, c) => (w / sum) -> c
    }
  }

  val weighted: Vec[(Double, Category)] = normalizeWeights(Vec(
    0.2 -> GettingLost,
    0.1 -> CountingTrees,
    0.1 -> SkyObservations,
    0.1 -> ChaoticEnumerations,
    0.1 -> Acrostics,
    0.1 -> PoeticManuals,
    0.3 -> MetaText
  ))

  def choose[Tx <: TxnLike]()(implicit tx: Tx, r: Random[Tx]): Category = {
    import Ops._
    weighted.chooseWeighted(_._1)._2
  }
}
sealed trait Category {
  def name    : String
  def abbrev  : String
  def isText  : Boolean

  def defaultIntelligible : Boolean
  def defaultSequential   : Boolean
  def defaultComplete     : Boolean
}