/*
 *  PoirotTests.scala
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

object PoirotTests {
  def main(args: Array[String]): Unit =
    concatAllDiff(sounds1)

  case class Sound(id: String, minDur: Int, maxDur: Int)

  val sounds1: Vec[Sound] = Vec(
    Sound("alpha", minDur = 10, maxDur = 10),
    Sound("beta" , minDur =  6, maxDur =  6),
    Sound("gamma", minDur =  4, maxDur =  4)
  )

  def concatAll(sounds: Vec[Sound]): Unit = {
    import de.sciss.poirot._
    import Implicits._

    implicit val m: Model = Model()

    val startSq = sounds.map { s =>
      IntVar(s"start-${s.id}", 0, 1000)
    }
    val durSq = sounds.map { s =>
      IntVar(s"dur-${s.id}", s.minDur, s.maxDur)
    }

    val stopSq = (startSq zip durSq).map { case (start, dur) =>
      start + dur
    }

//    val totalDur = sum(durSq)
    val zero: IntVar = 0

    val numSounds = sounds.size

    for (i <- 0 until numSounds) {
      val start = startSq(i)
      val options = for {
        j <- 0 until numSounds // if i != j
      } yield {
        start #= (if (i == j) zero else stopSq(j))
      }
      OR(options: _*)
    }

    allDifferent(startSq: _*)
//    min(startSq: _*) #= zero

    val allVars = startSq ++ durSq

    val (result, stats) = withStatistics(satisfyAll(search(allVars, inputOrder, indomainMin)))

    println(s"RESULT: $result")
    println(stats)
  }

  def concatAllDiff(sounds: Vec[Sound]): Unit = {
    import de.sciss.poirot._
    import Implicits._

    implicit val m: Model = Model()

    val startSq = sounds.map { s =>
      IntVar(s"start-${s.id}", 0, 1000)
    }
    val durSq = sounds.map { s =>
      IntVar(s"dur-${s.id}", s.minDur, s.maxDur)
    }

    val stopSq = (startSq zip durSq).map { case (start, dur) =>
      start + dur
    }

    //    val totalDur = sum(durSq)
    val zero: IntVar = 0

    val numSounds = sounds.size

    for (i <- 0 until numSounds) {
      val start = startSq(i)
      val options = for {
        j <- 0 until numSounds // if i != j
      } yield {
        start #= (if (i == j) zero else stopSq(j))
      }
      OR(options: _*)
    }

    allDifferent(startSq: _*)
    //    min(startSq: _*) #= zero

    val allVars = startSq ++ durSq

    val (result, stats) = withStatistics(satisfyAll(search(allVars, inputOrder, indomainMin)))

    println(s"RESULT: $result")
    println(stats)
  }
}
