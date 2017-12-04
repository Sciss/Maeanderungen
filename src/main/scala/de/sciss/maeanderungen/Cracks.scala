/*
 *  Cracks.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.maeanderungen

import java.io.{DataOutputStream, FileOutputStream}
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.neuralgas.ComputeGNG.Result
import de.sciss.neuralgas.{Algorithm, ComputeGNG, ImagePD}

object Cracks {
  def main(args: Array[String]): Unit = {
    val projectDir          = file("")   / "data" / "projects"
    val dirIn               = projectDir / "Imperfect" / "cracks"
    val fIn                 = dirIn      / "two_bw" / "cracks2_19bw.png"
    val dirOut              = projectDir / "Maeanderungen" / "cracks"
    val fOut                = dirOut     / "cracks2_gng.bin"

    if (fOut.isFile && fOut.length() > 0L) {
      println(s"'$fOut' already exists. Not overwriting.")
    } else {
      calcGNG(fIn = fIn, fOut = fOut)
    }
  }

  def calcGNG(fIn: File, fOut: File): Unit = {
    val compute             = new ComputeGNG
    val img                 = ImageIO.read(fIn)
    val pd                  = new ImagePD(img, true)
    compute.pd              = pd
    compute.panelWidth  = img.getWidth  / 8
    compute.panelHeight = img.getHeight / 8
    compute.maxNodes        = 10000; // pd.getNumDots / 8
    println(s"w ${compute.panelWidth}, h ${compute.panelHeight}, maxNodes ${compute.maxNodes}")
    compute.stepSize        = 400
    compute.algorithm       = Algorithm.GNGU
    compute.lambdaGNG       = 400
    compute.maxEdgeAge      = 88
    compute.epsilonGNG      = 0.1f // 0.05f
    compute.epsilonGNG2     = 6.0e-4f
    compute.alphaGNG        = 0.5f
    compute.setBetaGNG(5.0e-6f) // (1.5e-5f) // 5.0e-4f // 1.5e-5f
    compute.noNewNodesGNGB  = false
    compute.GNG_U_B         = true
    compute.utilityGNG      = 17f // 16f
    compute.autoStopB       = false
    compute.reset()
    compute.getRNG.setSeed(0L)
    compute.addNode(null)
    compute.addNode(null)

    require(fOut.parentOption.exists(_.canWrite) && (!fOut.exists() || fOut.canWrite))

    val res             = new Result
    var lastNum         = 0
    var iter            = 0
    val t0              = System.currentTimeMillis()
    println("_" * 60)
    while (!res.stop && compute.nNodes < compute.maxNodes) {
      compute.learn(res)
      val p = compute.nNodes * 60 / compute.maxNodes
      while (lastNum < p) {
        print('#')
        lastNum += 1
      }
      iter += 1
    }

    println()
    println(s"GNG took ${(System.currentTimeMillis() - t0)/1000} seconds, and $iter iterations, ${compute.numSignals} signals.")
    // println(compute.nodes.take(compute.nNodes).mkString("\n"))  }
    println(s"Writing '$fOut'...")

    val sOut = new FileOutputStream(fOut)
    try {
      val dOut = new DataOutputStream(sOut)
      import dOut._
      val nNodes = compute.nNodes
      writeInt(nNodes)
      var i = 0
      val nodes = compute.nodes
      while (i < nNodes) {
        val n = nodes(i)
        writeFloat(n.x)
        writeFloat(n.y)
        writeFloat(n.error)
        writeFloat(n.utility)
        i += 1
      }
      val nEdges = compute.nEdges
      writeInt(nEdges)
      val edges = compute.edges
      i = 0
      while (i < nEdges) {
        val e = edges(i)
        writeInt  (e.from       )
        writeInt  (e.to         )
        writeShort(e.age.toShort)
        i += 1
      }

    } finally {
      sOut.close()
    }

    println("Done.")
  }
}
