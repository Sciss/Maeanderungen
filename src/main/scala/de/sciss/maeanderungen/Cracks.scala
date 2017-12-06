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

import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.neuralgas.ComputeGNG.Result
import de.sciss.neuralgas.{Algorithm, ComputeGNG, EdgeGNG, ImagePD, NodeGNG}
import de.sciss.topology.Graph.EdgeMap
import de.sciss.topology.{EdgeView, Graph, Kruskal}

import scala.annotation.tailrec
import scala.collection.breakOut

object Cracks {
  private final val COOKIE = 0x474E4755 // 'GNGU'

  def main(args: Array[String]): Unit = {
    val crackIdx    = 2
    val projectDir  = file("")   / "data" / "projects"
    val dirIn       = projectDir / "Imperfect" / "cracks"
    val fIn         = dirIn      / "two_bw" / s"cracks${crackIdx}_19bw.png"
    val dirOut      = projectDir / "Maeanderungen" / "cracks"
    val fOut1       = dirOut     / s"cracks${crackIdx}_gng.bin"
    val fOut2       = dirOut     / s"cracks${crackIdx}_fuse.bin"

    if (fOut1.isFile && fOut1.length() > 0L) {
      println(s"'$fOut1' already exists. Not overwriting.")
    } else {
      calcGNG(fIn = fIn, fOut = fOut1)
    }

    if (fOut2.isFile && fOut2.length() > 0L) {
      println(s"'$fOut2' already exists. Not overwriting.")
    } else {
      calcFuse(fIn = fOut1, fOut = fOut2)
    }
  }

  /** Partition a graph into a list of disjoint graphs.
    * Each graph in the result, represented by a set of edges,
    * is guaranteed to have all vertices connected, i.e. you
    * can reach each vertex for any other vertex.
    */
  def partition[V, E](edges: Iterable[E], directed: Boolean)
                     (implicit edgeView: EdgeView[V, E]): List[Set[E]] = {
    import edgeView._

    val resMaps = edges.foldLeft(List.empty[EdgeMap[V, E]]) { (res, e) =>
      val start = sourceVertex(e)
      val end   = targetVertex(e)
      val in    = res.filter { map =>
        map.contains(start) || map.contains(end)
      }
      val out   = if (in.isEmpty) res else res.diff(in)
      val n0    = Map(start -> Set(e))
      val n1    = if (directed) n0 else n0 + (end -> Set(e))
      val inNew = in.fold(n1) { (m1, m2) =>
        val vertices = m1.keySet ++ m2.keySet
        vertices.map { v =>
          val edges = m1.getOrElse(v, Set.empty[E]) ++ m2.getOrElse(v, Set.empty[E])
          v -> edges
        } (breakOut)
      }

      inNew :: out
    }

    resMaps.map(_.valuesIterator.flatten.toSet)
  }

  /** Fuses multiple graphs by brute search for closest edges.
    * (This is not optimized for performance!)
    *
    * @param  distance  a function to calculate a distance between any two
    *                   vertices; this is currently assumed to be symmetric,
    *                   so `distance(a, b)` should yield the same result
    *                   as `distance(b, a)` for the method to work correctly.
    * @param  ord       ordering of the values produced by the `distance` function.
    *                   This is used to determine the minimal distance among a set of distances.
    * @return the set of _additional edges_ required to fuse the graph.
    *         It is _not the total_ set of edges.
    */
  def fuse[V, E, A](disjoint: List[Iterable[E]])(distance: (V, V) => A)
                   (implicit edgeView: EdgeView[V, E], ord: Ordering[A]): Set[(V, V)] = {
    @tailrec
    def loop(rem: List[Set[V]], res: Set[(V, V)]): Set[(V, V)] = rem match {
      case Nil        => res
      case _ :: Nil   => res
      case head :: tail =>
        val candH: Iterator[(V, V, A, Int)] = head.iterator.map { start =>
          val candT: Iterator[(V, A)] = tail.iterator.map { graph =>
            graph.iterator.map(end => end -> distance(start, end)).minBy(_._2)
          }
          val (end, dist, tailIdx) = candT.zipWithIndex.map { case ((v, a), i) => (v, a, i) } .minBy(_._2)
          (start, end, dist, tailIdx)
        }
        val (start, end, _ /* d */, tailIdx) = candH.minBy(_._3)
        val target  = tail(tailIdx)
        val headNew = head ++ target
        val tailNew = tail.patch(tailIdx, Nil, 1)
        // XXX TODO --- yeah, well, we'll calculate a lot of the same distances again
        // unless we introduce a cache...
        val remNew  = headNew :: tailNew
//        println(s"${rem.size} - d = $d")
        loop(remNew, res + (start -> end))
    }

    val disjointV: List[Set[V]] = disjoint.map(Graph.mkVertexSet[V, E])
    loop(disjointV, Set.empty)
  }

  def edgeViewGNG(compute: ComputeGNG): EdgeView[NodeGNG, EdgeGNG] = new EdgeView[NodeGNG, EdgeGNG] {
    def sourceVertex(e: EdgeGNG): NodeGNG = compute.nodes(e.from)
    def targetVertex(e: EdgeGNG): NodeGNG = compute.nodes(e.to  )
  }

  def euclideanSqr(n1: NodeGNG, n2: NodeGNG): Float = {
    val x1 = n1.x
    val y1 = n1.y
    val x2 = n2.x
    val y2 = n2.y
    val dx = x1 - x2
    val dy = y1 - y2
    dx*dx + dy*dy
  }

  /*
      - read the neural gas graph
      - make it a single connected graph
        by inserting edges between disconnected graph
      - calculate the minimum spanning tree
      - calculate the path from a random (?) point to another random (?) point
      - move along the path with a "perpendicular balancing pole"
      - the "pole" is trimmed by temporarily adding the walking position to the
        graph, calculating the overall voronoi, and intersecting with
        the voronoi region around the walking position.
      - collect the pixels along the "trimmed pole"
      - do something with them...

   */
  def calcFuse(fIn: File, fOut: File): Unit = {
    val compute = readGraph(fIn)

    implicit val edgeView: EdgeView[NodeGNG, EdgeGNG] = edgeViewGNG(compute)
    val part = partition(compute.edges.take(compute.nEdges), directed = false)
    println(s"Number of disjoint graphs: ${part.size}") // ; total num-vertices = ${part.map(_.size).sum}
    assert(part.map(_.size).sum == compute.nEdges)

    print("Fusing graphs... ")
    val newEdges = fuse(part)(euclideanSqr)
    println("Done.")
//    println(s"We've got to insert ${newEdges.size} edges.")
    newEdges.foreach { case (start, end) =>
      val e   = new EdgeGNG
      e.from  = compute.nodes.indexOf(start)
      e.to    = compute.nodes.indexOf(end  )
      e.age   = -1
      compute.edges(compute.nEdges) = e
      compute.nEdges += 1
    }

    writeGraph(compute, fOut = fOut)
  }

  def schoko(fIn: File): Unit = {
    val compute = readGraph(fIn)

    val nodes  = compute.nodes
    val sorted = compute.edges.iterator.take(compute.nEdges).toVector.sortBy { e =>
      val n1 = nodes(e.from)
      val n2 = nodes(e.to  )
      euclideanSqr(n1, n2)
    }

    implicit object EV extends EdgeView[Int, EdgeGNG] {
      def sourceVertex(e: EdgeGNG): Int = e.from
      def targetVertex(e: EdgeGNG): Int = e.to
    }
    val mst = Kruskal[Int, EdgeGNG, Vector[EdgeGNG], Vector[EdgeGNG]](sorted)
    println(s"MST size = ${mst.size}")
  }

  def intersectLineLineF(a1x: Float, a1y: Float, a2x: Float, a2y: Float,
                         b1x: Float, b1y: Float, b2x: Float, b2y: Float, eps: Float = 1.0e-6f): Option[Point2D] = {
    val dax   = a2x - a1x
    val day   = a2y - a1y
    val dbx   = b2x - b1x
    val dby   = b2y - b1y
    val dx1   = a1x - b1x
    val dy1   = a1y - b1y
    val ua_t  = dbx*dy1 - dby*dx1
    val ub_t  = dax*dy1 - day*dx1
    val u_b   = dby*dax - dbx*day

    if (u_b != 0) {
      val ua = ua_t / u_b
      val ub = ub_t / u_b
      val min = -eps
      val max = 1 + eps

      if (min <= ua && ua <= max && min <= ub && ub <= max) {
        val ix = a1x + ua * dax
        val iy = a1y + ua * day
        Some(Point2D(ix, iy))
      } else {
        None
      }
    } else {
      None
    }
  }

  def mkCompute(): ComputeGNG = {
    val compute       = new ComputeGNG
    compute.algorithm = Algorithm.GNGU
    compute.GNG_U_B   = true
    compute
  }

  def readGraph(f: File): ComputeGNG = {
    print(s"Reading graph from '$f'... ")
    val sIn = new FileInputStream(f)
    try {
      val dIn = new DataInputStream(sIn)
      import dIn._
      val cookie = readInt()
      require(cookie == COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${COOKIE.toHexString}")
      val compute     = new ComputeGNG
      val nNodes      = readInt()
      compute.nNodes  = nNodes
      var i = 0
      val nodes = compute.nodes
      while (i < nNodes) {
        val n     = new NodeGNG
        n.x       = readFloat()
        n.y       = readFloat()
        n.error   = readFloat()
        n.utility = readFloat()
        nodes(i)  = n
        i += 1
      }
      val nEdges      = readInt()
      compute.nEdges  = nEdges
      i = 0
      val edges = compute.edges
      while (i < nEdges) {
        val e     = new EdgeGNG
        e.from    = readInt()
        e.to      = readInt()
        e.age     = readShort()
        edges(i)  = e
        i += 1
      }
      compute.maxNodes = compute.nNodes // needed for voronoi
      println("Done. ${compute.nNodes} vertices, ${compute.nEdges} edges.")
      compute

    } finally {
      sIn.close()
    }
  }

  def calcGNG(fIn: File, fOut: File): Unit = {
    val compute             = new ComputeGNG
    val img                 = ImageIO.read(fIn)
    val pd                  = new ImagePD(img, true)
    compute.pd              = pd
    compute.panelWidth      = img.getWidth  / 8
    compute.panelHeight     = img.getHeight / 8
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

    writeGraph(compute, fOut)
  }

  def writeGraph(compute: ComputeGNG, fOut: File): Unit = {
    print(s"Writing '$fOut'... ")

    val sOut = new FileOutputStream(fOut)
    try {
      val dOut = new DataOutputStream(sOut)
      import dOut._
      writeInt(COOKIE)
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
