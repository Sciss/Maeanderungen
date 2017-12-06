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

import java.awt.geom.Line2D
import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}
import javax.imageio.ImageIO

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.numbers
import de.sciss.neuralgas.ComputeGNG.Result
import de.sciss.neuralgas.{Algorithm, ComputeGNG, EdgeGNG, ImagePD, LineFloat2D, NodeGNG, PointFloat2D}
import de.sciss.topology.Graph.EdgeMap
import de.sciss.topology.{EdgeView, Graph, Kruskal}

import scala.annotation.tailrec
import scala.collection.breakOut

object Cracks {
  private final val COOKIE = 0x474E4755 // 'GNGU'

  final val SCALE_DOWN = 8

  def main(args: Array[String]): Unit = {
    val crackIdx    = 2
    val projectDir  = file("")   / "data" / "projects"
    val dirIn       = projectDir / "Imperfect" / "cracks"
    val fImgIn      = dirIn      / "two_bw" / s"cracks${crackIdx}_19bw.png"
    val dirOut      = projectDir / "Maeanderungen" / "cracks"
    val fOutRaw     = dirOut     / s"cracks${crackIdx}_gng.bin"
    val fOutFuse    = dirOut     / s"cracks${crackIdx}_fuse.bin"

    if (fOutRaw.isFile && fOutRaw.length() > 0L) {
      println(s"'$fOutRaw' already exists. Not overwriting.")
    } else {
      calcGNG(fImg = fImgIn, fOut = fOutRaw)
    }

    if (fOutFuse.isFile && fOutFuse.length() > 0L) {
      println(s"'$fOutFuse' already exists. Not overwriting.")
    } else {
      calcFuse(fIn = fOutRaw, fOut = fOutFuse)
    }

    traverse(fGraphIn = fOutFuse, fImgIn = fImgIn)
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
    * @param progress   a thunk that is executed once per iteration.
    *                   there are `disjoint.size - 1` iterations.
    * @param distance   a function to calculate a distance between any two
    *                   vertices; this is currently assumed to be symmetric,
    *                   so `distance(a, b)` should yield the same result
    *                   as `distance(b, a)` for the method to work correctly.
    * @param ord        ordering of the values produced by the `distance` function.
    *                   This is used to determine the minimal distance among a set of distances.
    * @return the set of _additional edges_ required to fuse the graph.
    *         It is _not the total_ set of edges.
    */
  def fuse[V, E, A](disjoint: List[Iterable[E]], progress: => Unit = ())(distance: (V, V) => A)
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
        progress
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
    - make it a single connected graph
      by inserting edges between disconnected graph
    - read the neural gas graph
   */
  def calcFuse(fIn: File, fOut: File): Unit = {
    requireCanWrite(fOut)
    val compute = readGraph(fIn)

    implicit val edgeView: EdgeView[NodeGNG, EdgeGNG] = edgeViewGNG(compute)
    val part = partition(compute.edges.take(compute.nEdges), directed = false)
    val numParts = part.size
    println(s"Number of disjoint graphs: $numParts") // ; total num-vertices = ${part.map(_.size).sum}
    assert(part.map(_.size).sum === compute.nEdges)

    println("Fusing graphs... ")
    println("_" * 60)

    var partsDone = 0
    var lastProg  = 0
    val newEdges = fuse(part, {
      partsDone += 1
      val prog = partsDone * 60 / (numParts - 1)
      while (lastProg < prog) {
        print('#')
        lastProg += 1
      }
    })(euclideanSqr)

    println(" Done.")
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

  implicit final class PointOps(private val in: PointFloat2D) extends AnyVal {
    def immutable: Point2D = Point2D(in.x, in.y)
  }

  /*
    - calculate the minimum spanning tree
    - calculate the path from a random (?) point to another random (?) point
    - move along the path with a "perpendicular balancing pole"
    - the "pole" is trimmed by temporarily adding the walking position to the
      graph, calculating the overall voronoi, and intersecting with
      the voronoi region around the walking position.
    - collect the pixels along the "trimmed pole"
    - do something with them...
  */
  def traverse(fGraphIn: File, fImgIn: File): Unit = {
    val compute = readGraph(fGraphIn)

    val nodes0 = compute.nodes
    val unsorted = compute.edges.iterator.take(compute.nEdges).toVector
    val sorted = unsorted.sortBy { e =>
      val n1 = nodes0(e.from)
      val n2 = nodes0(e.to  )
      euclideanSqr(n1, n2)
    }


//    val test = sorted.map(e => (e.from, e.to))
//    assert(test === test.distinct)

    implicit object EV extends EdgeView[Int, EdgeGNG] {
      def sourceVertex(e: EdgeGNG): Int = e.from
      def targetVertex(e: EdgeGNG): Int = e.to
    }
    val mst0: Vector[EdgeGNG] = Kruskal[Int, EdgeGNG, Vector[EdgeGNG], Vector[EdgeGNG]](sorted)

//    val numIsolated = Graph.mkBiEdgeMap(unsorted).count(_._2.isEmpty)
//    println(s"numIsolated = $numIsolated")

    // XXX TODO --- I don't know why this assumption may fail.
    // (Perhaps we did forgot one partial-graph connection?)
    // E.g. we have a case with nNodes 8658 and mst.size 8652, an not duplicate edges or isolated vertices.
    // assert(mst.size === compute.nNodes - 1, s"mst.size ${mst.size} != compute.nNodes ${compute.nNodes}")

    // we now compress the nodes and edges in `compute` to contain
    // only those which participate in the MST

    val vertices0: Vector[Int] = Graph.mkVertexSeq(mst0)
    val vStart0 = vertices0.head
    val vEnd0   = vertices0.last

    val verticesS   = vertices0.sorted
    val vMap        = verticesS.iterator.zipWithIndex.toMap
    val mst         = mst0.iterator.zipWithIndex.map { case (eIn, i) =>
      val eOut    = new EdgeGNG
      eOut.from   = vMap(eIn.from)
      eOut.to     = vMap(eIn.to  )
      eOut.age    = eIn.age
      compute.edges(i) = eOut
      eOut
    } .toVector
    compute.nEdges = mst.size

    verticesS.iterator.zipWithIndex.foreach { case (ni, i) =>
      compute.nodes(i) = compute.nodes(ni)
    }
    compute.nNodes    = verticesS.size

    val edgeMap = Graph.mkBiEdgeMap(mst)
    val vStart  = vMap(vStart0)
    val vEnd    = vMap(vEnd0  )
    val path    = Graph.findUndirectedPath(vStart, vEnd, edgeMap)
    val pathSz  = path.size
    import compute.nodes
    val dist = math.sqrt(euclideanSqr(nodes(vStart), nodes(vEnd))).toFloat
    println(s"Path of length $pathSz from $vStart to $vEnd with distance $dist.")
//    println(path)

    var xMin = Float.MaxValue
    var xMax = Float.MinValue
    var yMin = Float.MaxValue
    var yMax = Float.MinValue
    vertices0.foreach { vi =>
      val n = nodes(vi)
      xMin = math.min(xMin, n.x)
      xMax = math.max(xMax, n.x)
      yMin = math.min(yMin, n.y)
      yMax = math.max(yMax, n.y)
    }
//    println(s"$xMin, $xMax, $yMin, $yMax")

    import kollflitz.Ops._

    val fImgOut1 = file("/data/temp/test1.png")
    if (!fImgOut1.exists()) {
      val imgTest = new BufferedImage((xMax * 2).toInt + 1, (yMax * 2).toInt + 1, BufferedImage.TYPE_INT_ARGB)
      val g = imgTest.createGraphics()
      g.setColor(Color.black)
      g.fillRect(0, 0, imgTest.getWidth, imgTest.getHeight)
      g.setColor(Color.white)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
      val ln = new Line2D.Float

      def drawLine(ni1: Int, ni2: Int): Unit = {
        val n1 = nodes(ni1)
        val n2 = nodes(ni2)
        ln.setLine(n1.x * 2, n1.y * 2, n2.x * 2, n2.y * 2)
        g.draw(ln)
      }

      mst.foreach { e =>
        drawLine(e.from, e.to)
      }

      g.setColor(Color.red)
      path.foreachPair(drawLine)

      g.dispose()
      ImageIO.write(imgTest, "png", fImgOut1)
    }

    val img     = ImageIO.read(fImgIn)
    val widthS  = img.getWidth .toFloat / SCALE_DOWN
    val heightS = img.getHeight.toFloat / SCALE_DOWN
    val vs      = new VoronoiSearch(compute, widthS.toInt, heightS.toInt)
    val box: List[LineFloat2D] = List(
      new LineFloat2D(    0f,      0f, widthS,      0f),
      new LineFloat2D(    0f, heightS, widthS, heightS),
      new LineFloat2D(    0f,      0f,     0f, heightS),
      new LineFloat2D(widthS,      0f, widthS, heightS),
    )
    val safeLen = widthS + heightS  // for artificial line segments in mkPole to sect the box

    def mkPole(nodeIdx: Int, ang: Double): Option[LineFloat2D] = {
      val n       = nodes(nodeIdx)
      val pt      = Point2D(n.x, n.y)
//      println(pt)
      val polyIt  = vs.perform(pt)
      if (polyIt.isEmpty) return None // XXX TODO --- yeah, there are some problems, I guess with minuscule cells and float res

      val dx      = math.cos(ang).toFloat * safeLen
      val dy      = math.sin(ang).toFloat * safeLen
      val nodeLn  = new LineFloat2D(n.x - dx, n.y - dy, n.x + dx, n.y + dy)
      // append to box, so we're sure we cut somewhere
      val inter   = (polyIt ++ box.iterator).flatMap(intersectLineLine(_, nodeLn))
      val pt1     = inter.next()
      val pt2     = inter.next()
      val res     = new LineFloat2D(pt1.x, pt1.y, pt2.x, pt2.y)
      Some(res)
    }

    val pixelStep = 1.0 / SCALE_DOWN
    val PiH = math.Pi / 2
    var poleLastProg = 0
    println("Tracing poles...")
    println("_" * 60)
    val poles: Iterator[LineFloat2D] = path.sliding(2).zipWithIndex.flatMap { case (Seq(ni1, ni2), pathIdx) =>
      val n1        = nodes(ni1)
      val n2        = nodes(ni2)
      val x1        = n1.x // * SCALE_DOWN
      val y1        = n1.y // * SCALE_DOWN
      val x2        = n2.x // * SCALE_DOWN
      val y2        = n2.y // * SCALE_DOWN
      val ang0      = math.atan2(y2 - y1, x2 - x1)
      val ang       = ang0 + PiH
      val dist      = math.sqrt(euclideanSqr(n1, n2))
      val numSteps  = (dist / pixelStep).toInt + 1
//      println(numSteps)
      val sub = (0 until numSteps).iterator.flatMap { step =>
//        println(s"step $step")
        val ni =
          if (step == 0) {
            ni1
          } else if (step == numSteps - 1) {
            compute.nNodes -= 1 // "pop"
            ni2
          } else {
            if (step == 1) {
              val n = new NodeGNG
              nodes(compute.nNodes) = n
              compute.nNodes += 1 // "push" an interpolated node
            }
            import numbers.Implicits._
            val res = compute.nNodes - 1
            val n = nodes(res)
            n.x = step.linlin(0, numSteps, n1.x, n2.x)
            n.y = step.linlin(0, numSteps, n1.y, n2.y)
            res
          }

        mkPole(ni, ang)
      }

      val prog = (pathIdx + 1) * 60 / (pathSz - 1)
      while (poleLastProg < prog) {
        print('#')
        poleLastProg += 1
      }

      sub
    }

    val fImgOut2 = file("/data/temp/test2.png")
    if (!fImgOut2.exists()) {
      val imgTest = new BufferedImage((xMax * 2).toInt + 1, (yMax * 2).toInt + 1, BufferedImage.TYPE_INT_ARGB)
      val g = imgTest.createGraphics()
      g.setColor(Color.black)
      g.fillRect(0, 0, imgTest.getWidth, imgTest.getHeight)
      g.setColor(Color.white)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
      val ln = new Line2D.Float

      def drawLine(l: LineFloat2D): Unit = {
        ln.setLine(l.x1 * 2, l.y1 * 2, l.x2 * 2, l.y2 * 2)
        g.draw(ln)
      }

      poles.foreach(drawLine)

      g.dispose()
      ImageIO.write(imgTest, "png", fImgOut2)
    }

    println()
  }

  def intersectLineLine(a: LineFloat2D, b: LineFloat2D, eps: Float = 1.0e-6f): Option[Point2D] =
    intersectLineLineF(
      a1x = a.x1, a1y = a.y1, a2x = a.x2, a2y = a.y2,
      b1x = b.x1, b1y = b.y1, b2x = b.x2, b2y = b.y2, eps = eps)

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
      require(cookie === COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${COOKIE.toHexString}")
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
      println(s"Done. ${compute.nNodes} vertices, ${compute.nEdges} edges.")
      compute

    } finally {
      sIn.close()
    }
  }

  def requireCanWrite(f: File): Unit =
  require(f.parentOption.exists(_.canWrite) && (!f.exists() || f.canWrite))

  def calcGNG(fImg: File, fOut: File): Unit = {
    requireCanWrite(fOut)

    val compute             = new ComputeGNG
    val img                 = ImageIO.read(fImg)
    val pd                  = new ImagePD(img, true)
    compute.pd              = pd
    compute.panelWidth      = img.getWidth  / SCALE_DOWN
    compute.panelHeight     = img.getHeight / SCALE_DOWN
    compute.maxNodes        = pd.getNumDots / 10
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

    println("Running GNG...")
    println(s"w ${compute.panelWidth}, h ${compute.panelHeight}, maxNodes ${compute.maxNodes}")
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
