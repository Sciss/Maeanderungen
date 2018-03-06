/*
 *  Cracks.scala
 *  (Mäanderungen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.topology.Graph.EdgeMap
import de.sciss.topology.{EdgeView, Graph, Kruskal}

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, breakOut}
import scala.util.Try

object Cracks {
  final val GRAPH_COOKIE  = 0x474E4755 // 'GNGU'
  final val POLE_COOKIE   = 0x506F6C65 // 'Pole'

  object Decimation {
    val names: Seq[String] = Seq(None.name, Variance.name)

    def apply(s: String): Decimation = s match {
      case None     .name => None
      case Variance .name => Variance
    }

    case object None      extends Decimation
    case object Variance  extends Decimation
  }
  sealed trait Decimation extends Product {
    final val name: String = productPrefix.toLowerCase
  }

  final case class Config(crackId         : Int         = 19,
                          blackAndWhite   : Boolean     = true,
                          startNode       : Int         = -1,
                          endNode         : Int         = -1,
                          gngBeta         : Float       = 5.0e-6f,
                          gngUtility      : Float       = 17f,
                          rndSeed         : Long        = 0L,
                          imgScaleDown    : Int         = 8,
                          maxNodesDown    : Int         = 10,
                          poleStep        : Int         = 16,
                          audioBreadthStep: Int         = 2,
                          audioTimePower  : Float       = 0.5f,
                          decimation      : Decimation  = Decimation.None,
                          projectDir      : File        = defaultProjectDir,
                          tempDir         : File        = defaultTempDir
                         )

  def defaultProjectDir: File = {
    val a = file("") / "data"       / "projects"
    val b = userHome / "Documents"  / "projects"
    if (a.isDirectory) a else b
  }

  def defaultTempDir: File = {
    val a = file("") / "data"       / "temp"
    val b = userHome / "Documents"  / "temp"
    if (a.isDirectory) a else b
  }

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Cracks") {
      opt[Int] ("crack")
        .text (s"Crack image id (1 to 19; default: ${default.crackId})")
        .action { (v, c) => c.copy(crackId = v) }

      opt[Boolean] ("bw")
        .text (s"Use black and white image for pole tracing (default: ${default.blackAndWhite})")
        .action { (v, c) => c.copy(blackAndWhite = v) }

      opt[Int] ("start-node")
        .text (s"Start node id of path traversal (-1 for first; default: ${default.startNode})")
        .action { (v, c) => c.copy(startNode = v) }

      opt[Int] ("end-node")
        .text (s"End node id of path traversal (-1 for last; default: ${default.endNode})")
        .action { (v, c) => c.copy(endNode = v) }

      opt[Double] ("gng-utility")
        .text (s"GNG utility parameter (default: ${default.gngUtility})")
        .action { (v, c) => c.copy(gngUtility = v.toFloat) }

      opt[Double] ("gng-beta")
        .text (s"GNG betga parameter (default: ${default.gngBeta})")
        .action { (v, c) => c.copy(gngBeta = v.toFloat) }

      opt[Long] ("random-seed")
        .text (s"Seed value for RNG (default: ${default.rndSeed})")
        .action { (v, c) => c.copy(rndSeed = v) }

      opt[Int] ("image-scale")
        .text (s"Image scale-down factor (default: ${default.imgScaleDown})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(imgScaleDown = v) }

      opt[Int] ("nodes-scale")
        .text (s"Number of nodes scale-down factor from black pixels (default: ${default.maxNodesDown})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(maxNodesDown = v) }

      opt[Int] ("pole-step")
        .text (s"Pole tracing pixel decimation factor (default: ${default.poleStep})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(poleStep = v) }

      opt[Int] ("audio-breadth")
        .text (s"Pole-to-audio breadth pixel decimation factor (default: ${default.audioBreadthStep})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(audioBreadthStep = v) }

      opt[Double] ("audio-time")
        .text (s"Audio scan time step exponent (default: ${default.audioTimePower})")
        .validate (v => if (v > 0 && v <= 1) success else failure("Must be > 0 and <= 1"))
        .action { (v, c) => c.copy(audioTimePower = v.toFloat) }

      opt[String] ("decimation")
        .text (s"Type of audio decimation (one of ${Decimation.names.mkString(", ")}; default: ${default.decimation.name})")
        .validate (v => Try(Decimation(v))
          .fold(_ => failure(s"one of ${Decimation.names.mkString(", ")}"), _ => success))
        .action { (v, c) => c.copy(decimation = Decimation(v)) }

      opt[File] ("project-dir")
        .text (s"Projects based directory (default: ${default.projectDir})")
        .action { (v, c) => c.copy(projectDir = v) }

      opt[File] ("temp-dir")
        .text (s"Directory for temporary files and illustrations (default: ${default.tempDir})")
        .action { (v, c) => c.copy(tempDir = v) }
    }
    p.parse(args, default).fold(sys.exit(1)){ implicit config => run() }
  }

  def mkFImgIn(projectDir: File, crackId: Int, blackAndWhite: Boolean): File = {
    val dirIn       = projectDir / "Imperfect" / "cracks"
    val crackIdx    = 2
    val fImgInBW    = dirIn      / "two_bw"   / s"cracks${crackIdx}_${crackId}bw.png"
    val fImgInGray  = dirIn      / "two_gray" / s"cracks${crackIdx}_${crackId}.jpg"
    val fImgIn      = if (blackAndWhite) fImgInBW else fImgInGray
    fImgIn
  }

  def run()(implicit config: Config): Unit = {
    import config._
    val crackIdx    = 2
    require(projectDir.isDirectory)

    val fImgIn1     = {
      val dirIn       = projectDir / "Imperfect" / "cracks"
      val fImgInBW    = dirIn      / "two_bw"   / s"cracks${crackIdx}_${crackId}bw.png"
      val fImgInGray  = dirIn      / "two_gray" / s"cracks${crackIdx}_${crackId}.jpg"
      if (blackAndWhite) fImgInBW else fImgInGray
    }
    val dirOut      = projectDir / "Maeanderungen" / "cracks"

    dirOut.mkdirs()

    val fOutRaw     = dirOut     / s"cracks${crackIdx}_gng.bin"
    val fOutFuse    = dirOut     / s"cracks${crackIdx}_fuse.bin"
    val fOutPoles   = dirOut     / s"cracks${crackIdx}_poles_${startNode}_${endNode}.aif"
//    val fOutAudio   = dirOut     / s"cracks${crackIdx}_out_${startNode}_${endNode}.aif"

    def withFileOut(f: File)(fun: File => Unit): Unit =
      if (f.isFile && f.length() > 0L) {
        println(s"'$f' already exists. Not overwriting.")
      } else {
        fun(f)
      }

    withFileOut(fOutRaw  )(f => calcGNG  (fImg      = fImgIn1                        , fOut = f))
    withFileOut(fOutFuse )(f => calcFuse (fIn       = fOutRaw                        , fOut = f))
    withFileOut(fOutPoles)(f => calcPoles(fGraphIn  = fOutFuse , fImgIn = fImgIn1    , fOut = f))
//    withFileOut(fOutAudio)(f => calcAudio(fPolesIn  = fOutPoles, fImgIn = fImgInGray, fOut = f))
//    withFileOut(fOutAudio)(f => calcAudio(fPolesIn  = fOutPoles, fImgIn = if (blackAndWhite) fImgInBW else fImgInGray, fOut = f))
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
  def calcFuse(fIn: File, fOut: File)/* (implicit config: Config) */: Unit = {
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

    TODO:

    - there are poles that still escape to box boundary; perhaps
      just a matter of increasing `eps`.
    - there are a lot of irregularities at the break points,
      no sure if this is an effect of the "missing" elastic node
      in the voronoi? perhaps just skip the break points?

  */
  def calcPoles(fGraphIn: File, fImgIn: File, fOut: File)(implicit config: Config): Unit = {
    import config._

    requireCanWrite(fOut)
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
    val numVertices = vertices0.size
    val startNode1  = if (startNode == -1) 0               else startNode
    val endNode1    = if (endNode   == -1) numVertices - 1 else endNode
    if (startNode1 < 0 || startNode1 >= vertices0.size) throw new IllegalArgumentException(s"startNode: 0 <= $startNode1 < $numVertices")
    if (endNode1   < 0 || endNode1   >= vertices0.size) throw new IllegalArgumentException(s"endNode  : 0 <= $endNode1   < $numVertices")
    val vStart0     = vertices0(startNode1)
    val vEnd0       = vertices0(endNode1  )

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

    val fImgOut1 = tempDir / "test_mst.png"
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
    val widthS  = img.getWidth .toFloat / imgScaleDown
    val heightS = img.getHeight.toFloat / imgScaleDown
    val vs      = new VoronoiSearch(compute, widthS.toInt, heightS.toInt)

//    val box: List[LineFloat2D] = List(
//      new LineFloat2D(    0f,      0f, widthS,      0f),
//      new LineFloat2D(    0f, heightS, widthS, heightS),
//      new LineFloat2D(    0f,      0f,     0f, heightS),
//      new LineFloat2D(widthS,      0f, widthS, heightS),
//    )

    val safeLen = widthS + heightS  // for artificial line segments in mkPole to sect the box

    def mkPole(nodeIdx: Int, ang: Double): Option[(Point2D, LineFloat2D)] = {
      val n       = nodes(nodeIdx)
      val pt      = Point2D(n.x, n.y)
//      println(pt)
      val polyIt  = vs.perform(pt)
      if (polyIt.isEmpty) return None // XXX TODO --- yeah, there are some problems, I guess with minuscule cells and float res

      val dx      = math.cos(ang).toFloat * safeLen
      val dy      = math.sin(ang).toFloat * safeLen
      val nodeLn  = new LineFloat2D(n.x - dx, n.y - dy, n.x + dx, n.y + dy)
      // append to box, so we're sure we cut somewhere -- NOT
      val inter   = polyIt /* ++ box.iterator */ .flatMap(intersectLineLine(_, nodeLn))
      if (inter.hasNext) {
        val pt1   = inter.next()
        if (inter.hasNext) {
          val pt2 = inter.next()
          val ln  = new LineFloat2D(pt1.x, pt1.y, pt2.x, pt2.y)
          val res = (pt, ln)
          Some(res)
        } else None
      } else None
    }

    val pixelStep = (1.0 / poleStep) / imgScaleDown
    import math.Pi
    val PiH = Pi / 2
    val Pi2 = Pi * 2
    var poleLastProg = 0
    println("Tracing poles...")
    println("_" * 60)

    val pathLenAng = path.mapPairs { (ni1, ni2) =>
      // assert(ni1 < compute.nNodes && ni2 < compute.nNodes)
      val n1        = nodes(ni1)
      val n2        = nodes(ni2)
      val x1        = n1.x // * SCALE_DOWN
      val y1        = n1.y // * SCALE_DOWN
      val x2        = n2.x // * SCALE_DOWN
      val y2        = n2.y // * SCALE_DOWN
      val ang0      = math.atan2(y2 - y1, x2 - x1)
      val ang       = ang0 + PiH
      val len       = math.sqrt(euclideanSqr(n1, n2))
      len -> ang
    }

    val pathLenAng2 = pathLenAng.head +: pathLenAng :+ pathLenAng.last
    val pathIt = path.sliding(2).zip(pathLenAng2.sliding(3)).zipWithIndex

    import numbers.Implicits._

//    def debugDegree(d: Double): String = ((d.toDegrees + 360) % 360).roundTo(0.1).toFloat.toString

    val poles0: Iterator[(Point2D, LineFloat2D)] =
      pathIt.flatMap { case ((Seq(ni1, ni2), Seq((l1, _a1), (l2, a2), (l3, _a3))), pathIdx) =>
        val n1        = nodes(ni1)
        val n2        = nodes(ni2)
        val numSteps  = math.round(l2 / pixelStep).toInt + 1
        // assert(l2 == math.sqrt(euclideanSqr(n1, n2)), s"for $pathIdx: $l2 != ${math.sqrt(euclideanSqr(n1, n2))}")

        val m1 =  math.min(l1, l2) / (2 * l2)
        val m2 = -math.min(l2, l3) / (2 * l2) + 1.0
        val a1 = if ((_a1 absdif a2) <= Pi) _a1 else if (_a1 < a2) _a1 + Pi2 else _a1 - Pi2
        val a3 = if ((_a3 absdif a2) <= Pi) _a3 else if (_a3 < a2) _a3 + Pi2 else _a3 - Pi2

//        println(s"pathIdx $pathIdx,  m1 ${m1.toFloat}, m2 ${m2.toFloat}, a1 ${debugDegree(a1)}, a2 ${debugDegree(a2)}, a3 ${debugDegree(a3)}")

        val sub = (0 until numSteps).iterator.flatMap { step =>
          val xi: Float = step.linlin(0, numSteps, n1.x, n2.x)
          val yi: Float = step.linlin(0, numSteps, n1.y, n2.y)
          val dx = xi - n1.x
          val dy = yi - n1.y
          val d0 = math.sqrt(dx*dx + dy*dy)
          val w  = d0 / l2
          assert(w >= 0 && w <= 1, s"pathIdx = $pathIdx, step = $step, w = $w")

          val ang = if (w < m1) {
            val res = w.linlin(-m1, m1, a1, a2)
//            println(s"  w ${w.toFloat} - ${debugDegree(res)} < m1")
            res
          } else if (w > m2) {
            val res = w.linlin(m2, 1.0 + (1.0 - m2), a2, a3)
//            println(s"  w ${w.toFloat} - ${debugDegree(res)} > m2")
            res
          } else {
//            println(s"  w ${w.toFloat} - ${debugDegree(a2)} = a2")
            a2
          }

          if (step == 0) {
            mkPole(ni1, ang)
          } else {
            val n = new NodeGNG
            val ni = compute.nNodes
            nodes(ni) = n
            compute.nNodes += 1 // "push" an interpolated node
            n.x = xi
            n.y = yi
            val res = mkPole(ni, ang)
            compute.nNodes -= 1
            res
          }
        }

        val prog = (pathIdx + 1) * 60 / (pathSz - 1)
        while (poleLastProg < prog) {
          print('#')
          poleLastProg += 1
        }

        sub
      }

    val fImgOut2 = tempDir / "test_poles.png"
    val poles: Iterator[(Point2D, LineFloat2D)] = if (fImgOut2.exists()) poles0 else
      new AbstractIterator[(Point2D, LineFloat2D)] {
        def hasNext: Boolean = poles0.hasNext

        private lazy val imgTest  = new BufferedImage((xMax * 2).toInt + 1, (yMax * 2).toInt + 1, BufferedImage.TYPE_INT_ARGB)
        private lazy val g        = {
          val _g = imgTest.createGraphics()
          _g.setColor(Color.black)
          _g.fillRect(0, 0, imgTest.getWidth, imgTest.getHeight)
          _g.setColor(Color.white)
          _g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
          _g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
          _g
        }
        private val ln = new Line2D.Float

        def drawLine(l: LineFloat2D): Unit = {
          ln.setLine(l.x1 * 2, l.y1 * 2, l.x2 * 2, l.y2 * 2)
          g.draw(ln)
        }

        def next(): (Point2D, LineFloat2D) = {
          val res = poles0.next()
          drawLine(res._2)
          if (!hasNext) {
            g.dispose()
            ImageIO.write(imgTest, "png", fImgOut2)
          }
          res
        }
      }

    val afOut = AudioFile.openWrite(fOut, AudioFileSpec(numChannels = 6, sampleRate = 44100))
    try {
      val bufLen  = 1024
      val buf     = afOut.buffer(bufLen)
      var bufOff  = 0

      def flush(): Unit = if (bufOff > 0) {
        afOut.write(buf, 0, bufOff)
        bufOff = 0
      }

      poles.foreach { case (pt, ln) =>
        buf(0)(bufOff) = pt.x
        buf(1)(bufOff) = pt.y
        buf(2)(bufOff) = ln.x1
        buf(3)(bufOff) = ln.y1
        buf(4)(bufOff) = ln.x2
        buf(5)(bufOff) = ln.y2
        bufOff += 1
        if (bufOff == bufLen) flush()
      }

      flush()

    } finally {
      afOut.close()
    }

    println()
  }

  def intersectLineLine(a: LineFloat2D, b: LineFloat2D, eps: Float = 1.0e-5f): Option[Point2D] =
    intersectLineLineF(
      a1x = a.x1, a1y = a.y1, a2x = a.x2, a2y = a.y2,
      b1x = b.x1, b1y = b.y1, b2x = b.x2, b2y = b.y2, eps = eps)

  def intersectLineLineF(a1x: Float, a1y: Float, a2x: Float, a2y: Float,
                         b1x: Float, b1y: Float, b2x: Float, b2y: Float, eps: Float = 1.0e-5f): Option[Point2D] = {
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
      require(cookie === GRAPH_COOKIE, s"Unexpected cookie ${cookie.toHexString} -- expected ${GRAPH_COOKIE.toHexString}")
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

  def calcGNG(fImg: File, fOut: File)(implicit config: Config): Unit = {
    import config._

    requireCanWrite(fOut)

    val compute             = new ComputeGNG
    val img                 = ImageIO.read(fImg)
    val pd                  = new ImagePD(img, true)
    compute.pd              = pd
    compute.panelWidth      = img.getWidth  / imgScaleDown
    compute.panelHeight     = img.getHeight / imgScaleDown
    compute.maxNodes        = pd.getNumDots / maxNodesDown
    compute.stepSize        = 400
    compute.algorithm       = Algorithm.GNGU
    compute.lambdaGNG       = 400
    compute.maxEdgeAge      = 88
    compute.epsilonGNG      = 0.1f // 0.05f
    compute.epsilonGNG2     = 6.0e-4f
    compute.alphaGNG        = 0.5f
    compute.setBetaGNG(gngBeta) // 5.0e-6f  (1.5e-5f) // 5.0e-4f // 1.5e-5f
    compute.noNewNodesGNGB  = false
    compute.GNG_U_B         = true
    compute.utilityGNG      = gngUtility // 17f // 16f
    compute.autoStopB       = false
    compute.reset()
    compute.getRNG.setSeed(rndSeed)
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
      writeInt(GRAPH_COOKIE)
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
