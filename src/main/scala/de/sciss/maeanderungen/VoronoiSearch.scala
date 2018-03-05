/*
 *  VoronoiSearch.scala
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

import de.sciss.neuralgas.{ComputeGNG, EdgeVoronoi, LineFloat2D, Voronoi}

final class VoronoiSearch(compute: ComputeGNG, w: Int, h: Int) extends Voronoi(compute) {
  voronoiB = true
  setSize(w, h)

  private[this] var _array  = new Array[LineFloat2D](4)
  private[this] var _numLn  = 0
  private[this] var _pt     = Point2D(0f, 0f)

  def perform(pt: Point2D): Iterator[LineFloat2D] = {
    _pt               = pt
    _numLn            = 0
    compute.maxNodes  = compute.nNodes // needed for voronoi
    val error = computeVoronoi()
    if (error) Iterator.empty else _array.iterator.take(_numLn)
  }

  private def push(ln: LineFloat2D): Unit = {
    val i = _numLn
    val a0 = _array
    val a = if (i < a0.length) a0 else {
      val a1 = new Array[LineFloat2D](i << 1)
      System.arraycopy(a0, 0, a1, 0, i)
      _array = a1
      a1
    }
    a(i) = ln
    _numLn = i + 1
  }

  override def out_ep(e: EdgeVoronoi): Boolean = {
    val hasLine = super.out_ep(e)
    if (hasLine) {
      val s1 = e.reg(0).coord
      val s2 = e.reg(1).coord
      val pt = _pt
      if ((s1.x == pt.x && s1.y == pt.y) || (s2.x == pt.x && s2.y == pt.y)) {
        val ln = lines(nLines - 1)
        push(ln)
      }
    }
    hasLine
  }
}