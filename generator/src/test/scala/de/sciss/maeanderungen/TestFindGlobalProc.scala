package de.sciss.maeanderungen

import de.sciss.kollflitz.Vec
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.InMemory
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.Timeline

object TestFindGlobalProc extends App {
  type S = InMemory
  implicit val system: S = InMemory()

  system.step { implicit tx =>
    val tl      = Timeline[S]
    val local1  = IntObj.newConst[S](1)
    val local2  = IntObj.newConst[S](2)
    val global1 = IntObj.newConst[S](3)
    val global2 = IntObj.newConst[S](4)

    tl.add(Span.All, global1)
    tl.add(Span.All, global2)
    tl.add(Span(100, 200), local1)
    tl.add(Span(-100, 200000), local2)

    val res: Iterator[(SpanLike, Vec[BiGroup.Entry[S, Obj[S]]])] =
      tl.rangeSearch(Span.Until(BiGroup.MinCoordinate + 1), Span.From(BiGroup.MaxCoordinate - 1))

    val resL = res.toList
    assert(resL.size == 1)

    val vec = resL.head._2
    assert(vec.size == 2)

    val values = vec.collect {
      case BiGroup.Entry(_, i: IntObj[S]) => i.value
    } .sorted

    assert (values == List(3, 4))
  }
  println("Absolutely.")
}
