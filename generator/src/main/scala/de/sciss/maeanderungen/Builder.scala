/*
 *  Builder.scala
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

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.{Folder, Obj, Sys}
import de.sciss.synth.proc.Implicits._

import scala.language.higherKinds
import scala.reflect.ClassTag

object Builder {
  def any2stringadd: Any = ()

  def mkFolder[S <: Sys[S]](parent: Folder[S], name: String)(implicit tx: S#Tx): Folder[S] =
    mkObj[S, Folder](parent, name, -1)(Folder[S])

  def mkObj[S <: Sys[S], R[~ <: Sys[~]] <: Obj[~]](parent: Folder[S], name: String, version: Int)
                                                  (create: => R[S])
                                                  (implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] = {
    val opt = parent.$[R](name)
    checkMkObj(opt, name, version)(create)(parent.remove)(parent.addLast)
  }

  private def checkMkObj[S <: Sys[S], R[~ <: Sys[~]] <: Obj[~]](opt: Option[R[S]], name: String, version: Int)
                                                               (create: => R[S])
                                                               (remove: Obj[S] => Unit)
                                                               (put: R[S] => Unit)
                                                               (implicit tx: S#Tx): R[S] = {

    def add(): R[S] = {
      val res = create
      res.name = name
      if (version >= 0) res.attr.put("version", IntObj.newConst(version))
      put(res)
      res
    }

    opt match {
      case Some(x) =>
        if (version < 0 || x.attr.$[IntObj]("version").exists(_.value >= version)) x
        else {
          remove(x)
          add()
        }

      case _ => add()
    }
  }

  def mkObjIn[S <: Sys[S], R[~ <: Sys[~]] <: Obj[~]](parent: Obj[S], key: String, version: Int)(create: => R[S])
                                                    (implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] = {
    val a = parent.attr
    val opt = a.$[R](key)
    checkMkObj(opt, key, version)(create)(_ => ())(a.put(key, _))
  }
}
