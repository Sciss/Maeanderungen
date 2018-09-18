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

import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{BooleanObj, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Action, AudioCue, GenContext, SoundProcesses, Timeline, Workspace}

import scala.concurrent.{Future, Promise}
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object Builder {
  def any2stringadd: Any = ()

  def mkFolder[S <: Sys[S]](parent: Folder[S], name: String)(implicit tx: S#Tx): Folder[S] =
    mkObj[S, Folder](parent, name, -1)(Folder[S])

  def mkObj[S <: Sys[S], R[~ <: stm.Sys[~]] <: Obj[~]](parent: Folder[S], name: String, version: Int)
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
        if (version < 0 || x.attr.$[IntObj]("version").exists(_.value >= version)) {
          x
        } else {
          remove(x)
          add()
        }

      case _ => add()
    }
  }

  def mkObjIn[S <: Sys[S], R[~ <: stm.Sys[~]] <: Obj[~]](parent: Obj[S], key: String, version: Int)(create: => R[S])
                                                    (implicit tx: S#Tx, ct: ClassTag[R[S]]): R[S] = {
    val a = parent.attr
    val opt = a.$[R](key)
    checkMkObj(opt, key, version)(create)(_ => ())(a.put(key, _))
  }

  def createMetaData[S <: Sys[S]](cue: AudioCue.Obj[S])(implicit tx: S#Tx, workspace: Workspace[S]): Future[Unit] = {
    import workspace.root
    implicit val cursor: Cursor[S] = workspace.cursor

    val cueName   = cue.value.artifact.base
    println(s"Preparing $cueName...")
    val hasPauses = cue.attr.$[Timeline]("pauses").isDefined
    val self      = root.![Folder]("analysis").![proc.Action]("find-pauses")

    val fsc       = self.attr.![FScape]("fsc")
    val loc       = root.![ArtifactLocation]("base")
    val aFsc      = fsc.attr
    aFsc.put("in", cue)
    val dirAna    = loc.directory / "analysis"
    dirAna.mkdirs()
    val isMale    = cueName.contains("_HH")
    val fLoud     = dirAna / s"$cueName-loud.aif"
    val fPitch    = dirAna / s"$cueName-pitch.aif"
    val fPauses   = dirAna / s"$cueName-pauses.aif"
    val artLoud   = Artifact(loc, fLoud   )
    val artPitch  = Artifact(loc, fPitch  )
    val artPauses = Artifact(loc, fPauses )
    aFsc.put("loud"   , artLoud   )
    aFsc.put("pitch"  , artPitch  )
    aFsc.put("pauses" , artPauses )
    aFsc.put("is-male", BooleanObj.newConst(isMale))

    val res = Promise[Unit]()

    def done()(implicit tx: S#Tx): Unit = {
      val tr = Try {
        val actDone   = self.attr.![Action]("done")
        val uDone     = Action.Universe(actDone, workspace, invoker = Some(fsc))
        actDone.execute(uDone)
      }
      tx.afterCommit(res.complete(tr))
    }

    if (hasPauses) {
      done()
    } else {
      implicit val gen: GenContext[S] = GenContext[S]
      val r = fsc.run()
      r.reactNow { implicit tx => state =>
        if (state.isComplete) {
          val tr = r.result.get
          tr match {
            case Success(_) =>
              done()

            case Failure(ex) =>
              tx.afterCommit(res.tryFailure(ex))
          }
        }
      }
    }

    res.future
  }

  def atomic[S <: Sys[S], A](body: S#Tx => A)(implicit cursor: stm.Cursor[S]): A =
    cursor.step(tx => body(tx))

  def flatMapTx[S <: Sys[S], A, B](fut: Future[A])(body: S#Tx => A => Future[B])
                                  (implicit cursor: stm.Cursor[S]): Future[B] = {
    import SoundProcesses.executionContext
    fut.flatMap { a =>
      atomic[S, Future[B]] { implicit tx => body(tx)(a) }
    }
  }

  def mapTx[S <: Sys[S], A, B](fut: Future[A])(body: S#Tx => A => B)
                              (implicit cursor: stm.Cursor[S]): Future[B] = {
    import SoundProcesses.executionContext
    fut.map { a =>
      atomic[S, B] { implicit tx => body(tx)(a) }
    }
  }

}
