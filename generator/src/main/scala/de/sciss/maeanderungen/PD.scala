package de.sciss.maeanderungen

import de.sciss.lucre.stm.Sys

trait PD[S <: Sys[S]] {
  def sample()(implicit tx: S#Tx): Double
}
