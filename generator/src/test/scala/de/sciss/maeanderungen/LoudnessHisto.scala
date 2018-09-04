package de.sciss.maeanderungen

import de.sciss.file._
import de.sciss.kollflitz.Ops._

object LoudnessHisto extends App {
  val names = List("PB-1_HH_T155", "CN_1_RH_T014", "MT-18_NC_T139", "MT-5_MR_T103")
  names.foreach { name =>
    val f1      = file(s"/data/projects/Maeanderungen/analysis/$name-loud.aif")
    val thresh  = 10
    val loud0   = Util.readAudioFileData1D(f1)
    val loud    = loud0.filter(_ >= thresh)
    val min     = 10.0 // loud.min.floor.roundTo  (5.0)
    val max     = 85.0 // loud.max.ceil .roundUpTo(5.0)
    val histo   = Util.mkHistogram(loud, numBins = 50, min = min, max = max)
    val loudS   = loud.sortedT
    println(s"For $name: max count = ${histo.max} at ${histo.indexOf(histo.max)}, " +
      f"median = ${loudS.median}%g, 80%% percentile = ${loudS.percentile(80)}%g, " +
      f"95%% percentile = ${loudS.percentile(95)}%g")
    val title   = s"${f1.name} - loudness histogram"
    val c       = Util.mkHistogramChart(histo, xMin = min, xMax = max, title = title)
    c.show(title)
  }

  /*

  For PB-1_HH_T155: max count = 47 at 36, median = 61.4477, 80% percentile = 65.9932, 95% percentile = 68.9777
  For CN_1_RH_T014: max count = 94 at 32, median = 58.1655, 80% percentile = 62.9621, 95% percentile = 65.5314
  For MT-18_NC_T139: max count = 210 at 38, median = 66.9812, 80% percentile = 69.9700, 95% percentile = 72.0916
  For MT-5_MR_T103: max count = 175 at 36, median = 62.4720, 80% percentile = 65.4158, 95% percentile = 68.0004

          HH = 69.0, RH = 65.5, NC = 72.1; MR = 68.0
  gains =      -3.5,       0.0,      -6.6,      -2.5

  "haut hin"

  in other words: filter loudness values >= 10, calculate the 95% percentile, subtract that from 65 (for example)

   */
}
