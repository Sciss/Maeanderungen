package de.sciss.maeanderungen

import java.awt.image.DataBuffer
import java.util.Locale

import de.sciss.file._
import de.sciss.fscape.graph.ImageFile
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import javax.imageio.ImageIO

import scala.swing.Swing

object CracksSynthesis {
  def any2stringadd: Nothing = sys.error("no way")

  final case class Config(
                         imageInF         : File    = file("image.jpg"),
                         polesInF         : File    = file("poles.aif"),
                         audioOutF        : File    = file("out.aif"),
                         sampleRate       : Int     = 44100,
                         audioBreadthStep : Int     = 2,
                         maxDur           : Double  = 30.0
                         )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("CracksSynthesis") {
//      opt[Long] ('r', "random-seed")
//        .text (s"Seed value for RNG (default: ${default.rndSeed})")
//        .action { (v, c) => c.copy(rndSeed = v) }

      opt[Int] ('w', "audio-breadth")
        .text (s"Pole-to-audio breadth pixel decimation factor (default: ${default.audioBreadthStep})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(audioBreadthStep = v) }

      opt[Double] ('l', "max-dur")
        .text (s"Audio output maximum duration in seconds (default: ${default.maxDur})")
        .validate (v => if (v > 0) success else failure("Must be > 0 and <= 1"))
        .action { (v, c) => c.copy(maxDur = v) }

      //      opt[String] ('m', "decimation")
      //        .text (s"Type of audio decimation (one of ${Decimation.names.mkString(", ")}; default: ${default.decimation.name})")
      //        .validate (v => Try(Decimation(v))
      //          .fold(_ => failure(s"one of ${Decimation.names.mkString(", ")}"), _ => success))
      //        .action { (v, c) => c.copy(decimation = Decimation(v)) }

      opt[File] ('i', "image")
        .required()
        .text (s"Image input file")
        .action { (v, c) => c.copy(imageInF = v) }

      opt[File] ('p', "poles")
        .required()
        .text (s"Poles input file")
        .action { (v, c) => c.copy(polesInF = v) }

      opt[File] ('o', "output")
        .required()
        .text (s"Audio output file")
        .action { (v, c) => c.copy(audioOutF = v) }

      opt[Int] ('r', "sample-rate")
        .text (s"Audio output nominal sample rate (default: ${default.sampleRate})")
        .validate (v => if (v >= 1) success else failure("Must be >= 1"))
        .action { (v, c) => c.copy(sampleRate = v) }

      //      opt[File] ('d', "project-dir")
//        .text (s"Projects based directory (default: ${default.projectDir})")
//        .action { (v, c) => c.copy(projectDir = v) }
//
//      opt[File] ('t', "temp-dir")
//        .text (s"Directory for temporary files and illustrations (default: ${default.tempDir})")
//        .action { (v, c) => c.copy(tempDir = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def readImageSpec(in: File): ImageFile.Spec = {
    val iis = ImageIO.createImageInputStream(in)
    val r   = ImageIO.getImageReaders(iis).next()
    r.setInput(iis)
    val w    = r.getWidth(0)
    val h    = r.getHeight(0)
    val raw  = r.getRawImageType(0)
    val spec = if (raw != null) raw else r.getImageTypes(0).next()
    val ch   = spec.getNumComponents
    val smp  = spec.getSampleModel.getDataType match {
      case DataBuffer.TYPE_BYTE  => ImageFile.SampleFormat.Int8
      case DataBuffer.TYPE_SHORT | DataBuffer.TYPE_USHORT => ImageFile.SampleFormat.Int16
      case DataBuffer.TYPE_FLOAT => ImageFile.SampleFormat.Float
    }
    val fmt  = r.getFormatName
    val tpe  = fmt.toLowerCase(Locale.US) match {
      case "jpeg" | "jpg" => ImageFile.Type.JPG
      case "png"          => ImageFile.Type.PNG
      case _ => sys.error(s"Unsupported image format '$fmt'")
    }
    ImageFile.Spec(tpe, smp, width = w, height = h, numChannels = ch)
  }

  def run(config: Config): Unit = {
    import config._
    // coordinates in polesInF are already up-sampled to original image dimension
//    val polesInF          = file("/data/projects/Maeanderungen/cracks/cracks2_poles.aif")
//    val imageInF          = file("/data/projects/Imperfect/cracks/two_gray/cracks2_19.jpg")
//    val audioOutF         = file("/data/temp/test.aif")
    val specImgIn         = readImageSpec(imageInF)
    val imgWidth          = specImgIn.width
    val imgHeight         = specImgIn.height
    val imgCh             = specImgIn.numChannels
//    val audioBreadthStep  = 2

    ImageFile

    val polesSpec         = AudioFile.readSpec(polesInF)
    println(s"num-poles = ${polesSpec.numFrames}")

    val g = Graph {
      import graph._
      val poles     = AudioFileIn(polesInF, numChannels = 6)
      val imgIn     = ImageFileIn(imageInF, numChannels = imgCh)
      val x1        = poles out 2
      val y1        = poles out 3
      val x2        = poles out 4
      val y2        = poles out 5
      val dx        = x2 - x1
      val dy        = y2 - y1
      val len       = (dx.squared + dy.squared).sqrt
      val pixelStep = 1.0 / audioBreadthStep
      val numSteps0 = (len / pixelStep).floor
      val numSteps1 = numSteps0 + (1: GE) - (numSteps0 % 2) // odd
      val numSteps  = numSteps1 + (1: GE) // even
//      RepeatWindow(Frames(numSteps)).poll(2, "num-steps")

      val BUF = sampleRate
      val numStepsM = BufferMemory(numSteps, BUF)

      val x         = DEnvGen(levels = x1 zip x2, lengths = numSteps1 zip DC(1))
      val y         = DEnvGen(levels = y1 zip y2, lengths = numSteps1 zip DC(1))
//      Plot1D(x, size = 4000, "x")
//      Plot1D(y, size = 4000, "y")

      val STATIC_STEP = true // false
      val DC_EARLY    = true
      val USE_IFFT    = false

      def dcBlock(in: GE): GE =
        Biquad(in, b0 = 1, b1 = -1, a1 = -0.99) // dc-block: y(n) = x(n) - x(n-1) + 0.99 * y(n-1)

//      val x         = RepeatWindow(x1.elastic(2), num = len) // XXX TODO
//      val y         = RepeatWindow(y1.elastic(2), num = len) // XXX TODO
      val scan0     = ScanImage(imgIn, width = imgWidth, height = imgHeight, x = x, y = y, zeroCrossings = 0)
      val scan      = -scan0 + (1.0: GE)
      val step0     = numStepsM.sqrt
      val step      = if (STATIC_STEP)
        WhiteNoise(512) + (512: GE)
      else
        (step0 + WhiteNoise(4)).max(1)

      val scanHPF   = if (DC_EARLY) dcBlock(scan) else scan
      val fftSize   = 1024 // BufferDisk(numStepsM)
//      RepeatWindow(fftSize).poll(2, "fft-size")
//      val fftSize   = 32768
      val scanSpec0 = if (USE_IFFT) {
//        Real1IFFT(scanHPF, size = fftSize, mode = 0)
        DCT_II(BufferDisk(scanHPF), size = fftSize, numCoeffs = fftSize)
      } else scanHPF
//      val framesOut = Frames(scanSpec \ 0)
//      ((framesOut - 1) / 44100).poll(44100, "spec [s]")
      val scanSpec  = BufferDisk(scanSpec0)
      val stepM     = BufferDisk(step)
      val lap0      = OverlapAdd(scanSpec, size = numStepsM, step = stepM)
      val lap       = lap0.take((sampleRate * maxDur).toLong)
      val hpf       = dcBlock(lap)
      val framesOut = Frames(hpf out 0)
      ((framesOut - 1) / sampleRate).poll(sampleRate, "spec [s]")
      val max       = RunningMax(hpf).last
      val disk      = BufferDisk(hpf)
      val sigOut    = disk / max
      /* val framesOut = */ AudioFileOut(file = audioOutF, spec = AudioFileSpec(numChannels = imgCh, sampleRate = sampleRate), in = sigOut)
//      ((framesOut - 1) / 44100).poll(44100, "out [s]")
//      x.poll(1000, "x")
//      y.poll(1000, "y")
//      lap.poll(1000, "scan")
    }

    val fCfg = stream.Control.Config()
    fCfg.useAsync = false
    val ctrl = stream.Control(fCfg)

    ctrl.run(g)

    Swing.onEDT {
      SimpleGUI(ctrl)
    }

    println("Running.")
  }
}
