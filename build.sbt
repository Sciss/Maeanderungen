lazy val baseName   = "Maeanderungen"
lazy val baseNameL  = baseName.toLowerCase

lazy val commonSettings = Seq(
  name         := "Maeanderungen",
  version      := "0.4.0-SNAPSHOT",
  description  := "An algorithmic art project",
  organization := "de.sciss",
  homepage     := Some(url(s"https://github.com/Sciss/${name.value}")),
  licenses     := Seq("agpl v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint"),
  updateOptions := updateOptions.value.withLatestSnapshots(false),
  resolvers    += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat
)

lazy val root = project.in(file("."))
  .aggregate(cam, cracks, generator)
  .settings(commonSettings)

lazy val cam = project.withId(s"$baseNameL-cam").in(file("cam"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-Cam",
    mainClass       in assembly := Some("de.sciss.maeanderungen.Cam"),
    assemblyJarName in assembly := "Cam.jar",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "fileutil"         % "1.1.3",
      "de.sciss"          %% "numbers"          % "0.2.0",
      "de.sciss"          %% "kollflitz"        % "0.2.2",
      "de.sciss"          %% "equal"            % "0.1.2",
      "de.sciss"          %% "fscape-core"      % deps.main.fscape,
      "de.sciss"          %  "jrpicam"          % "0.2.0",
      "com.pi4j"          %  "pi4j-core"        % "1.1",
      "com.github.scopt"  %% "scopt"            % deps.main.scopt
    )
  )

lazy val cracks = project.withId(s"$baseNameL-cracks").in(file("cracks"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-Cracks",
    libraryDependencies ++= Seq(
      "de.sciss"          %  "neuralgas-core"   % "2.3.1",
      "de.sciss"          %% "fileutil"         % "1.1.3",
      "de.sciss"          %% "numbers"          % "0.2.0",
      "de.sciss"          %% "kollflitz"        % "0.2.2",
      "de.sciss"          %% "equal"            % "0.1.2",
      "de.sciss"          %% "topology"         % "1.1.0",
      "de.sciss"          %% "fscape-core"      % deps.main.fscape,
      "de.sciss"          %% "travelling-ants"  % deps.main.travellingAnts,
      "de.sciss"          %  "jrpicam"          % "0.2.0",
//      "de.sciss"          %% "poirot"           % deps.main.poirot,
      "com.pi4j"          %  "pi4j-core"        % "1.1",
      "com.github.scopt"  %% "scopt"            % deps.main.scopt
    ),
    mainClass in (Compile, run) := Some("de.sciss.maeanderungen.Cracks")
  )

lazy val generator = project.withId(s"$baseNameL-generator").in(file("generator"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-Generator",
    mainClass       in assembly := Some("de.sciss.maeanderungen.Generator"),
    assemblyJarName in assembly := "Generator.jar",
    target          in assembly := baseDirectory.value,
    libraryDependencies ++= Seq(
      "de.sciss"          %% "mellite"          % deps.main.mellite,
      "de.sciss"          %% "soundprocesses"   % deps.main.soundProcesses,
      "de.sciss"          %% "fscape"           % deps.main.fscape,
      "de.sciss"          %% "travelling-ants"  % deps.main.travellingAnts,
      "de.sciss"          %% "poirot"           % deps.main.poirot,
      "com.github.scopt"  %% "scopt"            % deps.main.scopt
    ),
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in run := true,
    mainClass in (Compile, run) := Some("de.sciss.maeanderungen.Generator")
  )

lazy val deps = new {
  val main = new {
    val fscape         = "2.17.3"
    val mellite        = "2.27.2"
    val poirot         = "0.3.0"
    val scopt          = "3.7.0"
    val soundProcesses = "3.21.3"
    val travellingAnts = "0.1.1"
  }
}
