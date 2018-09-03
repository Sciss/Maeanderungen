lazy val baseName   = "Maeanderungen"
lazy val baseNameL  = baseName.toLowerCase

lazy val commonSettings = Seq(
  name         := "Maeanderungen",
  version      := "0.2.0-SNAPSHOT",
  description  := "An algorithmic art project",
  organization := "de.sciss",
  homepage     := Some(url(s"https://github.com/Sciss/${name.value}")),
  licenses     := Seq("agpl v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint"),
  updateOptions := updateOptions.value.withLatestSnapshots(false)
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
      "de.sciss"          %% "fscape-core"      % "2.17.0",
      "de.sciss"          %  "jrpicam"          % "0.2.0",
      "com.pi4j"          %  "pi4j-core"        % "1.1",
      "com.github.scopt"  %% "scopt"            % "3.7.0"
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
      "de.sciss"          %% "fscape-core"      % "2.17.0",
      "de.sciss"          %% "travelling-ants"  % "0.1.1",
      "de.sciss"          %  "jrpicam"          % "0.2.0",
//      "de.sciss"          %% "poirot"           % "0.3.0",
      "com.pi4j"          %  "pi4j-core"        % "1.1",
      "com.github.scopt"  %% "scopt"            % "3.7.0"
    ),
    mainClass in (Compile, run) := Some("de.sciss.maeanderungen.Cracks")
  )

lazy val generator = project.withId(s"$baseNameL-generator").in(file("generator"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-Generator",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "mellite"          % "2.27.0",
      "de.sciss"          %% "soundprocesses"   % "3.21.1",
      "de.sciss"          %% "fscape"           % "2.17.0",
      "de.sciss"          %% "travelling-ants"  % "0.1.1",
      "de.sciss"          %% "poirot"           % "0.3.0",
      "com.github.scopt"  %% "scopt"            % "3.7.0"
    ),
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in run := true,
    mainClass in (Compile, run) := Some("de.sciss.maeanderungen.Generator")
  )
