name         := "Maeanderungen"
version      := "0.1.0"
description  := "An algorithmic art project"
organization := "de.sciss"
homepage     := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses     := Seq("gpl v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))
scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss"          %  "neuralgas-core"  % "2.3.1",
  "de.sciss"          %% "fileutil"        % "1.1.3",
  "de.sciss"          %% "numbers"         % "0.1.3",
  "de.sciss"          %% "kollflitz"       % "0.2.1",
  "de.sciss"          %% "equal"           % "0.1.2",
  "de.sciss"          %% "topology"        % "1.1.0",
  "de.sciss"          %% "scalaaudiofile"  % "1.4.6",
  "de.sciss"          %% "fscape-core"     % "2.11.1",
  "de.sciss"          %% "travelling-ants" % "0.1.1",
  "com.github.scopt"  %% "scopt"           % "3.7.0"
)

mainClass in (Compile, run) := Some("de.sciss.maeanderungen.Cracks")
