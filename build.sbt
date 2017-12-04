name         := "Maeanderungen"
version      := "0.1.0-SNAPSHOT"
description  := "An algorithmic art project"
organization := "de.sciss"
homepage     := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses     := Seq("gpl v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))
scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss"          %  "neuralgas-core" % "2.2.1-SNAPSHOT",
  "de.sciss"          %% "fileutil"       % "1.1.3",
  "de.sciss"          %% "numbers"        % "0.1.3",
  "de.sciss"          %% "kollflitz"      % "0.2.1",
  "com.github.scopt"  %% "scopt"          % "3.7.0"
)

