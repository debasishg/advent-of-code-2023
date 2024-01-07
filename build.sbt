val scala3Version = "3.3.1"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := scala3Version
ThisBuild / version := "0.1.0"
ThisBuild / scalafmtOnCompile := true

run / fork := true
run / connectInput := true
run / javaOptions += "-Dfile.encoding=UTF-8"
outputStrategy := Some(StdoutOutput)

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"scalaz3_3-4.8.14.jar"
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "Year 2023",
    dependencies
  )

lazy val dependencies =
  libraryDependencies ++= Dependencies.commonDependencies