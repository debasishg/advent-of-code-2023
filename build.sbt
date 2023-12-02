val scala3Version = "3.3.1"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := scala3Version
ThisBuild / version := "0.1.0"
ThisBuild / scalafmtOnCompile := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "Year 2023",
    dependencies
  )

lazy val dependencies =
  libraryDependencies ++= Dependencies.commonDependencies