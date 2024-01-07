import sbt._
import Keys._

object Dependencies {
  object Cats {
    val cats       = "org.typelevel" %% "cats-core"   % "2.10.0"
    val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.2"
  }
  val quicklens = "com.softwaremill.quicklens" %% "quicklens" % "1.9.6"
  val jgrapht   = "org.jgrapht"                % "jgrapht-core" % "1.5.2"

  val commonDependencies: Seq[ModuleID] = Seq(Cats.cats, Cats.catsEffect, quicklens, jgrapht)
  
}
