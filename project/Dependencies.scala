import sbt._
import Keys._

object Dependencies {
  object Cats {
    val cats       = "org.typelevel" %% "cats-core"   % "2.10.0"
    val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.2"
  }

  val commonDependencies: Seq[ModuleID] = Seq(Cats.cats, Cats.catsEffect)
  
}
