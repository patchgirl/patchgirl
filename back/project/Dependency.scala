import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object Dependency {

  val db = Seq(
    "org.playframework.anorm" %% "anorm"     % "2.6.2",
    "org.postgresql"          % "postgresql" % "42.2.5",
    jdbc,
    evolutions,
  )

  val apiTest = Seq(
    "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.1" % Test
  )

  val api = Seq(
    ws,
    caffeine,
    guice
  )
}
