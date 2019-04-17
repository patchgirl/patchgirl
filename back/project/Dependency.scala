import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object Dependency {

  val apiTest = Seq(
    "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.1" % Test
  )

  val api = Seq(
    ws,
    caffeine,
  )
}
