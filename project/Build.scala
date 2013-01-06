import sbt._
import Keys._
import Process._
import java.io.File



object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := "org.reactress",
    version := "0.0.1",
    scalaVersion := "2.10.0-RC3",
    libraryDependencies <+= scalaVersion { sv =>
      "org.scala-lang" % "scala-compiler" % sv
    },
    libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC3" % "1.8-B1",
    scalacOptions += "",
    scalacOptions in Test <+= (Keys.`package` in Compile) map { artifact =>
      "-Xplugin:" + artifact
    }
  )
}


object ReactressBuild extends Build {
  import BuildSettings._

  /* projects */

  lazy val reactress = Project(
    "reactress",
    file("."),
    settings = buildSettings
  ) dependsOn (
  )

}