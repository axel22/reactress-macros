import sbt._
import Keys._
import Process._
import java.io.File



object ReactressBuild extends Build {
  val macroSettings = Defaults.defaultSettings ++ Seq (
    organization := "org.reactress",
    version := "0.0.1",
    scalaVersion := "2.10.1",
    libraryDependencies <+= scalaVersion { sv =>
      "org.scala-lang" % "scala-compiler" % sv
    },
    scalacOptions += ""
  )

  lazy val reactressMacros = Project(
    "reactress-macros",
    file("macro"),
    settings = macroSettings
  )

  val reactressSettings = Defaults.defaultSettings ++ Seq (
    organization := "org.reactress",
    version := "0.0.1",
    scalaVersion := "2.10.1",
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1",
    scalacOptions <+= (Keys.`package` in (reactressMacros, Compile)) map { artifact =>
      "-Xplugin:" + artifact
    }
  )

  lazy val reactress = Project(
    "reactress",
    file("."),
    settings = reactressSettings
  ) dependsOn(reactressMacros)

}