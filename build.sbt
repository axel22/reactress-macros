


organization := "org.reactress"

version := "0.0.1"

scalaVersion := "2.10.0-RC3"

scalacOptions += ""

libraryDependencies <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-reflect" % sv
}

libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC3" % "1.8-B1" 




