import sbt._
import sbt.Keys._

object FPBuild extends Build {

  lazy val proj = Project(
    id = "state",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "State in scala",
      organization := "keyel",
      version := "0.1",
      scalaVersion := "2.10.2"
      // add other settings here
    )
  )
}
