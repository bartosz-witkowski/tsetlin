import Dependencies._

ThisBuild / scalaVersion     := "3.1.0"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "net.like-a-boss"
ThisBuild / organizationName := "tsetlin"

lazy val core = (project in file("core")).settings(
    name := "tsetlin",
    libraryDependencies ++= List(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"))

lazy val tests = (project in file("tests")).settings(name := "tests").dependsOn(core)

/*
Tracing options for HotSpot
ThisBuild / fork := true
ThisBuild / run / javaOptions += "--add-modules=jdk.incubator.vector"
ThisBuild / run / javaOptions += "-XX:+UnlockDiagnosticVMOptions"
ThisBuild / run / javaOptions += "-XX:+PrintAssembly"
*/

lazy val root = (project in file("."))
  .aggregate(core, tests).settings(
    Compile / run := (core / Compile / run).evaluated)

