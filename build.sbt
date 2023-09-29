import Dependencies.*

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "hw9",
    libraryDependencies ++= Seq(
      catsEffect,
      scalaTest % Test
    )
  )
