import Dependencies._

ThisBuild / organization     := "com.anglypascal"
ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.2-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "mustache",
    libraryDependencies += "org.specs2" %% "specs2-core" % "4.15.0" % "test",
    libraryDependencies ++= Seq(
      "com.rallyhealth" %% "weepickle-v1" % "1.7.2",
      "com.rallyhealth" %% "weeyaml-v1" % "1.7.2"
    ),
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5",
    resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    resolvers += "Typesafe Snapshots Repository" at 
      "https://repo.typesafe.com/typesafe/snapshots/",
    resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
  )

scalacOptions ++= Seq("-Yrangepos")
