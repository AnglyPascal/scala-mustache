import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.2-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "mustache",
    libraryDependencies += scalaTest % Test,
    /* libraryDependencies += "junit" % "junit" % "4.8.1" % "test->default", */
    /* libraryDependencies += "org.specs2" %% "specs2-core" % "5.0.7" % "test", */
    /* libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3" % "test->default", */
    resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    resolvers += "Typesafe Snapshots Repository" at "https://repo.typesafe.com/typesafe/snapshots/"
  )
