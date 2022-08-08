val scala2Version = "2.13.8"
val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .aggregate(scalus.js, scalus.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val scalus = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "scalus",
    version := "0.1-SNAPSHOT"
  )
  .jvmSettings(
    // Add JVM-specific settings here
    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version,
    // To cross compile with Scala 3 and Scala 2
    crossScalaVersions := Seq(scala3Version, scala2Version),
    libraryDependencies ++= Seq(
      "io.bullet" %% "borer-core" % "1.8.0",
      "io.bullet" %% "borer-derivation" % "1.8.0"
    ),
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0" % "test"
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    scalaJSUseMainModuleInitializer := true
  )
