val scala3Version = "3.2.1"
ThisBuild / scalaVersion := scala3Version

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
    version := "0.1-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions += "-Xcheck-macros",
    scalacOptions += "-Yretain-trees",
    libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.8",
    libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.2",
    libraryDependencies ++= Seq(
      "io.bullet" %%% "borer-core" % "1.10.1",
      "io.bullet" %%% "borer-derivation" % "1.10.1"
    ),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.12.0" % "test"
  )
  .jvmSettings(
    ThisBuild / javaOptions ++= Seq("-Xss10m"),
    Test / fork := true,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true
  )
