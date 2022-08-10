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
    version := "0.1-SNAPSHOT",
    scalaVersion := scala3Version,
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
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true
  )
