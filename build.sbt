val scala3Version = "3.2.2"
ThisBuild / scalaVersion := scala3Version

lazy val root = project
  .in(file("."))
  .aggregate(scalusPlugin, scalus.js, scalus.jvm, examples, `examples-js`)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val scalusPlugin = project
  .in(file("scalus-plugin"))
  .dependsOn(scalus.jvm)
  .settings(
    name := "scalus-plugin",
    organization := "scalus",
    version := "0.1.0",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version // % "provided"
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
    javaOptions in ThisBuild ++= Seq("-Xss10m"),
    fork in Test := true,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true
  )

lazy val examples = project
  .in(file("examples"))
  .dependsOn(scalus.jvm % "compile->compile;compile->test")
  .settings(
    libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.3.0"
  )

lazy val `examples-js` = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("examples-js"))
  .dependsOn(scalus.js)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalacOptions += "-Xcheck-macros",
    scalacOptions += "-Yretain-trees")

lazy val bench = project
  .dependsOn(scalus.jvm)
  .settings(
    name := "scalus-bench",
    organization := "scalus",
    version := "0.1.0",
//    scalacOptions += "-Xprint:patternMatcher,genBCode",
    libraryDependencies += compilerPlugin("scalus" %% "scalus-plugin" % "0.1.0")
  )
