val scala3Version = "3.2.2"
ThisBuild / scalaVersion := scala3Version
autoCompilerPlugins := true
lazy val root = project
  .in(file("."))
  .aggregate(scalusPlugin, scalus.js, scalus.jvm, `examples-js`, examples)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val scalusPlugin = project
  .in(file("scalus-plugin"))
  .settings(
    name := "scalus-plugin",
    organization := "scalus",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.12.0" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version // % "provided"
  ).settings(
    // Include common sources in the plugin
    // we can't add the scalus project as a dependency because this is a Scala compiler plugin
    // and apparently it's not supported
    // TODO: add other common sources
    Compile / managedSources ++= {
      val baseDir = baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
      val files = Seq(
        baseDir / "scalus/sir/SIR.scala",
        baseDir / "scalus/flat/package.scala",
        )
      files
    }
  )

lazy val PluginDependency: List[Def.Setting[_]] = List(scalacOptions ++= {
  val jar = (packageBin in Compile in scalusPlugin).value
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
})

lazy val scalus = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "scalus",
    version := "0.1-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions += "-Xcheck-macros",
    scalacOptions += "-explain",
    // scalacOptions += "-Yretain-trees",
    libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.8",
    libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.2",
    libraryDependencies ++= Seq(
      "io.bullet" %%% "borer-core" % "1.10.1",
      "io.bullet" %%% "borer-derivation" % "1.10.1"
    ),
    PluginDependency,
    // libraryDependencies += compilerPlugin("scalus" %% "scalus-plugin" % "0.1.0-SNAPSHOT"),
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
    PluginDependency,
    libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.3.0"
  )

lazy val `examples-js` = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("examples-js"))
  .dependsOn(scalus.js)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    PluginDependency,
  )