import org.scalajs.linker.interface.OutputPatterns

val scala3Version = "3.3.1"
val scalusVersion = "0.4.0"

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / scalaVersion := scala3Version
Test / publishArtifact := false
autoCompilerPlugins := true
ThisBuild / organization := "org.scalus"
ThisBuild / organizationName := "Scalus"
ThisBuild / organizationHomepage := Some(url("https://scalus.org/"))

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/nau/scalus"), "scm:git@github.com:nau/scalus.git")
)
ThisBuild / developers := List(
  Developer(
    id = "atlanter",
    name = "Alexander Nemish",
    email = "anemish@gmail.com",
    url = url("https://github.com/nau")
  )
)

ThisBuild / description := "Scalus is a Scala library for writing Plutus smart contracts."
ThisBuild / licenses := List(
  "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / homepage := Some(url("https://github.com/nau/scalus"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

// Temporary. This is needed for addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.1+14-bd750aad-SNAPSHOT")
// TODO: Remove when mdoc 2.5.2 is released
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots")

lazy val root: Project = project
  .in(file("."))
  .aggregate(scalusPlugin, scalus.js, scalus.jvm, `examples-js`, examples)
  .settings(
    publish / skip := true
  )

lazy val scalusPlugin = project
  .in(file("scalus-plugin"))
  .settings(
    name := "scalus-plugin",
    organization := "org.scalus",
    version := scalusVersion,
    scalacOptions += "-Wunused:all",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.14.0" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version // % "provided"
  )
  .settings(
    // Include common sources in the plugin
    // we can't add the scalus project as a dependency because this is a Scala compiler plugin
    // and apparently it's not supported
    // Another option is to use sbt-assembly to create a fat jar with all the dependencies
    // This is a simpler solution
    Compile / managedSources ++= {
      val baseDir = baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
      val files = Seq(
        baseDir / "scalus/utils/Hex.scala",
        baseDir / "scalus/builtins/ByteString.scala",
        baseDir / "scalus/builtins/List.scala",
        baseDir / "scalus/sir/SIR.scala",
        baseDir / "scalus/sir/FlatInstances.scala",
        baseDir / "scalus/uplc/Constant.scala",
        baseDir / "scalus/uplc/Data.scala",
        baseDir / "scalus/uplc/DefaultFun.scala",
        baseDir / "scalus/uplc/DefaultUni.scala",
        baseDir / "scalus/uplc/CommonFlatInstantces.scala",
        baseDir / "scalus/flat/package.scala"
      )
      files
    }
  )

lazy val scalusPluginTests = project
  .in(file("scalus-plugin-tests"))
  .dependsOn(scalus.jvm)
  .settings(
    name := "scalus-plugin-tests",
    organization := "scalus",
    version := scalusVersion,
    PluginDependency,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.12.0" % "test"
  )

lazy val PluginDependency: List[Def.Setting[_]] = List(scalacOptions ++= {
  val jar = (scalusPlugin / Compile / packageBin).value
  // add plugin timestamp to compiler options to trigger recompile of
  // main after editing the plugin. (Otherwise a 'clean' is needed.)
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
})

lazy val scalus = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "scalus",
    version := scalusVersion,
    scalaVersion := scala3Version,
    scalacOptions += "-Xcheck-macros",
    scalacOptions += "-explain",
    scalacOptions += "-Wunused:imports",
    scalacOptions += "-Wunused:params",
    // scalacOptions += "-Yretain-trees",
    libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.0.0",
    libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.3",
    libraryDependencies ++= Seq(
      "io.bullet" %%% "borer-core" % "1.13.0",
      "io.bullet" %%% "borer-derivation" % "1.13.0"
    ),
    PluginDependency,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.14.0" % "test"
  )
  .jvmSettings(
    ThisBuild / javaOptions ++= Seq("-Xss10m"),
    Test / fork := true,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version,
    libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.77",
    libraryDependencies += "org.bitcoin-s" % "bitcoin-s-crypto_2.13" % "1.9.7",
    libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "1.9.7"
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := false
  )

lazy val examples = project
  .in(file("examples"))
  .dependsOn(scalus.jvm % "compile->compile;compile->test")
  .settings(
    PluginDependency,
    publish / skip := true,
    libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % "0.5.0",
    libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.5.0"
  )

lazy val `examples-js` = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("examples-js"))
  .dependsOn(scalus.js)
  .settings(
    publish / skip := true,
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        // Use .mjs extension.
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    },
    PluginDependency
  )

lazy val docs = project // documentation project
  .in(file("scalus-docs")) // important: it must not be docs/
  .dependsOn(scalus.jvm)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .settings(
    publish / skip := true,
    moduleName := "scalus-docs",
    mdocVariables := Map(
      "VERSION" -> scalusVersion,
      "SCALA3_VERSION" -> scala3Version
    ),
    // mdocOut := mdocOut.value / "docs",
    scalacOptions ++= Seq(
      // FIXME: use published scalus-plugin
      s"-Xplugin:${(scalusPlugin / Compile / target).value}/scala-${scala3Version}/scalus-plugin_3-${scalusVersion}.jar",
    )
  )
