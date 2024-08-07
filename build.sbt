import org.scalajs.linker.interface.OutputPatterns
import sbtwelcome.*

import java.net.URI

Global / onChangedBuildSource := ReloadOnSourceChanges
autoCompilerPlugins := true

val scalusStableVersion = "0.7.2"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / organization := "org.scalus"
ThisBuild / organizationName := "Scalus"
ThisBuild / organizationHomepage := Some(url("https://scalus.org/"))
ThisBuild / developers := List(
  Developer(
    id = "atlanter",
    name = "Alexander Nemish",
    email = "anemish@gmail.com",
    url = url("https://github.com/nau")
  )
)

ThisBuild / description := "Scalus - DApps Development Platform for Cardano"
ThisBuild / licenses := List(
  "Apache 2" -> new URI("http://www.apache.org/licenses/LICENSE-2.0.txt").toURL
)
ThisBuild / homepage := Some(url("https://github.com/nau/scalus"))
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
Test / publishArtifact := false

lazy val root: Project = project
    .in(file("."))
    .aggregate(
      scalusPlugin,
      scalus.js,
      scalus.jvm,
      `examples-js`,
      examples,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
      docs
    )
    .settings(
      publish / skip := true
    )

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-Wunused:imports",
  "-Wunused:params",
  "-Xcheck-macros"
)

// Scala 3 Compiler Plugin for Scalus
lazy val scalusPlugin = project
    .in(file("scalus-plugin"))
    .settings(
      name := "scalus-plugin",
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Wunused:all",
      // Manually set a fixed version to avoid recompilation on every commit
      // as sbt-ci-release plugin increments the version on every commit
      // thus recompiling the plugin and all dependent projects
      // COMMENT THIS LINE TO ENABLE VERSION INCREMENT during Scalus plugin development
      // version := "0.6.2-SNAPSHOT",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.14.0" % "test",
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value // % "provided"
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
            baseDir / "scalus/builtin/BLS12_381.scala",
            baseDir / "scalus/builtin/ByteString.scala",
            baseDir / "scalus/builtin/Data.scala",
            baseDir / "scalus/builtin/List.scala",
            baseDir / "scalus/sir/SIR.scala",
            baseDir / "scalus/sir/FlatInstances.scala",
            baseDir / "scalus/uplc/Constant.scala",
            baseDir / "scalus/uplc/DefaultFun.scala",
            baseDir / "scalus/uplc/DefaultUni.scala",
            baseDir / "scalus/uplc/CommonFlatInstances.scala",
            baseDir / "scalus/flat/package.scala"
          )
          files
      }
    )

// Used only for Scalus compiler plugin development
// I use it to not recompile all the tests in the main project
// TODO remove or comment out
lazy val scalusPluginTests = project
    .in(file("scalus-plugin-tests"))
    .dependsOn(scalus.jvm)
    .settings(
      name := "scalus-plugin-tests",
      publish / skip := true,
      PluginDependency,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.12.0" % "test"
    )

// Scalus Compiler Plugin Dependency
lazy val PluginDependency: List[Def.Setting[?]] = List(scalacOptions ++= {
    val jar = (scalusPlugin / Compile / packageBin).value
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)

    // NOTE: uncomment for faster Scalus Plugin development
    // this will recompile the plugin when the jar is modified
    // Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    Seq(s"-Xplugin:${jar.getAbsolutePath}")
})

// Scalus Core and Standard Library for JVM and JS
lazy val scalus = crossProject(JSPlatform, JVMPlatform)
    .in(file("."))
    .settings(
      name := "scalus",
      scalaVersion := scalaVersion.value,
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      // scalacOptions += "-Yretain-trees",
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0",
      libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.0.0",
      libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.4",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "3.3.1",
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % "1.14.1",
        "io.bullet" %%% "borer-derivation" % "1.14.1"
      ),
      PluginDependency,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.14.0" % "test"
    )
    .jvmSettings(
      Test / fork := true,
      // Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-S", "-8077211454138081902"),
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.16" % "provided",
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.78.1",
      libraryDependencies += "org.chiachat" % "kbls" % "1.0.3",
      libraryDependencies += "org.bitcoin-s" % "bitcoin-s-crypto_2.13" % "1.9.9",
      libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "1.9.9"
    )
    .jsSettings(
      // Add JS-specific settings here
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
          // Use .mjs extension.
//              .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
      },
      scalaJSUseMainModuleInitializer := false
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

lazy val examples = project
    .in(file("examples"))
    .dependsOn(scalus.jvm, `scalus-bloxbean-cardano-client-lib`)
    .settings(
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.5.1"
    )

lazy val `examples-js` = project
    .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
    .in(file("examples-js"))
    .dependsOn(scalus.js)
    .settings(
      publish / skip := true,
      scalacOptions ++= commonScalacOptions,
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      scalaJSUseMainModuleInitializer := false,
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      },
      PluginDependency
    )

// Bloxbean Cardano Client Lib integration and Tx Evaluator implementation
lazy val `scalus-bloxbean-cardano-client-lib` = project
    .in(file("bloxbean-cardano-client-lib"))
    .dependsOn(scalus.jvm)
    .settings(
      publish / skip := false,
      scalacOptions ++= commonScalacOptions,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % "0.5.1",
      libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.13",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.16" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.5.1" % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % "0.3.0-beta14" % "test",
      Test / fork := true, // needed for BlocksValidation to run in sbt
      inConfig(Test)(PluginDependency)
    )

// Documentation
// We use Docusaurus for documentation
// and Mdoc for Scala code examples
lazy val docs = project // documentation project
    .in(file("scalus-docs")) // important: it must not be docs/
    .dependsOn(scalus.jvm)
    .enablePlugins(MdocPlugin, DocusaurusPlugin)
    .settings(
      publish / skip := true,
      moduleName := "scalus-docs",
      mdocVariables := Map(
        "VERSION" -> scalusStableVersion,
        "SCALA3_VERSION" -> scalaVersion.value
      ),
      PluginDependency
    )

// Benchmarks for Cardano Plutus VM Evaluator
lazy val bench = project
    .in(file("bench"))
    .dependsOn(scalus.jvm)
    .enablePlugins(JmhPlugin)
    .settings(
      name := "scalus-bench",
      PluginDependency,
      publish / skip := true
    )

addCommandAlias(
  "precommit",
  "clean;docs/clean;scalusPluginTests/clean;scalafmtAll;scalafmtSbt;Test/compile;scalusPluginTests/Test/compile;test;docs/mdoc"
)

logo :=
    s"""
     |  ${scala.Console.RED}███████╗ ██████╗ █████╗ ██╗     ██╗   ██╗███████╗
     |  ${scala.Console.RED}██╔════╝██╔════╝██╔══██╗██║     ██║   ██║██╔════╝
     |  ${scala.Console.RED}███████╗██║     ███████║██║     ██║   ██║███████╗
     |  ${scala.Console.RED}╚════██║██║     ██╔══██║██║     ██║   ██║╚════██║
     |  ${scala.Console.RED}███████║╚██████╗██║  ██║███████╗╚██████╔╝███████║
     |  ${scala.Console.RED}╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚══════╝
     |
     |  Version: ${version.value} ${scala.Console.YELLOW}Scala ${scalaVersion.value}${scala.Console.RESET}
     |
     |""".stripMargin

usefulTasks := Seq(
  UsefulTask("~compile", "Compile with file-watch enabled"),
  UsefulTask("precommit", "Format all, clean compile and test everything")
)
