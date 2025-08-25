import org.scalajs.linker.interface.OutputPatterns
import sbtwelcome.*

import scala.scalanative.build.*
import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.IncompatibleResultTypeProblem
import com.typesafe.tools.mima.core.ProblemFilters.*
import sbt.internal.util.ManagedLogger

import java.net.URI

Global / onChangedBuildSource := ReloadOnSourceChanges
autoCompilerPlugins := true

val scalusStableVersion = "0.10.0"
val scalusCompatibleVersion = scalusStableVersion
//ThisBuild / scalaVersion := "3.8.0-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.3.7-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.7.3-RC1-bin-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"
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
Test / publishArtifact := false

ThisBuild / javaOptions += "-Xss64m"

Compile / doc / scalacOptions ++= Seq(
  "-groups",
  "-project-version",
  scalusStableVersion,
  "-project-footer",
  "Lantr.io"
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-Wunused:imports",
  "-Wunused:params",
  "-Xcheck-macros"
  //  "-rewrite",
  //  "-source:future-migration"
)

lazy val copySharedFiles = taskKey[Unit]("Copy shared files")

lazy val root: Project = project
    .in(file("."))
    .aggregate(
      scalusPlugin,
      scalus.js,
      scalus.jvm,
      scalus.native,
      scalusCardanoLedger.jvm,
      scalusCardanoLedger.js,
      scalusTestkit.js,
      scalusTestkit.jvm,
      scalusTestkit.native,
      scalusExamples.js,
      scalusExamples.jvm,
      scalusDesignPatterns,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
      docs
    )
    .settings(
      publish / skip := true
    )

// all JVM projects are aggregated in the jvm project just for convenience
lazy val jvm: Project = project
    .in(file("jvm"))
    .aggregate(
      scalusPlugin,
      scalus.jvm,
      scalusPluginTests,
      scalusCardanoLedger.jvm,
      scalusTestkit.jvm,
      scalusExamples.jvm,
      scalusDesignPatterns,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
    )
    .settings(
      publish / skip := true
    )

// Scala 3 Compiler Plugin for Scalus
lazy val scalusPlugin = project
    .in(file("scalus-plugin"))
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
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
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value // % "provided"
    )
    .settings(
      /*
       Include common sources in the plugin
       we can't add the scalus project as a dependency because this is a Scala compiler plugin
       and apparently it's not supported
       Another option is to use sbt-assembly to create a fat jar with all the dependencies
       I copy the shared files to the plugin project because when I use managedSources in the plugin
       IntelliJ IDEA only sees these files being used in the plugin project and not in the main project
       This breaks navigation and refactoring in the main project.
       By copying the shared files to the plugin project, IntelliJ IDEA sees them as used in the plugin project
       */
      copySharedFiles := {
          val sharedFiles = Seq(
            "scalus/utils/Hex.scala",
            "scalus/utils/HashConsed.scala",
            "scalus/utils/HashConsedFlat.scala",
            "scalus/builtin/Data.scala",
            "scalus/builtin/List.scala",
            "scalus/sir/SIR.scala",
            "scalus/sir/SIRDefaultOptions.scala",
            "scalus/sir/SIRMacro.scala",
            "scalus/sir/SIRType.scala",
            "scalus/sir/SIRToExpr.scala",
            "scalus/sir/SIRBuiltins.scala",
            "scalus/sir/SIRUnify.scala",
            "scalus/sir/SIRHashCodeInRec.scala",
            "scalus/sir/FlatInstances.scala",
            "scalus/sir/RemoveRecursivity.scala",
            "scalus/sir/RenamingTypeVars.scala",
            "scalus/uplc/Constant.scala",
            "scalus/uplc/DefaultFun.scala",
            "scalus/uplc/DefaultUni.scala",
            "scalus/uplc/CommonFlatInstances.scala",
            "scalus/uplc/TypeScheme.scala",
            "scalus/flat/package.scala"
          )

          val baseDir =
              baseDirectory.value / ".." / "scalus-core" / "shared" / "src" / "main" / "scala"
          val targetDir = (Compile / sourceDirectory).value / "shared" / "scala"
          val log = streams.value.log
          copyFiles(sharedFiles, baseDir, targetDir, log)
          log.info(s"Copied shared files to target $targetDir")
      },
//      Compile / managedSources ++= {
//          val baseDir = baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
//          sharedFiles.map(file => baseDir / file)
//      },
      Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value / "shared" / "scala",
      clean := {
          (Compile / clean).value
          streams.value.log.info("Cleaning shared files")
          IO.delete((Compile / sourceDirectory).value / "shared")
      },
      Compile / compile := (Compile / compile).dependsOn(copySharedFiles).value
    )

// Used only for Scalus compiler plugin development
// I use it to not recompile all the tests in the main project
// TODO remove or comment out
lazy val scalusPluginTests = project
    .in(file("scalus-plugin-tests"))
    .dependsOn(scalus.jvm)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-plugin-tests",
      publish / skip := true,
      PluginDependency,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test"
    )

// Scalus Compiler Plugin Dependency
lazy val PluginDependency: List[Def.Setting[?]] = List(scalacOptions ++= {
    val jar = (scalusPlugin / Compile / packageBin).value
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)

    // NOTE: uncomment for faster Scalus Plugin development
    // this will recompile the plugin when the jar is modified
//    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    Seq(s"-Xplugin:${jar.getAbsolutePath}")
})

lazy val prepareNpmPackage = taskKey[Unit]("Make an copy scalus bundle.js to npm directory")

// Scalus Core and Standard Library for JVM and JS
lazy val scalus = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("scalus-core"))
    .settings(
      name := "scalus",
      scalaVersion := scalaVersion.value,
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      // scalacOptions += "-Yretain-trees",
      mimaPreviousArtifacts := Set(organization.value %%% name.value % scalusCompatibleVersion),

      // enable when debug compilation of tests
      Test / scalacOptions += "-color:never",
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
      libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.1.0",
      libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.4",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.2.1",
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.36.5",
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.36.5" % "compile",
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % "1.16.1",
        "io.bullet" %%% "borer-derivation" % "1.16.1"
      ),
      PluginDependency,
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      buildInfoKeys ++= Seq[BuildInfoKey](
        "scalusVersion" -> scalusStableVersion
      ),
      buildInfoPackage := "scalus.buildinfo"
    )
    .configure { project =>
        project.enablePlugins(BuildInfoPlugin)
    }
    .jvmSettings(
      Test / fork := true,
      // needed for secp256k1jni. Otherwise, JVM loads secp256k1 library from LD_LIBRARY_PATH
      // which doesn't export the secp256k1_ec_pubkey_decompress function
      // that is needed by bitcoin-s-secp256k1jni, because it's an older fork of secp256k1
      Test / javaOptions += "-Djava.library.path=",
      // Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-S", "-8077211454138081902"),
      Test / testOptions += Tests.Argument("-oF"),
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "provided",
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.81",
      libraryDependencies += "foundation.icon" % "blst-java" % "0.3.2",
      libraryDependencies += "org.bitcoin-s" % "bitcoin-s-crypto_2.13" % "1.9.10" % "test",
      libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "1.9.10"
    )
    .jsSettings(
      // Add JS-specific settings here
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      // copy scalus-*-bundle.js to dist for publishing on npm
      prepareNpmPackage := {
          val bundle = (Compile / fullOptJS / webpack).value
          val target = (Compile / sourceDirectory).value / "npm"
          bundle.foreach(f => IO.copyFile(f.data.file, target / f.data.file.getName))
          streams.value.log.info(s"Copied ${bundle} to ${target}")
      },
      // use custom webpack config to export scalus as a commonjs2 module
      // otherwise it won't export the module correctly
      webpackConfigFile := Some(sourceDirectory.value / "main" / "webpack" / "webpack.config.js"),
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
          // Use .mjs extension.
//              .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
      },
      scalaJSUseMainModuleInitializer := false
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }
    .nativeSettings(
      nativeConfig ~= {
          _.withBuildTarget(BuildTarget.libraryStatic)
//              .withLTO(LTO.thin)
              .withMode(Mode.releaseFast)
              .withGC(GC.immix)
      }
    )

def copyFiles(files: Seq[String], baseDir: File, targetDir: File, log: ManagedLogger): Unit = {
    files.foreach { file =>
        val source = baseDir / file
        val target = targetDir / file
        if (source.exists) {
            if (!target.exists) {
                IO.copyFile(source, target)
            } else if (source.lastModified() > target.lastModified()) {
                IO.copyFile(source, target)
            }
        } else {
            log.error(s"Shared file $file does not exist in $baseDir")
        }
    }
}

// Scalus Testkit library for testing Scalus applications
lazy val scalusTestkit = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("scalus-testkit"))
    .dependsOn(scalus)
    .settings(
      name := "scalus-testkit",
      scalaVersion := scalaVersion.value,
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for Arbitrary[Certificate] = autoDerived
      Test / scalacOptions += "-color:never",
      copySharedFiles := {
          val sharedFiles = Seq(
            "scalus/testutil/ArbitraryDerivation.scala",
            "scalus/uplc/test/ArbitraryInstances.scala",
            "scalus/ledger/api/v1/ArbitraryInstances.scala",
            "scalus/ledger/api/v2/ArbitraryInstances.scala",
            "scalus/ledger/api/v3/ArbitraryInstances.scala",
            "scalus/cardano/address/ArbitraryInstances.scala",
            "scalus/cardano/ledger/ArbitraryInstances.scala"
          )
          val baseDir =
              (scalus.jvm / crossProjectBaseDirectory).value / "shared" / "src" / "test" / "scala"
          val targetDir = crossProjectBaseDirectory.value / "core-shared" / "scala"
          val log = streams.value.log
          copyFiles(sharedFiles, baseDir, targetDir, log)
          log.info(s"Copied shared files to target $targetDir")
      },
      Compile / unmanagedSourceDirectories += crossProjectBaseDirectory.value / "core-shared" / "scala",
      Compile / compile := (Compile / compile).dependsOn(copySharedFiles).value,
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0"
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      },
      scalaJSUseMainModuleInitializer := false
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }
    .nativeSettings(
      nativeConfig ~= {
          _.withBuildTarget(BuildTarget.libraryStatic)
      }
    )

lazy val scalusExamples = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-examples"))
    .dependsOn(scalus, scalusTestkit)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "io.bullet" %%% "borer-derivation" % "1.16.1",
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test"
    )
    .configurePlatform(JVMPlatform)(_.dependsOn(`scalus-bloxbean-cardano-client-lib`))
    .jvmSettings(
      Test / fork := true,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.6.6"
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      scalaJSUseMainModuleInitializer := false,
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

lazy val scalusDesignPatterns = project
    .in(file("scalus-design-patterns"))
    .dependsOn(scalus.jvm, scalusTestkit.jvm)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      Test / fork := true,
      //// enable if need speedup
      // trackInternalDependencies := TrackLevel.TrackIfMissing,
    )

// Bloxbean Cardano Client Lib integration and Tx Evaluator implementation
lazy val `scalus-bloxbean-cardano-client-lib` = project
    .in(file("bloxbean-cardano-client-lib"))
    .dependsOn(scalus.jvm, scalusCardanoLedger.jvm)
    .settings(
      publish / skip := false,
      scalacOptions ++= commonScalacOptions,
      mimaPreviousArtifacts := Set(organization.value %% name.value % scalusCompatibleVersion),
      mimaBinaryIssueFilters ++= Seq(
        ProblemFilters
            .exclude[IncompatibleResultTypeProblem]("scalus.bloxbean.Interop.getMintValue"),
        ProblemFilters.exclude[IncompatibleResultTypeProblem]("scalus.bloxbean.Interop.getValue"),
        ProblemFilters
            .exclude[IncompatibleResultTypeProblem]("scalus.bloxbean.Interop.getVotingProcedures")
      ),
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % "0.6.6",
      libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % "0.6.6" % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % "0.3.6" % "test",
      libraryDependencies += "io.bullet" %%% "borer-derivation" % "1.16.1",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.0" % "test",
      Test / fork := true, // needed for BlocksValidation to run in sbt
      inConfig(Test)(PluginDependency)
    )

// Documentation
// We use Docusaurus for documentation
// and Mdoc for Scala code examples
lazy val docs = project // documentation project
    .in(file("scalus-docs")) // important: it must not be docs/
    .dependsOn(scalus.jvm)
    .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      publish / skip := true,
      moduleName := "scalus-docs",
      mdocVariables := Map(
        "VERSION" -> scalusStableVersion,
        "SCALA3_VERSION" -> scalaVersion.value
      ),
      ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(scalus.jvm),
      ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
      cleanFiles += (ScalaUnidoc / unidoc / target).value,
      docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
      docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value,
      PluginDependency
    )

// Benchmarks for Cardano Plutus VM Evaluator
lazy val bench = project
    .in(file("bench"))
    .dependsOn(scalus.jvm)
    .enablePlugins(JmhPlugin)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-bench",
      PluginDependency,
      publish / skip := true
    )

// Cardano Ledger domain model and CBOR serialization
lazy val scalusCardanoLedger = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-cardano-ledger"))
    .dependsOn(scalus % "compile->compile;test->test")
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-cardano-ledger",
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % "1.16.1",
        "io.bullet" %%% "borer-derivation" % "1.16.1"
      ),
      libraryDependencies += "com.outr" %%% "scribe" % "3.17.0",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % "0.6.6" % "test",
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      publish / skip := false
    )
    .jvmSettings(
      // temporary, needed for current PlutusScriptEvaluator implementation
      // TODO: remove when PlutusScriptEvaluator uses different logger
      libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.4.2",
      scalaJSUseMainModuleInitializer := false,
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

addCommandAlias(
  "mima",
  "scalus-bloxbean-cardano-client-lib/mimaReportBinaryIssues"
)
addCommandAlias(
  "quick",
  "scalafmtAll;scalafmtSbt;jvm/Test/compile;jvm/testQuick"
)
addCommandAlias(
  "precommit",
  "clean;docs/clean;scalusPluginTests/clean;scalafmtAll;scalafmtSbt;Test/compile;scalusPluginTests/Test/compile;test;docs/mdoc"
)
addCommandAlias(
  "ci",
  "clean;docs/clean;scalusPluginTests/clean;scalafmtCheckAll;scalafmtSbtCheck;Test/compile;scalusPluginTests/Test/compile;test;docs/mdoc;mima"
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
  UsefulTask("quick", "Format all, compile and quick test everything on JVM"),
  UsefulTask("precommit", "Format all, clean compile and test everything"),
  UsefulTask("ci", "Clean compile, check formatting and test everything"),
  UsefulTask("mima", "Check binary compatibility with the previous version using MiMa"),
  UsefulTask("docs/docusaurusCreateSite", "Generate Scalus documentation website")
)
