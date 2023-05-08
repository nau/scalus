package scalus

import scalus.macros.Macros
import scalus.sir.SIR
import scalus.uplc.Data

import scala.annotation.Annotation

final class Compile extends Annotation

object Compiler:
  inline def fieldAsData[A](inline expr: A => Any): Data => Data = ${
    Macros.fieldAsDataMacro('expr)
  }

  /* This method call is handled by the compiler plugin.
     Leave it as it is.
   */
  def compile(e: Any): SIR = throw new RuntimeException(
    "This method call is handled by the Scalus compiler plugin. " +
      "If you see this message at runtime, the compiler plugin is not enabled." +
      "Try adding the compiler plugin to your build.sbt: " +
      "compilerPlugin(\"scalus\" %% \"scalus-plugin\" % scalusPluginVersion)"
  )
