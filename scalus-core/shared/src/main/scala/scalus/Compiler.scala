package scalus

import scalus.macros.Macros
import scalus.sir.{SIR, SIRType}
import scalus.builtin.Data
import scalus.uplc.Term

import scala.annotation.Annotation

final class Compile extends Annotation
final class Ignore extends Annotation

/** This is a marker trait for the compiler plugin to compile derivations of the instances of the
  * type classes.
  * @see
  *   scalus.prelude.ToData, scalus.prelude.FromData
  */
trait CompileDerivations

object Compiler:
    inline def fieldAsData[A](inline expr: A => Any): Data => Data = ${
        Macros.fieldAsDataMacro('expr)
    }

    /* This method call is handled by the compiler plugin.
     Leave it as it is.
     */
    def compile(e: Any): SIR = throwCompilerPluginMissingException()

    /* This method call is handled by the compiler plugin.
         Leave it as it is.
     */
    def compileDebug(e: Any): SIR = throwCompilerPluginMissingException()

    /* This method call is handled by the compiler plugin.
         Leave it as it is.
     */
    def compileType[T]: SIRType = throwCompilerPluginMissingException()

    inline def compileToUplc(inline code: Any): Term = ${
        Macros.compileToUplcImpl('code)
    }

    private def throwCompilerPluginMissingException(): Nothing =
        throw new RuntimeException(
          "This method call is handled by the Scalus compiler plugin. " +
              "If you see this message at runtime, the compiler plugin is not enabled." +
              "Try adding the compiler plugin to your build.sbt: " +
              "compilerPlugin(\"scalus\" %% \"scalus-plugin\" % scalusPluginVersion)"
        )
