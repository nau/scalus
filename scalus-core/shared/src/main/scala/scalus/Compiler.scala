package scalus

import scalus.macros.Macros
import scalus.sir.{SIR, SIRType}
import scalus.builtin.Data

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

    enum TargetLoweringBackend:
        case SimpleSirToUplcLowering
        case SirToUplc110Lowering
        case SirToUplcV3Lowering

    case class Options(
        targetLoweringBackend: TargetLoweringBackend =
            TargetLoweringBackend.SimpleSirToUplcLowering,
        generateErrorTraces: Boolean = true,
        optimizeUplc: Boolean = false,
        debug: Boolean = false
    )
    val defaultOptions: Options = Options()

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

    /** Generates a `Compiler.compile(code)` call at compile time.
      *
      * When you want to use [[Compiler.compile]] in an inline method you can't do it directly,
      * because the Scalus compiler plugin finds the call to `Compiler.compile` and replaces it with
      * the compiled code. Which is not what you want in this case!
      *
      * For example this will not work:
      * {{{
      *   inline def myTest(inline code: Any): SIR = {
      *       Compiler.compile(code).toUplc().evaluate
      *   }
      * }}}
      *
      * Instead, you should use this method:
      * {{{
      *  inline def myTest(inline code: Any): SIR = {
      *     Compiler.compileInline(code).toUplc().evaluate
      *  }
      * }}}
      */
    inline def compileInline(inline code: Any): SIR = ${ Macros.generateCompileCall('code) }

    private def throwCompilerPluginMissingException(): Nothing =
        throw new RuntimeException(
          "This method call is handled by the Scalus compiler plugin. " +
              "If you see this message at runtime, the compiler plugin is not enabled." +
              "Try adding the compiler plugin to your build.sbt: " +
              "compilerPlugin(\"scalus\" %% \"scalus-plugin\" % scalusPluginVersion)"
        )
