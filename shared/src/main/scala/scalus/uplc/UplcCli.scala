package scalus.uplc

import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.uplc.eval.ExBudget
import scalus.uplc.eval.ExCPU
import scalus.uplc.eval.ExMemory

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import scala.sys.process.*

/** Represents the result of evaluating a UPLC program using `uplc` CLI */
enum UplcEvalResult:
    /** Represents a successful evaluation of a UPLC program Contains the evaluated term and the
      * execution budget used for evaluation
      */
    case Success(term: Term, budget: ExBudget)

    /** Represents a failure in evaluating a UPLC program
      *
      * @param errorCode
      *   the error code returned by the `uplc` CLI
      * @param error
      *   the error message returned by the `uplc` CLI
      */
    case UplcFailure(errorCode: Int, error: String)

    /** Represents a failure in parsing the evaluated term Normally this should not happen and
      * indicates a bug in the UPLC parser or changes in the output format of `uplc`
      */
    case TermParsingError(error: String)

/** Cardano `uplc` CLI interface */
object UplcCli:
    private val budget = raw"""\s*CPU budget:\s+(\d+)\s*Memory budget:\s+(\d+)""".r

    /** Evaluates a UPLC program using the Cardano `uplc` CLI using the builtin semantics variant
      * 'B'.
      *
      * @param program
      *   the UPLC program
      */
    def evalFlat(program: Program): UplcEvalResult =
        evalFlat(program, BuiltinSemanticsVariant.B)

    /** Evaluates a UPLC program using the Cardano `uplc` CLI
      *
      * @param program
      *   the UPLC program
      */
    def evalFlat(program: Program, semanticsVariant: BuiltinSemanticsVariant): UplcEvalResult =
        import cats.implicits.toShow
        val flat = program.flatEncoded
        val cmd =
            s"uplc evaluate --input-format flat --counting --trace-mode LogsWithBudgets --builtin-semantics-variant $semanticsVariant"
        var out = ""
        val retCode = cmd.#<(new ByteArrayInputStream(flat)).!(ProcessLogger(o => out += o))
        if retCode == 0 then
            UplcParser().term.parse(out) match
                case Right(budget(cpu, mem), term) =>
                    UplcEvalResult.Success(term, ExBudget(ExCPU(cpu.toLong), ExMemory(mem.toLong)))
                case Right(left, term) =>
                    UplcEvalResult.Success(term, ExBudget(ExCPU(-1), ExMemory(-1)))
                case Left(err) => UplcEvalResult.TermParsingError(err.show)
        else UplcEvalResult.UplcFailure(retCode, out)

    /** Converts a UPLC program to `flat` encoding using the Cardano `uplc` CLI
      *
      * @param program
      *   the textual representation of the UPLC program
      * @return
      *   byte array of the flat encoded program
      */
    def uplcToFlat(program: String): Array[Byte] =
        val cmd = "uplc convert --of flat"
        val outStream = new ByteArrayOutputStream()
        cmd.#<(new ByteArrayInputStream(program.getBytes("UTF-8"))).#>(outStream).!
        outStream.toByteArray
