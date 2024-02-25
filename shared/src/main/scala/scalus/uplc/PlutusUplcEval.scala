package scalus.uplc

import java.io.ByteArrayInputStream
import scalus.uplc.eval.ExBudget
import scalus.uplc.eval.ExMemory
import scalus.uplc.eval.ExCPU

enum UplcEvalResult:
    case Success(term: Term, budget: ExBudget)
    case UplcFailure(errorCode: Int, error: String)
    case TermParsingError(error: String)

object PlutusUplcEval:
    val budget = raw"""\s*CPU budget:\s+(\d+)\s*Memory budget:\s+(\d+)""".r

    def evalFlat(program: Program): UplcEvalResult =
        import cats.implicits.toShow
        val flat = program.flatEncoded
        // println(s"Flat size: ${flat.length}}")
        import scala.sys.process.*
        val cmd = "uplc evaluate --input-format flat --counting --trace-mode LogsWithBudgets"
        var out = ""
        val retCode = cmd.#<(new ByteArrayInputStream(flat)).!(ProcessLogger(o => out += o))
        if retCode == 0 then
            UplcParser.term.parse(out) match
                case Right(budget(cpu, mem), term) =>
                    UplcEvalResult.Success(term, ExBudget(ExCPU(cpu.toLong), ExMemory(mem.toLong)))
                case Right(left, term) =>
                    UplcEvalResult.Success(term, ExBudget(ExCPU(-1), ExMemory(-1)))
                case Left(err) => UplcEvalResult.TermParsingError(err.show)
        else UplcEvalResult.UplcFailure(retCode, out)
