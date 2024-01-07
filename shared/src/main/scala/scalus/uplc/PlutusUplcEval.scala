package scalus.uplc

import java.io.ByteArrayInputStream

enum UplcEvalResult:
    case Success(term: Term)
    case UplcFailure(errorCode: Int, error: String)
    case TermParsingError(error: String)

object PlutusUplcEval:
    def evalFlat(program: Program): UplcEvalResult =
        import cats.implicits.toShow
        val flat = program.flatEncoded
        // println(s"Flat size: ${flat.length}}")
        import scala.sys.process.*
        val cmd = "uplc evaluate --input-format flat --trace-mode LogsWithBudgets"
        var out = ""
        val retCode = cmd.#<(new ByteArrayInputStream(flat)).!(ProcessLogger(o => out += o))
        if retCode == 0 then
            UplcParser.term.parse(out) match
                case Right(_, term) => UplcEvalResult.Success(term)
                case Left(err)      => UplcEvalResult.TermParsingError(err.show)
        else UplcEvalResult.UplcFailure(retCode, out)
