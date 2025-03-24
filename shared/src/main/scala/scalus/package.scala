import org.typelevel.paiges.Doc
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.sir.PrettyPrinter.Style
import scalus.sir.*
import scalus.uplc.eval.*
import scalus.uplc.transform.{CaseConstrApply, EtaReduce, ForcedBuiltinsExtractor, Inliner}
import scalus.uplc.{Program, *}
import scalus.utils.Utils

package object scalus {

    /** Pipe operator */
    extension [A](inline a: A) inline infix def |>[B](inline f: A => B): B = f(a)

    extension (sir: SIR)
        def pretty: Doc = PrettyPrinter.pretty(sir, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(sir, Style.XTerm)
        def show: String = pretty.render(80)
        def showHighlighted: String = sir.prettyXTerm.render(80)
        @deprecated("Use toUplc().plutusV* instead", "0.8.4")
        def doubleCborHex(version: (Int, Int, Int), generateErrorTraces: Boolean = false): String =
            val term = sir.toUplc(generateErrorTraces)
            Program(version, term).doubleCborHex

        def toUplc(generateErrorTraces: Boolean = false): Term =
            SimpleSirToUplcLowering(sir, generateErrorTraces).lower()
        def toUplcOptimized(generateErrorTraces: Boolean = false): Term = {
            SimpleSirToUplcLowering(sir, generateErrorTraces).lower()
                |> EtaReduce.apply
                |> Inliner.apply
                |> CaseConstrApply.apply
                |> ForcedBuiltinsExtractor.apply
        }

        @deprecated("Use toUplc().plutusV* instead", "0.8.4")
        def toPlutusProgram(
            version: (Int, Int, Int),
            generateErrorTraces: Boolean = false
        ): Program =
            val term = sir.toUplc(generateErrorTraces)
            Program(version, term)

    extension (p: Program)
        def pretty: Doc = PrettyPrinter.pretty(p, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(p, Style.XTerm)
        def show: String = p.pretty.render(80)
        def showHighlighted: String = p.prettyXTerm.render(80)
        def writePlutusFile(path: String, plutusVersion: PlutusLedgerLanguage): Unit =
            Utils.writePlutusFile(path, p.deBruijnedProgram, plutusVersion)
        @deprecated("Use evaluateDebug instead", "0.8.4")
        def evalDebug: Result = VM.evaluateDebug(p.term)
        @deprecated("Use evaluate instead", "0.8.4")
        def eval: Term = VM.evaluateProgram(p)

        /** Evaluates the program using the given VM according to the Plutus specification.
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term =
            vm.evaluateScript(p.deBruijnedProgram, NoBudgetSpender, NoLogger)

        /** Evaluates the program using the given VM according to the Plutus specification.
          * @return
          *   [[Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result = vm.evaluateScriptDebug(p.deBruijnedProgram)

    extension (p: DeBruijnedProgram)
        def pretty: Doc = PrettyPrinter.pretty(p.toProgram, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(p.toProgram, Style.XTerm)
        def show: String = p.pretty.render(80)
        def showHighlighted: String = p.prettyXTerm.render(80)
        def writePlutusFile(path: String, plutusVersion: PlutusLedgerLanguage): Unit =
            Utils.writePlutusFile(path, p, plutusVersion)
        @deprecated("Use evaluateDebug instead", "0.8.4")
        def evalDebug: Result = VM.evaluateDebug(p.term)
        @deprecated("Use evaluate instead", "0.8.4")
        def eval: Term = VM.evaluateProgram(p.toProgram)

        /** Evaluates the program using the given VM according to the Plutus specification.
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term = vm.evaluateScript(p, NoBudgetSpender, NoLogger)

        /** Evaluates the program using the given VM according to the Plutus specification.
          * @return
          *   [[Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result = vm.evaluateScriptDebug(p)

    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)

    extension (self: Term)
        def pretty: Doc = PrettyPrinter.pretty(self, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(self, Style.XTerm)
        def show: String = self.pretty.render(80)
        def showHighlighted: String = self.prettyXTerm.render(80)
        @deprecated("Use evaluateDebug instead", "0.8.4")
        def evalDebug: Result = VM.evaluateDebug(self)
        @deprecated("Use evaluate instead", "0.8.4")
        def eval: Term = VM.evaluateTerm(self)

        /** Evaluate the term using the given VM.
          * @note
          *   This method just runs the CEK machine on the term. It does not follow Plutus
          *   specification like CIP-117
          *
          * @throws RuntimeException
          *   on evaluation error
          */
        def evaluate(using vm: PlutusVM): Term =
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(self))

        /** Evaluate the term using the given VM.
          * @note
          *   This method just runs the CEK machine on the term. It does not follow Plutus *
          *   specification like CIP-117
          *
          * @return
          *   [[Result]] with the evaluation result and the spent budget
          */
        def evaluateDebug(using vm: PlutusVM): Result =
            val spenderLogger = TallyingBudgetSpenderLogger(CountingBudgetSpender())
            try
                val result = vm.evaluateDeBruijnedTerm(
                  DeBruijn.deBruijnTerm(self),
                  spenderLogger,
                  spenderLogger
                )
                Result.Success(
                  result,
                  spenderLogger.getSpentBudget,
                  spenderLogger.costs.toMap,
                  spenderLogger.getLogsWithBudget
                )
            catch
                case e: Exception =>
                    Result.Failure(
                      e,
                      spenderLogger.getSpentBudget,
                      spenderLogger.costs.toMap,
                      spenderLogger.getLogsWithBudget
                    )
        def plutusV1: Program = Program.plutusV1(self)
        def plutusV2: Program = Program.plutusV2(self)
        def plutusV3: Program = Program.plutusV3(self)
}
