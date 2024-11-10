import org.typelevel.paiges.Doc
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.sir.EtaReduce
import scalus.sir.OptimizingSirToUplcLowering
import scalus.sir.PrettyPrinter
import scalus.sir.PrettyPrinter.Style
import scalus.sir.RemoveRecursivity
import scalus.sir.SIR
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Constant
import scalus.uplc.DefaultUni
import scalus.uplc.Inliner
import scalus.uplc.Program
import scalus.uplc.Term
import scalus.uplc.eval.Result
import scalus.uplc.eval.VM
import scalus.utils.Utils
package object scalus {

    /** Pipe operator */
    extension [A](inline a: A) inline infix def |>[B](inline f: A => B): B = f(a)

    extension (sir: SIR)
        def pretty: Doc = PrettyPrinter.pretty(sir, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(sir, Style.XTerm)
        def show: String = pretty.render(80)
        def showHighlighted: String = sir.prettyXTerm.render(80)
        def doubleCborHex(version: (Int, Int, Int), generateErrorTraces: Boolean = false): String =
            val term = sir.toUplc(generateErrorTraces)
            Program(version, term).doubleCborHex

        def toUplc(generateErrorTraces: Boolean = false): Term =
            SimpleSirToUplcLowering(sir, generateErrorTraces).lower()
        def toUplcOptimized(generateErrorTraces: Boolean = false): Term =
            OptimizingSirToUplcLowering(sir |> RemoveRecursivity.apply, generateErrorTraces)
                .lower() |> EtaReduce.apply |> Inliner.inlinePass

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
            Utils.writePlutusFile(path, p, plutusVersion)
        def evalDebug: Result = VM.evaluateDebug(p.term)
        def eval: Term = VM.evaluateProgram(p)

    extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
    extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)

    extension (self: Term)
        def pretty: Doc = PrettyPrinter.pretty(self, Style.Normal)
        def prettyXTerm: Doc = PrettyPrinter.pretty(self, Style.XTerm)
        def show: String = self.pretty.render(80)
        def showHighlighted: String = self.prettyXTerm.render(80)
        def evalDebug: Result = VM.evaluateDebug(self)
        def eval: Term = VM.evaluateTerm(self)
}
