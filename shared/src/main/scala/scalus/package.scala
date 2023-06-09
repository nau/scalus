import org.typelevel.paiges.Doc
import scalus.sir.PrettyPrinter
import scalus.sir.SIR
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Constant
import scalus.uplc.DefaultUni
import scalus.uplc.Program
import scalus.uplc.Term
package object scalus {
  extension (sir: SIR)
    def pretty: Doc = PrettyPrinter.pretty(sir)
    def doubleCborHex(version: (Int, Int, Int), generateErrorTraces: Boolean = false): String =
      val term = sir.toUplc(generateErrorTraces)
      Program(version, term).doubleCborHex

    def toUplc(generateErrorTraces: Boolean = false): Term =
      SimpleSirToUplcLowering(sir, generateErrorTraces).lower()

  extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
  extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
