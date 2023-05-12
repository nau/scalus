import scalus.sir.SIR
import scalus.sir.PrettyPrinter
import scalus.uplc.DefaultUni
import org.typelevel.paiges.Doc
import scalus.uplc.Constant
package object scalus {
  extension (sir: SIR) def pretty: Doc = PrettyPrinter.pretty(sir)
  extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
  extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
