
import scalus.sir.SIR
import scalus.sir.PrettyPrinter
import org.typelevel.paiges.Doc
package object scalus {
  extension (sir: SIR)
    def pretty: Doc = PrettyPrinter.pretty(sir)
}
