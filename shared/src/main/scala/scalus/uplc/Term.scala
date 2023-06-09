package scalus.uplc

import org.typelevel.paiges.Doc
import scalus.*
import scalus.sir.PrettyPrinter
import scalus.uplc.Data.*
import io.bullet.borer.Cbor
import scalus.utils.Hex

case class NamedDeBruijn(name: String, index: Int = 0):
  assert(index >= 0)
  override def toString: String =
    if index == 0 then s"NamedDeBruijn(\"$name\")"
    else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
  case Var(name: NamedDeBruijn) extends Term
  case LamAbs(name: String, term: Term) extends Term
  case Apply(f: Term, arg: Term) extends Term
  case Force(term: Term) extends Term
  case Delay(term: Term) extends Term
  case Const(const: Constant) extends Term
  case Builtin(bn: DefaultFun) extends Term
  case Error extends Term

  def pretty: Doc = this match
    case Var(name) => Doc.text(name.name)
    case LamAbs(name, term) =>
      Doc.text("(") + Doc.text("lam") + Doc.space + Doc.text(name) + Doc.line + term.pretty.indent(
        2
      ) + Doc.text(")")
    case a @ Apply(f, arg) =>
      Doc.text("[") + f.pretty + Doc.space + arg.pretty + Doc.text("]")
    case Force(term) =>
      Doc.text("(") + Doc.text("force") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Delay(term) =>
      Doc.text("(") + Doc.text("delay") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Const(const) => Doc.text("(") + Doc.text("con") + Doc.space + const.pretty + Doc.text(")")
    case Builtin(bn) =>
      Doc.text("(") + Doc.text("builtin") + Doc.space + PrettyPrinter.pretty(bn) + Doc.text(")")
    case Error => Doc.text("(error)")

case class Program(version: (Int, Int, Int), term: Term):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")

  lazy val flatEncoded = ProgramFlatCodec.encodeFlat(this)
  lazy val cborEncoded = Cbor.encode(flatEncoded).toByteArray
  lazy val doubleCborEncoded = Cbor.encode(cborEncoded).toByteArray
  lazy val doubleCborHex = Hex.bytesToHex(doubleCborEncoded)

case class DeBruijnedProgram private[uplc] (version: (Int, Int, Int), term: Term):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")
