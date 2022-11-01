package scalus.sir

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.uplc.Data.*
import scalus.uplc.{Constant, Data, DefaultFun, NamedDeBruijn}
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

case class Binding(name: String, value: SIR) {
  override def toString: String = s"Binding(\"$name\", $value)"
}

enum Recursivity:
  case NonRec, Rec

enum SIR:
  case Var(name: NamedDeBruijn) extends SIR
  case Let(recursivity: Recursivity, bindings: immutable.List[Binding], body: SIR) extends SIR
  case LamAbs(name: String, term: SIR) extends SIR
  case Apply(f: SIR, arg: SIR) extends SIR
  case Const(const: Constant) extends SIR
  case IfThenElse(cond: SIR, t: SIR, f: SIR) extends SIR
  case Builtin(bn: DefaultFun) extends SIR
  case Error(msg: String) extends SIR

  def pretty: Doc = this match
    case Var(name) => Doc.text(name.name)
    case Let(Recursivity.NonRec, immutable.List(Binding(name, body)), inExpr) =>
      Doc.text("let") & Doc.text(name) & Doc.text(
        "="
      ) & body.pretty & Doc.text("in") + Doc.lineOrSpace + inExpr.pretty
    case Let(Recursivity.Rec, immutable.List(Binding(name, body)), inExpr) =>
      val (args, body1) = TermDSL.lamAbsToList(body)
      val prettyArgs = Doc.intercalate(Doc.text(", "), args.map(Doc.text))
      Doc.text("fun") & Doc.text(name) + Doc.char('(') + prettyArgs + Doc
        .char(')') & Doc.char('=') / body1.pretty.indent(2) / Doc.text(
        "in"
      ) & inExpr.pretty
    case LamAbs(name, term) =>
      val (args, body1) = TermDSL.lamAbsToList(this)
      val prettyArgs = Doc.intercalate(Doc.space, args.map(Doc.text))
      Doc.text("{λ") & prettyArgs & Doc.text("->") + Doc.lineOrEmpty + body1.pretty.indent(2) + Doc
        .text("}")
    case a @ Apply(f, arg) =>
      val (t, args) = TermDSL.applyToList(a)
      val prettyArgs = args match
        case immutable.List() => Doc.text("()")
        case _ =>
          Doc.text("(") + Doc.intercalate(Doc.text(", "), args.map(_.pretty)) + Doc.text(")")

      t.pretty + prettyArgs
    case Const(const) => const.prettyValue
    case IfThenElse(cond, t, f) =>
      Doc.text("if")
        & cond.pretty
        + Doc.lineOrSpace + Doc.text("then")
        + t.pretty.indent(2)
        + Doc.lineOrSpace + Doc.text("else")
        + f.pretty.indent(2)
    case Builtin(bn) => bn.pretty
    case Error(_)    => Doc.text("ERROR")

object TermDSL:
  def applyToList(app: SIR): (SIR, immutable.List[SIR]) =
    app match
      case SIR.Apply(f, arg) =>
        val (f1, args) = applyToList(f)
        (f1, args :+ arg)
      case f => (f, Nil)

  // flatten LamAbs into a list of names and the body
  def lamAbsToList(lam: SIR): (immutable.List[String], SIR) =
    lam match
      case SIR.LamAbs(name, body) =>
        val (names, body1) = lamAbsToList(body)
        (name :: names, body1)
      case body => (Nil, body)

  def λ(names: String*)(term: SIR): SIR = lam(names: _*)(term)
  def lam(names: String*)(term: SIR): SIR = names.foldRight(term)(SIR.LamAbs(_, _))
  extension (term: SIR) def $(rhs: SIR) = SIR.Apply(term, rhs)

  given Conversion[DefaultFun, SIR] with
    def apply(bn: DefaultFun): SIR = SIR.Builtin(bn)

  given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIR] with
    def apply(c: A): SIR = SIR.Const(summon[Constant.LiftValue[A]].lift(c))

  given Conversion[Constant, SIR] with
    def apply(c: Constant): SIR = SIR.Const(c)

  given constantAsData[A: Data.ToData]: Conversion[A, Data] with
    def apply(c: A): Data = summon[Data.ToData[A]].toData(c)

case class Program(version: (Int, Int, Int), term: SIR):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")
