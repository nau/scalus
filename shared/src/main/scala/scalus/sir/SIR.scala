package scalus.sir

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{DataItem as DI, Decoder, Encoder, Reader, Writer}
import org.typelevel.paiges.{Doc, Style}
import scalus.uplc.{Constant, Data, DefaultFun, NamedDeBruijn}
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util

case class Binding(name: String, value: SIR) {
  override def toString: String = s"Binding(\"$name\", $value)"
}

enum Recursivity:
  case NonRec, Rec

case class ConstrDecl(name: String, params: List[String])
case class DataDecl(name: String, constructors: List[ConstrDecl])
case class Case(constr: ConstrDecl, bindings: List[String], body: SIR)

enum SIR:
  case Var(name: NamedDeBruijn) extends SIR
  case Let(recursivity: Recursivity, bindings: List[Binding], body: SIR) extends SIR
  case LamAbs(name: String, term: SIR) extends SIR
  case Apply(f: SIR, arg: SIR) extends SIR
  case Const(const: Constant) extends SIR
  case IfThenElse(cond: SIR, t: SIR, f: SIR) extends SIR
  case Builtin(bn: DefaultFun) extends SIR
  case Error(msg: String) extends SIR
  case Decl(data: DataDecl, term: SIR) extends SIR
  case Constr(name: String, data: DataDecl, args: List[SIR]) extends SIR
  case Match(scrutinee: SIR, cases: List[Case]) extends SIR

  def pretty: Doc =
    def kw(s: String): Doc = Doc.text(s).style(Style.XTerm.Fg.colorCode(172))
    def ctr(s: String): Doc = Doc.text(s).style(Style.XTerm.Fg.colorCode(21))
    this match
      case Decl(DataDecl(name, constructors), term) =>
        val prettyConstrs = constructors.map { constr =>
          val params = constr.params match
            case Nil => Doc.empty
            case _ =>
              Doc
                .intercalate(
                  Doc.text(",") + Doc.line,
                  constr.params.map(Doc.text)
                )
                .tightBracketBy(Doc.text("("), Doc.text(")"))
          (ctr(constr.name) + params).aligned
        }
        kw("data") & Doc.text(name) &
          (Doc.text("=") & Doc.intercalate(
            Doc.line + Doc.text("|") + Doc.space,
            prettyConstrs
          )).grouped.aligned
          / term.pretty
      case Constr(name, _, args) =>
        ctr(name).style(Style.XTerm.Fg.colorCode(21)) + Doc
          .intercalate(
            Doc.text(",") + Doc.line,
            args.map(_.pretty)
          )
          .tightBracketBy(Doc.text("("), Doc.text(")"))
      case Match(scrutinee, cases) =>
        val prettyCases = Doc.stack(cases.map { case Case(constr, bindings, body) =>
          val params = bindings match
            case Nil => Doc.empty
            case _ =>
              Doc
                .intercalate(Doc.text(",") + Doc.line, bindings.map(Doc.text))
                .tightBracketBy(Doc.text("("), Doc.text(")"))
          (kw("case") & ctr(constr.name) + params & Doc.text("->") + (Doc.line + body.pretty)
            .nested(2)).grouped.aligned
        })
        ((kw("match") & scrutinee.pretty & kw("with")).grouped + (Doc.line + prettyCases).nested(
          2
        )).aligned

      case Var(name) => Doc.text(name.name)
      case Let(Recursivity.NonRec, List(Binding(name, body)), inExpr) =>
        body.pretty.bracketBy(
          kw("let") & Doc.text(name) & Doc.text("="),
          kw("in")
        ) + Doc.line + inExpr.pretty
      case Let(Recursivity.Rec, List(Binding(name, body)), inExpr) =>
        val (args, body1) = TermDSL.lamAbsToList(body)
        val prettyArgs = Doc.stack(args.map(Doc.text))
        val signatureLine =
          (kw("fun") & Doc.text(name) + (Doc.line + prettyArgs & Doc.char('=')).nested(2)).grouped
        (signatureLine + (Doc.line + body1.pretty).nested(4).grouped).grouped.aligned / kw(
          "in"
        ) & inExpr.pretty
      case LamAbs(name, term) =>
        val (args, body1) = TermDSL.lamAbsToList(this)
        val prettyArgs = Doc.stack(args.map(Doc.text))
        val decl = (Doc.text("{λ") + (Doc.line + prettyArgs & Doc.text("->")).nested(4)).grouped
        ((decl + (Doc.line + body1.pretty).nested(2)).grouped / Doc.text("}")).grouped.aligned
      case a @ Apply(f, arg) =>
        val (t, args) = TermDSL.applyToList(a)
        val prettyArgs = args match
          case List() => Doc.text("()")
          case _ =>
            Doc
              .intercalate(Doc.text(",") + Doc.line, args.map(_.pretty))
              .tightBracketBy(Doc.text("("), Doc.text(")"))

        t.pretty + prettyArgs
      case Const(const) => const.prettyValue.style(Style.XTerm.Fg.colorCode(64))
      case IfThenElse(cond, t, f) =>
        ((kw("if") + (Doc.line + cond.pretty).nested(4)).grouped
          + (Doc.line + kw("then") + (Doc.line + t.pretty).nested(4)).grouped
          + (Doc.line + kw("else") + (Doc.line + f.pretty).nested(4)).grouped).aligned
      case Builtin(bn) => bn.pretty.style(Style.XTerm.Fg.colorCode(176))
      case Error(msg)    => Doc.text(s"ERROR '$msg'").style(Style.XTerm.Fg.colorCode(124))

object TermDSL:
  def applyToList(app: SIR): (SIR, List[SIR]) =
    app match
      case SIR.Apply(f, arg) =>
        val (f1, args) = applyToList(f)
        (f1, args :+ arg)
      case f => (f, Nil)

  // flatten LamAbs into a list of names and the body
  def lamAbsToList(lam: SIR): (List[String], SIR) =
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
