package scalus.sir

import io.bullet.borer.Cbor
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Style
import scalus.*
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.uplc.PlutusDataCborEncoder
import scalus.utils.Utils

object PrettyPrinter {
  def pretty(df: DefaultFun): Doc = Doc.text(Utils.lowerFirst(df.toString))

  def prettyValue(c: Constant): Doc =
    import Constant.*
    c match
      case Integer(value)    => Doc.text(value.toString)
      case ByteString(value) => Doc.text("#" + value.toHex)
      case String(value)     => Doc.text("\"" + value + "\"")
      case Unit              => Doc.text("()")
      case Bool(value)       => Doc.text(if value then "True" else "False")
      case Data(value) =>
        implicit val encoder = PlutusDataCborEncoder
        val byteArray = Cbor.encode(value).toByteArray
        Doc.text("#" + Utils.bytesToHex(byteArray))
      case Pair(a, b) =>
        Doc.text("(") + prettyValue(a) + Doc.text(", ") + prettyValue(b) + Doc.text(")")
      case List(tpe, values) =>
        Doc.text("[") + Doc.intercalate(Doc.text(", ") + Doc.space, values.map(prettyValue)) + Doc
          .text("]")

  def pretty(c: Constant): Doc = pretty(c.tpe) + Doc.space + prettyValue(c)

  def pretty(du: DefaultUni): Doc = du match
    case DefaultUni.Integer    => Doc.text("integer")
    case DefaultUni.ByteString => Doc.text("bytestring")
    case DefaultUni.String     => Doc.text("string")
    case DefaultUni.Unit       => Doc.text("unit")
    case DefaultUni.Bool       => Doc.text("bool")
    case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
      Doc.text("(") + Doc.text("list") + Doc.space + pretty(arg) + Doc.text(")")
    case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
      Doc.text("(") + Doc.text("pair") + Doc.space + pretty(a) + Doc.space + pretty(b) + Doc.text(
        ")"
      )
    case DefaultUni.Data => Doc.text("data")
    case _               => sys.error(s"Unexpected default uni: $du")

  def pretty(sir: SIR): Doc =
    import SIR.*
    def kw(s: String): Doc = Doc.text(s).style(Style.XTerm.Fg.colorCode(172))
    def ctr(s: String): Doc = Doc.text(s).style(Style.XTerm.Fg.colorCode(21))
    sir match
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
          / pretty(term)
      case Constr(name, _, args) =>
        ctr(name).style(Style.XTerm.Fg.colorCode(21)) + Doc
          .intercalate(
            Doc.text(",") + Doc.line,
            args.map(pretty)
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
          (kw("case") & ctr(constr.name) + params & Doc.text("->") + (Doc.line + pretty(body))
            .nested(2)).grouped.aligned
        })
        ((kw("match") & pretty(scrutinee) & kw("with")).grouped + (Doc.line + prettyCases).nested(
          2
        )).aligned

      case Var(name)                     => Doc.text(name)
      case ExternalVar(moduleName, name) => Doc.text(moduleName + "::" + name)
      case Let(Recursivity.NonRec, List(Binding(name, body)), inExpr) =>
        pretty(body).bracketBy(
          kw("let") & Doc.text(name) & Doc.text("="),
          kw("in")
        ) + Doc.line + pretty(inExpr)
      case Let(Recursivity.Rec, List(Binding(name, body)), inExpr) =>
        val (args, body1) = TermDSL.lamAbsToList(body)
        val prettyArgs = Doc.stack(args.map(Doc.text))
        val signatureLine =
          (kw("fun") & Doc.text(name) + (Doc.line + prettyArgs & Doc.char('=')).nested(2)).grouped
        (signatureLine + (Doc.line + pretty(body1)).nested(4).grouped).grouped.aligned / kw(
          "in"
        ) & pretty(inExpr)
      case Let(_, _, inExpr) => ???
      case LamAbs(name, term) =>
        val (args, body1) = TermDSL.lamAbsToList(sir)
        val prettyArgs = Doc.stack(args.map(Doc.text))
        val decl = (Doc.text("{λ") + (Doc.line + prettyArgs & Doc.text("->")).nested(4)).grouped
        ((decl + (Doc.line + pretty(body1)).nested(2)).grouped / Doc.text("}")).grouped.aligned
      case a @ Apply(f, arg) =>
        val (t, args) = TermDSL.applyToList(a)
        val prettyArgs = args match
          case List() => Doc.text("()")
          case _ =>
            Doc
              .intercalate(Doc.text(",") + Doc.line, args.map(pretty))
              .tightBracketBy(Doc.text("("), Doc.text(")"))

        pretty(t) + prettyArgs
      case Const(const) => prettyValue(const).style(Style.XTerm.Fg.colorCode(64))
      case And(a, b)    =>
        // We don't add parentheses for nested Ands, because they are associative.
        // But we add parentheses for nested Ors and Nots.
        val docA = a match {
          case _: Or | _: Not => Doc.char('(') + pretty(a) + Doc.char(')')
          case _              => pretty(a)
        }
        val docB = b match {
          case _: Or | _: Not => Doc.char('(') + pretty(b) + Doc.char(')')
          case _              => pretty(b)
        }
        (docA + Doc.line + kw("and") + Doc.line + docB).grouped.aligned

      case Or(a, b) =>
        // We add parentheses for nested Ors and Nots.
        val docA = a match {
          case _: Or | _: Not => Doc.char('(') + pretty(a) + Doc.char(')')
          case _              => pretty(a)
        }
        val docB = b match {
          case _: Or | _: Not => Doc.char('(') + pretty(b) + Doc.char(')')
          case _              => pretty(b)
        }
        (docA + Doc.line + kw("or") + Doc.line + docB).grouped.aligned

      case Not(a) =>
        // We add parentheses for nested Nots, Ands, and Ors.
        val docA = a match {
          case _: Not | _: And | _: Or => Doc.char('(') + pretty(a) + Doc.char(')')
          case _                       => pretty(a)
        }
        (kw("not") + Doc.line + docA).grouped.aligned

      case IfThenElse(cond, t, f) =>
        ((kw("if") + (Doc.line + pretty(cond)).nested(4)).grouped
          + (Doc.line + kw("then") + (Doc.line + pretty(t)).nested(4)).grouped
          + (Doc.line + kw("else") + (Doc.line + pretty(f)).nested(4)).grouped).aligned
      case Builtin(bn) => pretty(bn).style(Style.XTerm.Fg.colorCode(176))
      case Error(msg)  => Doc.text(s"ERROR '$msg'").style(Style.XTerm.Fg.colorCode(124))

  def pretty(p: Program): Doc =
    val (major, minor, patch) = p.version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + pretty(p.term) + Doc.text(")")
}
