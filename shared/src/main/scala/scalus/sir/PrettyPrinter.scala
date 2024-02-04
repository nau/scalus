package scalus.sir

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.*
import scalus.uplc.Constant
import scalus.uplc.Data
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.utils.Utils

object PrettyPrinter {
    enum Style:
        case Normal, XTerm

    def inParens(d: Doc): Doc = Doc.char('(') + d + Doc.char(')')
    def pretty(df: DefaultFun): Doc = Doc.text(Utils.lowerFirst(df.toString))

    def prettyValue(c: Constant, dataParens: Boolean = false): Doc =
        import Constant.*
        c match
            case Integer(value)    => Doc.text(value.toString)
            case ByteString(value) => Doc.text("#" + value.toHex)
            case String(value)     => Doc.text("\"" + value + "\"")
            case Unit              => Doc.text("()")
            case Bool(value)       => Doc.text(if value then "True" else "False")
            case Constant.Data(value) =>
                if dataParens then inParens(pretty(value)) else pretty(value)
            case Pair(a, b) =>
                inParens(prettyValue(a) + Doc.text(", ") + prettyValue(b))
            case List(tpe, values) =>
                Doc.text("[") + Doc.intercalate(
                  Doc.text(", ") + Doc.space,
                  values.map(v => prettyValue(v))
                ) + Doc
                    .text("]")

    def pretty(d: Data): Doc =
        d match
            case Data.I(value) => Doc.text("I") + Doc.space + Doc.text(value.toString)
            case Data.B(value) => Doc.text("B") + Doc.space + Doc.text("#" + value.toHex)
            case Data.Constr(tag, args) =>
                Doc.text("Constr") + Doc.space + Doc.text(tag.toString) + Doc.space +
                    Doc.text("[") + Doc.intercalate(
                      Doc.text(",") + Doc.space,
                      args.map(pretty)
                    ) + Doc.text("]")
            case Data.List(values) =>
                Doc.text("List") + Doc.space + Doc.text("[") + Doc.intercalate(
                  Doc.text(",") + Doc.space,
                  values.map(pretty)
                ) + Doc.text("]")
            case Data.Map(entries) =>
                Doc.text("Map") + Doc.space + Doc.text("[") + Doc.intercalate(
                  Doc.text(",") + Doc.space,
                  entries.map { case (k, v) =>
                      inParens(pretty(k) + Doc.text(",") + Doc.space + pretty(v))
                  }
                ) + Doc.text("]")

    def pretty(c: Constant): Doc = pretty(c.tpe) + Doc.space + prettyValue(c, true)

    def pretty(du: DefaultUni): Doc = du match
        case DefaultUni.Integer    => Doc.text("integer")
        case DefaultUni.ByteString => Doc.text("bytestring")
        case DefaultUni.String     => Doc.text("string")
        case DefaultUni.Unit       => Doc.text("unit")
        case DefaultUni.Bool       => Doc.text("bool")
        case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
            inParens(Doc.text("list") + Doc.space + pretty(arg))
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
            inParens(Doc.text("pair") + Doc.space + pretty(a) + Doc.space + pretty(b))
        case DefaultUni.Data => Doc.text("data")
        case _               => sys.error(s"Unexpected default uni: $du")

    def pretty(sir: SIR, style: Style): Doc =
        import SIR.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == Style.XTerm then d.style(s) else d
        def kw(s: String): Doc = Doc.text(s).styled(Fg.colorCode(172))
        def ctr(s: String): Doc = Doc.text(s).styled(Fg.colorCode(21))
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
                    / pretty(term, style)
            case Constr(name, _, args) =>
                ctr(name) + Doc
                    .intercalate(
                      Doc.text(",") + Doc.line,
                      args.map(pretty(_, style))
                    )
                    .tightBracketBy(Doc.text("("), Doc.text(")"))
            case Match(scrutinee, cases) =>
                val prettyCases =
                    Doc.stack(cases.map { case Case(constr, bindings, body) =>
                        val params = bindings match
                            case Nil => Doc.empty
                            case _ =>
                                Doc
                                    .intercalate(Doc.text(",") + Doc.line, bindings.map(Doc.text))
                                    .tightBracketBy(Doc.text("("), Doc.text(")"))
                        (kw("case") & ctr(constr.name) + params & Doc.text(
                          "->"
                        ) + (Doc.line + pretty(body, style))
                            .nested(2)).grouped.aligned
                    })
                ((kw("match") & pretty(scrutinee, style) & kw(
                  "with"
                )).grouped + (Doc.line + prettyCases)
                    .nested(
                      2
                    )).aligned

            case Var(name)                     => Doc.text(name)
            case ExternalVar(moduleName, name) => Doc.text(name)
            case Let(Recursivity.NonRec, List(Binding(name, body)), inExpr) =>
                pretty(body, style).bracketBy(
                  kw("let") & Doc.text(name) & Doc.text("="),
                  kw("in")
                ) + Doc.line + pretty(inExpr, style)
            case Let(Recursivity.Rec, List(Binding(name, body)), inExpr) =>
                val (args, body1) = SirDSL.lamAbsToList(body)
                val prettyArgs = Doc.stack(args.map(Doc.text))
                val signatureLine =
                    (kw("fun") & Doc
                        .text(name) + (Doc.line + prettyArgs & Doc.char('=')).nested(2)).grouped
                (signatureLine + (Doc.line + pretty(body1, style))
                    .nested(4)
                    .grouped).grouped.aligned / kw(
                  "in"
                ) & pretty(inExpr, style)
            case Let(_, _, inExpr) => ???
            case LamAbs(name, term) =>
                val (args, body1) = SirDSL.lamAbsToList(sir)
                val prettyArgs = Doc.stack(args.map(Doc.text))
                val decl =
                    (Doc.text("{λ") + (Doc.line + prettyArgs & Doc.text("->")).nested(4)).grouped
                ((decl + (Doc.line + pretty(body1, style)).nested(2)).grouped / Doc.text(
                  "}"
                )).grouped.aligned
            case a @ Apply(f, arg) =>
                val (t, args) = SirDSL.applyToList(a)
                val prettyArgs = args match
                    case List() => Doc.text("()")
                    case _ =>
                        Doc
                            .intercalate(Doc.text(",") + Doc.line, args.map(pretty(_, style)))
                            .tightBracketBy(Doc.text("("), Doc.text(")"))

                pretty(t, style) + prettyArgs
            case Const(const) => prettyValue(const).styled(Fg.colorCode(64))
            case And(a, b)    =>
                // We don't add parentheses for nested Ands, because they are associative.
                // But we add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => Doc.char('(') + pretty(a, style) + Doc.char(')')
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => Doc.char('(') + pretty(b, style) + Doc.char(')')
                    case _              => pretty(b, style)
                }
                (docA + Doc.line + kw("and") + Doc.line + docB).grouped.aligned

            case Or(a, b) =>
                // We add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => Doc.char('(') + pretty(a, style) + Doc.char(')')
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => Doc.char('(') + pretty(b, style) + Doc.char(')')
                    case _              => pretty(b, style)
                }
                (docA + Doc.line + kw("or") + Doc.line + docB).grouped.aligned

            case Not(a) =>
                // We add parentheses for nested Nots, Ands, and Ors.
                val docA = a match {
                    case _: Not | _: And | _: Or => Doc.char('(') + pretty(a, style) + Doc.char(')')
                    case _                       => pretty(a, style)
                }
                (kw("not") + Doc.line + docA).grouped.aligned

            case IfThenElse(cond, t, f) =>
                ((kw("if") + (Doc.line + pretty(cond, style)).nested(4)).grouped
                    + (Doc.line + kw("then") + (Doc.line + pretty(t, style)).nested(4)).grouped
                    + (Doc.line + kw("else") + (Doc.line + pretty(f, style)).nested(
                      4
                    )).grouped).aligned
            case Builtin(bn) => pretty(bn).styled(Fg.colorCode(176))
            case Error(msg)  => Doc.text(s"ERROR '$msg'").styled(Fg.colorCode(124))

    def pretty(p: Program): Doc =
        val (major, minor, patch) = p.version
        Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
          s"$major.$minor.$patch"
        ) + Doc.space + pretty(p.term, Style.Normal) + Doc.text(")")
}
