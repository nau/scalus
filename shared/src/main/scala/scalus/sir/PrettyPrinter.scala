package scalus.sir

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.*
import scalus.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.uplc.Term
import scalus.uplc.TermDSL
import scalus.utils.Utils

/** Pretty printers.
  *
  *   - Use `pretty` to pretty print to a [[org.typelevel.paiges.Doc]].
  *   - Use `prettyXTerm` to pretty print to a [[org.typelevel.paiges.Doc]] with syntax highlighting
  *     for XTerm.
  *
  * For example:
  * {{{
  *  PrettyPrinter.prettyXTerm(sir).render(80)
  *  // renders a single line if possible
  *  PrettyPrinter.pretty(sir).flatten.render(120)
  * }}}
  */
object PrettyPrinter:
    enum Style:
        case Normal, XTerm

    def inParens(d: Doc): Doc = char('(') + d + char(')')
    def pretty(df: DefaultFun): Doc = text(Utils.lowerFirst(df.toString))

    def prettyValue(c: Constant, dataParens: Boolean = false): Doc =
        import Constant.*
        c match
            case Integer(value)    => str(value)
            case ByteString(value) => text("#" + value.toHex)
            case String(value)     => text("\"" + value + "\"")
            case Unit              => text("()")
            case Bool(value)       => text(if value then "True" else "False")
            case Constant.Data(value) =>
                if dataParens then inParens(pretty(value)) else pretty(value)
            case Pair(a, b) =>
                inParens(prettyValue(a) + text(", ") + prettyValue(b))
            case List(tpe, values) =>
                text("[") + intercalate(
                  text(", ") + space,
                  values.map(v => prettyValue(v))
                ) + text("]")
            case BLS12_381_G1_Element(value) => text(s"0x${value.value.toHex}")
            case BLS12_381_G2_Element(value) => text(s"0x${value.value.toHex}")
            case BLS12_381_MlResult(_) =>
                throw new IllegalArgumentException("Cannot print to BLS12_381_MlResult")

    def pretty(d: Data): Doc =
        d match
            case Data.I(value) => text("I") & str(value)
            case Data.B(value) => text("B") & text("#" + value.toHex)
            case Data.Constr(tag, args) =>
                text("Constr") & str(tag) &
                    text("[") + intercalate(
                      text(",") + space,
                      args.map(pretty)
                    ) + text("]")
            case Data.List(values) =>
                text("List") & text("[") + intercalate(
                  text(",") + space,
                  values.map(pretty)
                ) + text("]")
            case Data.Map(entries) =>
                text("Map") & text("[") + intercalate(
                  text(",") + space,
                  entries.map { case (k, v) =>
                      inParens(pretty(k) + text(",") & pretty(v))
                  }
                ) + text("]")

    def pretty(c: Constant): Doc = pretty(c.tpe) & prettyValue(c, true)

    def pretty(du: DefaultUni): Doc = du match
        case DefaultUni.Integer    => text("integer")
        case DefaultUni.ByteString => text("bytestring")
        case DefaultUni.String     => text("string")
        case DefaultUni.Unit       => text("unit")
        case DefaultUni.Bool       => text("bool")
        case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
            inParens(text("list") & pretty(arg))
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
            inParens(text("pair") & pretty(a) & pretty(b))
        case DefaultUni.Data                 => text("data")
        case DefaultUni.BLS12_381_G1_Element => text("bls12_381_G1_element")
        case DefaultUni.BLS12_381_G2_Element => text("bls12_381_G2_element")
        case _                               => sys.error(s"Unexpected default uni: $du")

    def pretty(sir: SIR, style: Style): Doc =
        import SIR.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == Style.XTerm then d.style(s) else d
        def kw(s: String): Doc = text(s).styled(Fg.colorCode(172))
        def ctr(s: String): Doc = text(s).styled(Fg.colorCode(21))
        sir match
            case Decl(DataDecl(name, constructors), term) =>
                val prettyConstrs = constructors.map { constr =>
                    val params = constr.params match
                        case Nil => empty
                        case _ =>
                            intercalate(
                              text(",") + line,
                              constr.params.map(text)
                            )
                                .tightBracketBy(text("("), text(")"))
                    (ctr(constr.name) + params).aligned
                }
                kw("data") & text(name) &
                    (text("=") & intercalate(
                      line + text("|") + space,
                      prettyConstrs
                    )).grouped.aligned
                    / pretty(term, style)
            case Constr(name, _, args) =>
                ctr(name) + intercalate(
                  text(",") + line,
                  args.map(pretty(_, style))
                )
                    .tightBracketBy(text("("), text(")"))
            case Match(scrutinee, cases) =>
                val prettyCases =
                    stack(cases.map { case Case(constr, bindings, body) =>
                        val params = bindings match
                            case Nil => empty
                            case _ =>
                                intercalate(text(",") + line, bindings.map(text))
                                    .tightBracketBy(text("("), text(")"))
                        (kw("case") & ctr(constr.name) + params & text(
                          "->"
                        ) + (line + pretty(body, style))
                            .nested(2)).grouped.aligned
                    })
                ((kw("match") & pretty(scrutinee, style) & kw(
                  "with"
                )).grouped + (line + prettyCases)
                    .nested(
                      2
                    )).aligned

            case Var(name)                     => text(name)
            case ExternalVar(moduleName, name) => text(name)
            case Let(Recursivity.NonRec, List(Binding(name, body)), inExpr) =>
                pretty(body, style).bracketBy(
                  kw("let") & text(name) & text("="),
                  kw("in")
                ) / pretty(inExpr, style)
            case Let(Recursivity.Rec, List(Binding(name, body)), inExpr) =>
                val (args, body1) = SirDSL.lamAbsToList(body)
                val prettyArgs = stack(args.map(text))
                val signatureLine =
                    (kw("fun") & text(name) + (line + prettyArgs & char('=')).nested(2)).grouped
                (signatureLine + (line + pretty(body1, style))
                    .nested(4)
                    .grouped).grouped.aligned / kw(
                  "in"
                ) & pretty(inExpr, style)
            // TODO: support multiple bindings
            case Let(_, _, inExpr) => sys.error(s"Multiple bindings not supported: $sir")
            case LamAbs(name, term) =>
                val (args, body1) = SirDSL.lamAbsToList(sir)
                val prettyArgs = stack(args.map(text))
                val decl =
                    (text("{λ") + (line + prettyArgs & text("->")).nested(4)).grouped
                ((decl + (line + pretty(body1, style)).nested(2)).grouped / text(
                  "}"
                )).grouped.aligned
            case a @ Apply(f, arg) =>
                val (t, args) = SirDSL.applyToList(a)
                val prettyArgs = args match
                    case List() => text("()")
                    case _ =>
                        intercalate(text(",") + line, args.map(pretty(_, style)))
                            .tightBracketBy(text("("), text(")"))

                pretty(t, style) + prettyArgs
            case Const(const) => prettyValue(const).styled(Fg.colorCode(64))
            case And(a, b)    =>
                // We don't add parentheses for nested Ands, because they are associative.
                // But we add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => inParens(pretty(a, style))
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => inParens(pretty(b, style))
                    case _              => pretty(b, style)
                }
                (docA / kw("and") / docB).grouped.aligned

            case Or(a, b) =>
                // We add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => inParens(pretty(a, style))
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => inParens(pretty(b, style))
                    case _              => pretty(b, style)
                }
                (docA / kw("or") / docB).grouped.aligned

            case Not(a) =>
                // We add parentheses for nested Nots, Ands, and Ors.
                val docA = a match {
                    case _: Not | _: And | _: Or => inParens(pretty(a, style) + line)
                    case _                       => pretty(a, style)
                }
                (kw("not") / docA).grouped.aligned

            case IfThenElse(cond, t, f) =>
                ((kw("if") + (line + pretty(cond, style)).nested(4)).grouped
                    + (line + kw("then") + (line + pretty(t, style)).nested(4)).grouped
                    + (line + kw("else") + (line + pretty(f, style)).nested(
                      4
                    )).grouped).aligned
            case Builtin(bn) => pretty(bn).styled(Fg.colorCode(176))
            case Error(msg)  => text(s"ERROR '$msg'").styled(Fg.colorCode(124))

    def pretty(p: Program): Doc =
        val (major, minor, patch) = p.version
        inParens(
          text("program") & text(
            s"$major.$minor.$patch"
          ) & pretty(p.term, Style.Normal)
        )

    def pretty(term: Term, style: Style): Doc =
        import Term.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == Style.XTerm then d.style(s) else d
        def kw(s: String): Doc = text(s).styled(Fg.colorCode(172))
        term match
            case Var(name) => text(name.name)
            case LamAbs(name, term) =>
                inParens(kw("lam") & text(name) / pretty(term, style).indent(2))
            case a @ Apply(f, arg) =>
                val (t, args) = TermDSL.applyToList(a)
                intercalate(lineOrSpace, (t :: args).map(pretty(_, style)))
                    .tightBracketBy(text("["), text("]"))
            case Force(term) =>
                inParens(kw("force") & pretty(term, style))
            case Delay(term) =>
                inParens(kw("delay") & pretty(term, style))
            case Const(const) =>
                inParens(kw("con") & const.pretty.styled(Fg.colorCode(64)))
            case Builtin(bn) =>
                inParens(kw("builtin") & pretty(bn).styled(Fg.colorCode(176)))
            case Error => kw("(error)")
            case Constr(tag, args) =>
                val prettyArgs = intercalate(
                  lineOrSpace,
                  args.map(pretty(_, style))
                )
                inParens(kw("constr") & str(tag) & prettyArgs)
            case Case(arg, cases) =>
                val prettyCases = stack(cases.map(pretty(_, style)))
                inParens(kw("case") & pretty(arg, style) / prettyCases.indent(2))

    def pretty(program: uplc.Program, style: Style): Doc =
        val (major, minor, patch) = program.version
        (text("program") / text(s"$major.$minor.$patch") / pretty(program.term, style))
            .tightBracketBy(
              text("("),
              text(")")
            )
