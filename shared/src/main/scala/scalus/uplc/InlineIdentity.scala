package scalus.uplc
import scalus.uplc.Term.*

/** Inlines identity function application */
object InlineIdentity:
    /** Inlines identity function application */
    def apply(term: Term): Term = inlineIdentity(term)

    /** Inlines identity function application */
    def inlineIdentity(term: Term): Term = term match
        case Apply(LamAbs(param, Var(NamedDeBruijn(name, _))), arg) if param == name =>
            inlineIdentity(arg)
        case Apply(f, arg)       => Apply(inlineIdentity(f), inlineIdentity(arg))
        case Force(term)         => Force(inlineIdentity(term))
        case Delay(term)         => Delay(inlineIdentity(term))
        case LamAbs(param, body) => LamAbs(param, inlineIdentity(body))
        case Constr(tag, args)   => Constr(tag, args.map(inlineIdentity))
        case Case(arg, cases)    => Case(inlineIdentity(arg), cases.map(inlineIdentity))
        case _                   => term
