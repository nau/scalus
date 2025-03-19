package scalus.sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.*

object RemoveRecursivity:

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def apply(sir: SIR): SIR = removeRecursivity(sir)

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def removeRecursivity(sir: SIR): SIR =
        sir match
            case Let(Rec, List(Binding(name, binding)), body, anns)
                if !isRecursive(name, binding) =>
                removeRecursivity(Let(NonRec, List(Binding(name, binding)), body, anns))
            case Let(rec, bindings, body, anns) =>
                Let(
                  rec,
                  bindings.map { case Binding(name, rhs) =>
                      Binding(name, removeRecursivity(rhs))
                  },
                  removeRecursivity(body),
                  anns
                )
            case LamAbs(name, term, anns) => LamAbs(name, removeRecursivity(term), anns)
            case Apply(f, arg, tp, anns) =>
                Apply(removeRecursivity(f), removeRecursivity(arg), tp, anns)
            case Select(s, field, tp, anns) => Select(removeRecursivity(s), field, tp, anns)
            case IfThenElse(cond, t, f, tp, anns) =>
                IfThenElse(
                  removeRecursivity(cond),
                  removeRecursivity(t),
                  removeRecursivity(f),
                  tp,
                  anns
                )
            case And(lhs, rhs) => And(removeRecursivity(lhs), removeRecursivity(rhs))
            case Or(lhs, rhs)  => Or(removeRecursivity(lhs), removeRecursivity(rhs))
            case Not(term)     => Not(removeRecursivity(term))
            case Match(scrutinee, cases, tp, anns) =>
                Match(
                  removeRecursivity(scrutinee),
                  cases.map {
                      case SIR.Case(Pattern.Constr(constr, bindings, typeBindings), body) =>
                          Case(
                            Pattern.Constr(constr, bindings, typeBindings),
                            removeRecursivity(body)
                          )
                      case SIR.Case(Pattern.Wildcard, body) =>
                          Case(Pattern.Wildcard, removeRecursivity(body))
                  },
                  tp,
                  anns
                )
            case Constr(name, data, args, tp, anns) =>
                Constr(name, data, args.map(removeRecursivity), tp, anns)
            case Decl(data, term) => Decl(data, removeRecursivity(term))
            case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    def isRecursive(name: String, term: SIR, env: List[String] = Nil): Boolean =
        term match
            case Var(n, tp, _)            => n == name && !env.contains(n)
            case ExternalVar(_, n, tp, _) => n == name && !env.contains(n)
            case Let(_, bindings, body, _) =>
                val newEnv = bindings.map(_.name) ++ env
                bindings.exists(b => isRecursive(name, b.value, newEnv))
                || isRecursive(name, body, newEnv)
            case LamAbs(n, t, _)    => isRecursive(name, t, n.name :: env)
            case Apply(f, a, tp, _) => isRecursive(name, f, env) || isRecursive(name, a, env)
            case Select(s, _, _, _) => isRecursive(name, s, env)
            case And(a, b)          => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Or(a, b)           => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Not(a)             => isRecursive(name, a, env)
            case IfThenElse(c, t, f, tp, _) =>
                isRecursive(name, c, env) || isRecursive(name, t, env) || isRecursive(name, f, env)
            case Decl(_, t) => isRecursive(name, t, env)
            case Constr(_, _, args, tp, _) =>
                args.exists(a => isRecursive(name, a, env))
            case Match(scrutinee, cases, tp, _) =>
                isRecursive(name, scrutinee, env) || cases.exists {
                    case SIR.Case(Pattern.Constr(_, bindings, typeBindings), body) =>
                        isRecursive(name, body, bindings ++ env)
                    case SIR.Case(Pattern.Wildcard, body) =>
                        isRecursive(name, body, env)
                }
            case _: Builtin | _: Error | _: Const => false
