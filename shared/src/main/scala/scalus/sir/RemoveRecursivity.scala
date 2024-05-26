package scalus.sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.*

object RemoveRecursivity:

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def apply(sir: SIR): SIR = removeRecursivity(sir)

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def removeRecursivityExpr(sir: SIR): SIR =
        sir match
            case Let(Rec, List(Binding(name, binding)), body) if !isRecursive(name, binding) =>
                removeRecursivity(Let(NonRec, List(Binding(name, binding)), body))
            case Let(rec, bindings, body) =>
                Let(
                  rec,
                  bindings.map { case Binding(name, rhs) => Binding(name, removeRecursivity(rhs)) },
                  removeRecursivity(body)
                )
            case LamAbs(name, term) => LamAbs(name, removeRecursivity(term))
            case Apply(f, arg)      => Apply(removeRecursivity(f), removeRecursivity(arg))
            case IfThenElse(cond, t, f) =>
                IfThenElse(removeRecursivity(cond), removeRecursivity(t), removeRecursivity(f))
            case And(lhs, rhs) => And(removeRecursivity(lhs), removeRecursivity(rhs))
            case Or(lhs, rhs)  => Or(removeRecursivity(lhs), removeRecursivity(rhs))
            case Not(term)     => Not(removeRecursivity(term))
            case Match(scrutinee, cases) =>
                Match(
                  removeRecursivity(scrutinee),
                  cases.map { case Case(constr, bindings, body) =>
                      Case(constr, bindings, removeRecursivity(body))
                  }
                )
            case Constr(name, data, args) =>
                Constr(name, data, args.map(removeRecursivity))
            case Decl(name, term) => Decl(name, removeRecursivity(term))
            case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    def isRecursive(name: String, term: SIR, env: List[String] = Nil): Boolean =
        term match
            case Var(n)            => n == name && !env.contains(n)
            case ExternalVar(_, n) => n == name && !env.contains(n)
            case Let(_, bindings, body) =>
                val newEnv = bindings.map(_.name) ++ env
                bindings.exists(b => isRecursive(name, b.value, newEnv))
                || isRecursive(name, body, newEnv)
            case LamAbs(n, t) => isRecursive(name, t, n :: env)
            case Apply(f, a)  => isRecursive(name, f, env) || isRecursive(name, a, env)
            case And(a, b)    => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Or(a, b)     => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Not(a)       => isRecursive(name, a, env)
            case IfThenElse(c, t, f) =>
                isRecursive(name, c, env) || isRecursive(name, t, env) || isRecursive(name, f, env)
            case Decl(_, t) => isRecursive(name, t, env)
            case Constr(_, _, args) =>
                args.exists(a => isRecursive(name, a, env))
            case Match(scrutinee, cases) =>
                isRecursive(name, scrutinee, env) || cases.exists { case Case(_, bindings, body) =>
                    isRecursive(name, body, bindings ++ env)
                }
            case _: Builtin | _: Error | _: Const => false
