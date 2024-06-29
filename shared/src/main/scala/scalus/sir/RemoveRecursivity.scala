package scalus.sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.*

object RemoveRecursivity:

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def apply(sir: SIR): SIR = removeRecursivity(sir)

    def removeRecursivity(sir: SIR): SIR =
        sir match
            case decl: SIRDef => removeRecursivityDecl(decl)
            case term: SIRExpr  => removeRecursivityExpr(term)

    def removeRecursivityDecl(sir: SIRDef): SIRDef =
        sir match
            case SIR.Decl(name, term) => SIR.Decl(name, removeRecursivity(term))

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def removeRecursivityExpr(sir: SIRExpr): SIRExpr =
        sir match
            case Let(Rec, List(Binding(name, binding)), body) if !isRecursive(name, binding) =>
                removeRecursivityExpr(Let(NonRec, List(Binding(name, binding)), body))
            case Let(rec, bindings, body) =>
                Let(
                  rec,
                  bindings.map { case Binding(name, rhs) => Binding(name, removeRecursivityExpr(rhs)) },
                  removeRecursivityExpr(body)
                )
            case LamAbs(name, term) => LamAbs(name, removeRecursivityExpr(term))
            case Apply(f, arg, tp)      => Apply(removeRecursivityExpr(f), removeRecursivityExpr(arg), tp)
            case IfThenElse(cond, t, f, tp) =>
                IfThenElse(removeRecursivityExpr(cond), removeRecursivityExpr(t), removeRecursivityExpr(f), tp)
            case And(lhs, rhs) => And(removeRecursivityExpr(lhs), removeRecursivityExpr(rhs))
            case Or(lhs, rhs)  => Or(removeRecursivityExpr(lhs), removeRecursivityExpr(rhs))
            case Not(term)     => Not(removeRecursivityExpr(term))
            case Match(scrutinee, cases, tp) =>
                Match(
                  removeRecursivityExpr(scrutinee),
                  cases.map { case SIR.Case(constr, bindings, typeBindings, body) =>
                      Case(constr, bindings, typeBindings, removeRecursivityExpr(body))
                  },
                    tp
                )
            case Constr(name, data, args) =>
                Constr(name, data, args.map(removeRecursivityExpr))
            case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    def isRecursive(name: String, term: SIR, env: List[String] = Nil): Boolean =
        term match
            case Var(n, tp)            => n == name && !env.contains(n)
            case ExternalVar(_, n, tp) => n == name && !env.contains(n)
            case Let(_, bindings, body) =>
                val newEnv = bindings.map(_.name) ++ env
                bindings.exists(b => isRecursive(name, b.value, newEnv))
                || isRecursive(name, body, newEnv)
            case LamAbs(n, t) => isRecursive(name, t, n.name :: env)
            case Apply(f, a, tp)  => isRecursive(name, f, env) || isRecursive(name, a, env)
            case And(a, b)    => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Or(a, b)     => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Not(a)       => isRecursive(name, a, env)
            case IfThenElse(c, t, f, tp) =>
                isRecursive(name, c, env) || isRecursive(name, t, env) || isRecursive(name, f, env)
            case Decl(_, t) => isRecursive(name, t, env)
            case Constr(_, _, args) =>
                args.exists(a => isRecursive(name, a, env))
            case Match(scrutinee, cases, tp) =>
                isRecursive(name, scrutinee, env) || cases.exists { case SIR.Case(_, bindings, typeBindings, body) =>
                    isRecursive(name, body, bindings ++ env)
                }
            case _: Builtin | _: Error | _: Const => false

