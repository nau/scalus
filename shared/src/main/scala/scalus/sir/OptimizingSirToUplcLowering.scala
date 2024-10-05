package scalus
package sir

import scalus.sir.Recursivity.*
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.ExprBuilder
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.TermDSL.*
import scalus.uplc.TypeScheme
import scalus.uplc.given

import scala.collection.mutable.HashMap
import scala.collection.mutable.TreeSet

enum ForceBuiltins:
    case None, AllUsed
    case Only(builtins: Set[DefaultFun])

/** Lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  * @param forceBuiltins
  *   generate variables for forced builtins
  */
class OptimizingSirToUplcLowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    forceBuiltins: ForceBuiltins = ForceBuiltins.AllUsed
):
    // import PolyBuiltin.given
    private var zCombinatorNeeded: Boolean = false
    private val decls = HashMap.empty[String, DataDecl]

    // Builtins used in the SIR
    // use ordered set to ensure deterministic order, mainly for tests
    private val usedBuiltins = TreeSet.empty[DefaultFun]

    private def isPoly(bi: DefaultFun): Boolean =
        Meaning.allBuiltins.BuiltinMeanings(bi).typeScheme.numTypeVars > 0

    analizeSir(sir)

    private lazy val builtinTerms: Map[DefaultFun, Term] =

        val usedPolyBuiltins = usedBuiltins.filter(isPoly)

        val toForce = forceBuiltins match
            case ForceBuiltins.None           => TreeSet.empty
            case ForceBuiltins.AllUsed        => usedPolyBuiltins
            case ForceBuiltins.Only(builtins) => usedPolyBuiltins.intersect(builtins)

        usedBuiltins.view
            .map: bi =>
                if toForce.contains(bi)
                then bi -> Term.Var(NamedDeBruijn(s"__builtin_${bi}"))
                else
                    bi ->
                        forceBuiltin(
                          Meaning.allBuiltins.BuiltinMeanings(bi).typeScheme,
                          Term.Builtin(bi)
                        )
            .toMap

    private def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
        case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
        case _                    => term

    private def analizeSir(sir: SIR): Unit = {
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                analizeSir(body)
            case SIR.Constr(name, data, args) =>
                args.foreach(analizeSir)
            case SIR.Match(scrutinee, cases, tp) =>
                analizeSir(scrutinee)
                cases.foreach { case SIR.Case(_, _, _, body) =>
                    analizeSir(body)
                }
            case SIR.Let(_, bindings, body) =>
                bindings.foreach { case Binding(_, rhs) =>
                    analizeSir(rhs)
                }
                analizeSir(body)
            case SIR.LamAbs(_, term) =>
                analizeSir(term)
            case SIR.Apply(f, arg, tp) =>
                analizeSir(f)
                analizeSir(arg)
            case SIR.IfThenElse(cond, t, f, tp) =>
                usedBuiltins += DefaultFun.IfThenElse
                analizeSir(cond)
                analizeSir(t)
                analizeSir(f)
            case SIR.And(lhs, rhs) =>
                usedBuiltins += DefaultFun.IfThenElse
                analizeSir(lhs)
                analizeSir(rhs)
            case SIR.Or(lhs, rhs) =>
                usedBuiltins += DefaultFun.IfThenElse
                analizeSir(lhs)
                analizeSir(rhs)
            case SIR.Not(term) =>
                usedBuiltins += DefaultFun.IfThenElse
                analizeSir(term)
            case SIR.Builtin(bi, _)       => usedBuiltins += bi
            case SIR.Error(_, _)             =>
            case SIR.Var(_, _)            =>
            case SIR.ExternalVar(_, _, _) =>
            case SIR.Const(_, _)          =>
    }

    def lower(): Term =
        val term =
            forceBuiltins match
                case ForceBuiltins.None    => sir |> lowerInner
                case ForceBuiltins.AllUsed => sir |> lowerInner |> addForcedBuiltins(usedBuiltins)
                case ForceBuiltins.Only(builtins) =>
                    sir |> lowerInner |> addForcedBuiltins(builtins)

        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term

    private def addForcedBuiltins(toForce: collection.Set[DefaultFun])(term: Term): Term =
        usedBuiltins.filter(isPoly).foldLeft(term) { (acc, bi) =>
            val scheme = Meaning.allBuiltins.BuiltinMeanings(bi).typeScheme
            if toForce.contains(bi) then
                val forced = forceBuiltin(scheme, Term.Builtin(bi))
                Term.Apply(Term.LamAbs(s"__builtin_${bi}", acc), forced)
            else acc
        }

    private def lowerInner(sir: SIR): Term =
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args) =>
                /* data List a = Nil | Cons a (List a)
                    Nil is represented as \Nil Cons -> force Nil
                    Cons is represented as (\head tail Nil Cons -> Cons head tail) h tl
                 */
                val constrs = data.constructors.map(_.name)
                val ctorParams = data.constructors.find(_.name == name) match
                    case None =>
                        throw new IllegalArgumentException(s"Constructor $name not found in $data")
                    case Some(value) => value.params

                // force Nil | Cons head tail
                val appInner = ctorParams match
                    case Nil => Term.Force(Term.Var(NamedDeBruijn(name)))
                    case _ =>
                        ctorParams.foldLeft(Term.Var(NamedDeBruijn(name)))((acc, param) =>
                            acc $ Term.Var(NamedDeBruijn(param.name))
                        )
                // \Nil Cons -> ...
                val ctor = constrs.foldRight(appInner) { (constr, acc) =>
                    Term.LamAbs(constr, acc)
                }
                // \head tail Nil Cons -> ...
                val ctorParamsLambda = ctorParams.foldRight(ctor) { (param, acc) =>
                    Term.LamAbs(param.name, acc)
                }
                // (\Nil Cons -> force Nil) | (\head tail Nil Cons -> ...) h tl
                args.foldLeft(ctorParamsLambda) { (acc, arg) =>
                    Term.Apply(acc, lowerInner(arg))
                }
            case SIR.Match(scrutinee, cases, tp) =>
                /* list match
                    case Nil -> 1
                    case Cons(h, tl) -> 2

                    lowers to list (delay 1) (\h tl -> 2)
                 */
                val scrutineeTerm = lowerInner(scrutinee)
                val casesTerms = cases.map { case SIR.Case(constr, bindings, typeBindings, body) =>
                    constr.params match
                        case Nil => ~lowerInner(body)
                        case _ =>
                            bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                Term.LamAbs(binding, acc)
                            }
                }
                casesTerms.foldLeft(scrutineeTerm) { (acc, caseTerm) => Term.Apply(acc, caseTerm) }
            case SIR.Var(name, _)            => Term.Var(NamedDeBruijn(name))
            case SIR.ExternalVar(_, name, _) => Term.Var(NamedDeBruijn(name))
            case SIR.Let(NonRec, bindings, body) =>
                bindings.foldRight(lowerInner(body)) { case (Binding(name, rhs), body) =>
                    Term.Apply(Term.LamAbs(name, body), lowerInner(rhs))
                }
            case SIR.Let(Rec, Binding(name, rhs) :: Nil, body) =>
                /*  let rec f x = f (x + 1)
                    in f 0
                    (\f -> f 0) (Z (\f. \x. f (x + 1)))
                 */
                zCombinatorNeeded = true
                val fixed =
                    Term.Apply(
                      Term.Var(NamedDeBruijn("__z_combinator__")),
                      Term.LamAbs(name, lowerInner(rhs))
                    )
                Term.Apply(Term.LamAbs(name, lowerInner(body)), fixed)
            case SIR.Let(Rec, bindings, body) =>
                // TODO: implement mutual recursion
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
            case SIR.LamAbs(sirVar, term) => Term.LamAbs(sirVar.name, lowerInner(term))
            case SIR.Apply(f, arg, tp)    => Term.Apply(lowerInner(f), lowerInner(arg))
            case SIR.Const(const, tp)       => Term.Const(const)
            case SIR.And(lhs, rhs) =>
                lowerInner(SIR.IfThenElse(lhs, rhs, SIR.Const(Constant.Bool(false),SIRType.BooleanPrimitive), SIRType.BooleanPrimitive))
            case SIR.Or(lhs, rhs) =>
                lowerInner(SIR.IfThenElse(lhs, SIR.Const(Constant.Bool(true), SIRType.BooleanPrimitive), rhs, SIRType.BooleanPrimitive))
            case SIR.Not(term) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.BooleanPrimitive),
                    SIR.Const(Constant.Bool(true), SIRType.BooleanPrimitive),
                    SIRType.BooleanPrimitive
                  )
                )
            case SIR.IfThenElse(cond, t, f, tp) =>
                !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Builtin(bn, _) => builtinTerms(bn)
            case SIR.Error(msg, _) =>
                if generateErrorTraces
                then
                    usedBuiltins += DefaultFun.Trace
                    !(builtinTerms(DefaultFun.Trace) $ Term.Const(
                      Constant.String(msg)
                    ) $ ~Term.Error)
                else Term.Error
