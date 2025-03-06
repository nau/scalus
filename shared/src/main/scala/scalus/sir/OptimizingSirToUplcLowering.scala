package scalus
package sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.Pattern
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.ExprBuilder
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.TermDSL.*
import scalus.uplc.TypeScheme
import scalus.uplc.given

import scala.collection.mutable
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

    analyzeSir(sir)

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

    private def analyzeSir(sir: SIR): Unit = {
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                analyzeSir(body)
            case SIR.Constr(name, data, args, tp) =>
                args.foreach(analyzeSir)
            case SIR.Match(scrutinee, cases, tp) =>
                analyzeSir(scrutinee)
                cases.foreach { case SIR.Case(_, body) =>
                    analyzeSir(body)
                }
            case SIR.Let(_, bindings, body) =>
                bindings.foreach { case Binding(_, rhs) =>
                    analyzeSir(rhs)
                }
                analyzeSir(body)
            case SIR.LamAbs(_, term) =>
                analyzeSir(term)
            case SIR.Apply(f, arg, tp) =>
                analyzeSir(f)
                analyzeSir(arg)
            case SIR.Select(scrutinee, _, _) =>
                analyzeSir(scrutinee)
            case SIR.IfThenElse(cond, t, f, tp) =>
                usedBuiltins += DefaultFun.IfThenElse
                analyzeSir(cond)
                analyzeSir(t)
                analyzeSir(f)
            case SIR.And(lhs, rhs) =>
                usedBuiltins += DefaultFun.IfThenElse
                analyzeSir(lhs)
                analyzeSir(rhs)
            case SIR.Or(lhs, rhs) =>
                usedBuiltins += DefaultFun.IfThenElse
                analyzeSir(lhs)
                analyzeSir(rhs)
            case SIR.Not(term) =>
                usedBuiltins += DefaultFun.IfThenElse
                analyzeSir(term)
            case SIR.Builtin(bi, _) => usedBuiltins += bi
            case SIR.Error(_, _) =>
                if generateErrorTraces then usedBuiltins += DefaultFun.Trace
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
            case SIR.Constr(name, data, args, tp) =>
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

                def find(sirType: SIRType): Seq[ConstrDecl] =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _) => Seq(constrDecl)
                        case SIRType.SumCaseClass(decl, _) =>
                            decl.constructors
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show}"
                            )

                val constructors = find(scrutinee.tp)

                // 1. If we have a wildcard case, it must be the last one
                // 2. Validate we don't have any errors
                // 3. Convert Wildcard to the rest of the cases/constructors
                // 4. Sort the cases by constructor name

                var idx = 0
                val iter = cases.iterator
                val allConstructors = constructors.toSet
                val matchedConstructors = mutable.HashSet.empty[String]
                val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]

                while iter.hasNext do
                    iter.next() match
                        case c @ SIR.Case(Pattern.Constr(constrDecl, _, _), _) =>
                            matchedConstructors += constrDecl.name // collect all matched constructors
                            expandedCases += c
                        case SIR.Case(Pattern.Wildcard, rhs) =>
                            // If we have a wildcard case, it must be the last one
                            if idx != cases.length - 1 then
                                throw new IllegalArgumentException(
                                  s"Wildcard case must be the last and only one in match expression"
                                )
                            else
                                // Convert Wildcard to the rest of the cases/constructors
                                val missingConstructors = allConstructors.filter(c =>
                                    !matchedConstructors.contains(c.name)
                                )
                                missingConstructors.foreach { constrDecl =>
                                    val bindings = constrDecl.params.map(_.name)
                                    // TODO: extract rhs to a let binding before the match
                                    // so we don't have to repeat it for each case
                                    // also we have no way to know type-arguments, so use abstract type-vars (will use FreeUnificator)
                                    val typeArgs =
                                        constrDecl.typeParams.map(_ => SIRType.FreeUnificator)
                                    expandedCases += SIR.Case(
                                      Pattern.Constr(constrDecl, bindings, typeArgs),
                                      rhs
                                    )
                                    matchedConstructors += constrDecl.name // collect all matched constructors
                                }

                    idx += 1
                end while
                // Sort the cases by constructor name to ensure we have a deterministic order
                val sortedCases = expandedCases.sortBy {
                    case SIR.Case(Pattern.Constr(constr, _, _), _) =>
                        constr.name
                    case SIR.Case(Pattern.Wildcard, _) =>
                        throw new IllegalArgumentException(
                          "Wildcard case must have been eliminated"
                        )
                }.toList

                val casesTerms = sortedCases.map {
                    case SIR.Case(Pattern.Constr(constr, bindings, _), body) =>
                        constr.params match
                            case Nil => ~lowerInner(body)
                            case _ =>
                                bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                    Term.LamAbs(binding, acc)
                                }
                    case SIR.Case(Pattern.Wildcard, _) =>
                        throw new IllegalArgumentException(
                          "Wildcard case must have been eliminated"
                        )
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
            case SIR.Select(scrutinee, field, _) =>
                def find(sirType: SIRType): ConstrDecl =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _) => constrDecl
                        case SIRType.SumCaseClass(decl, _) =>
                            if decl.constructors.length == 1 then decl.constructors.head
                            else
                                throw new IllegalArgumentException(
                                  s"Expected case class type, got ${sirType} in expression: ${sir.show}"
                                )
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show}"
                            )
                def lowerSelect(constrDecl: ConstrDecl) = {
                    val fieldIndex = constrDecl.params.indexWhere(_.name == field)
                    if fieldIndex == -1 then
                        throw new IllegalArgumentException(
                          s"Field $field not found in constructor ${constrDecl.name}"
                        )
                    val instance = lowerInner(scrutinee)
                    val s0 = Term.Var(NamedDeBruijn(field))
                    val lam = constrDecl.params.foldRight(s0) { case (f, acc) =>
                        Term.LamAbs(f.name, acc)
                    }
                    Term.Apply(instance, lam)
                }
                lowerSelect(find(scrutinee.tp))
            case SIR.Const(const, tp) => Term.Const(const)
            case SIR.And(lhs, rhs) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean),
                    SIRType.Boolean
                  )
                )
            case SIR.Or(lhs, rhs) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.Boolean),
                    rhs,
                    SIRType.Boolean
                  )
                )
            case SIR.Not(term) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean),
                    SIR.Const(Constant.Bool(true), SIRType.Boolean),
                    SIRType.Boolean
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
