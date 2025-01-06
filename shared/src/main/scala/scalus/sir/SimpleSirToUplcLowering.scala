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

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

/** Lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  */
class SimpleSirToUplcLowering(sir: SIR, generateErrorTraces: Boolean = false):

    private val builtinTerms =
        def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
            case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
            case _                    => term

        Meaning.allBuiltins.BuiltinMeanings.map((bi, rt) =>
            bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi))
        )

    private var zCombinatorNeeded: Boolean = false
    private val decls = HashMap.empty[String, DataDecl]

    def lower(): Term =
        val term = lowerInner(sir)
        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term

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
            case SIR.LamAbs(name, term) => Term.LamAbs(name.name, lowerInner(term))
            case SIR.Apply(f, arg, _)   => Term.Apply(lowerInner(f), lowerInner(arg))
            case SIR.Select(scrutinee, field, _) =>
                @tailrec
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
            case SIR.Const(const, _) => Term.Const(const)
            case SIR.And(lhs, rhs) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.BooleanPrimitive),
                    SIRType.BooleanPrimitive
                  )
                )
            case SIR.Or(lhs, rhs) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.BooleanPrimitive),
                    rhs,
                    SIRType.BooleanPrimitive
                  )
                )
            case SIR.Not(term) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.BooleanPrimitive),
                    SIR.Const(Constant.Bool(true), SIRType.BooleanPrimitive),
                    SIRType.BooleanPrimitive
                  )
                )
            case SIR.IfThenElse(cond, t, f, _) =>
                !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Builtin(bn, _) => builtinTerms(bn)
            case SIR.Error(msg, _) =>
                if generateErrorTraces
                then
                    !(builtinTerms(DefaultFun.Trace) $ Term.Const(
                      Constant.String(msg)
                    ) $ ~Term.Error)
                else Term.Error
