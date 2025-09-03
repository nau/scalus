package scalus
package sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.Pattern
import scalus.uplc.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * We use UPLC version 1.1.0 and generate Sums of Products (SoP) constructors `case` and `constr`
  * to represent data types. Also we optimize `newtype` kind of constructors to be represented as
  * just values.
  *
  * @example
  *   {{{
  *  case class Wrapper(a: BigInt)
  *  val x = Wrapper(1) // lowers to just (const integer 1)
  *   }}}
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  */
class SirToUplc110Lowering(sir: SIR, generateErrorTraces: Boolean = false):

    private def builtinTerms = Meaning.allBuiltins.forcedBuiltins

    private var zCombinatorNeeded: Boolean = false
    private val decls = mutable.HashMap.empty[String, DataDecl]

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
            case SIR.Constr(name, data, args, tp, anns) =>
                /*
                  data Newtype(a) is represented as a
                
                  data List a = Nil | Cons a (List a)
                    Nil is represented as (constr 0 [])
                    Cons is represented as (constr 1 [h, tl])
                 */

                if data.constructors.size == 1 && data.constructors.head.params.size == 1 then
                    assert(args.size == 1)
                    lowerInner(args.head)
                else
                    val tag = data.constructors.indexWhere(_.name == name)
                    if tag == -1 then
                        throw new IllegalArgumentException(s"Constructor $name not found in $data")
                    Term.Constr(tag, args.map(lowerInner))
            case SIR.Match(scrutinee, cases, tp, anns) =>
                /* list match
                    case Nil -> error
                    case Cons(h, tl) -> 2

                    lowers to (case list [error, \h tl -> 2])

                    newtype match
                        case Newtype(a) -> error

                    lowers to (\a -> error) newtype
                 */
                val scrutineeTerm = lowerInner(scrutinee)

                def find(sirType: SIRType): Seq[ConstrDecl] =
                    sirType match
                        case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                            optParent match
                                case None         => Seq(constrDecl)
                                case Some(parent) => find(parent)
                        case SIRType.SumCaseClass(decl, _) =>
                            decl.constructors
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show} at ${anns.pos}"
                            )

                val constructors = find(scrutinee.tp)

                // 1. If we have a wildcard case, it must be the last one
                // 2. Validate we don't have any errors
                // 3. Convert Wildcard to the rest of the cases/constructors
                // 4. Sort the cases by constructor name

                var idx = 0

                val allConstructors = constructors.toSet
                val matchedConstructors = mutable.HashSet.empty[String]
                val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]
                // when we have a deconstruction like this:
                // val Some(x) = expr
                // Scala compiler generates an @unchecked annotation
                // and code like this:
                // val x = expr match
                //   case Some(x) => x
                // }
                // which doesn't have a wildcard case
                // so we need to add a wildcard case in this case ;)
                val isUnchecked = anns.data.contains("unchecked")
                val enhancedCases =
                    if isUnchecked && cases.length < allConstructors.size then
                        cases :+ SIR.Case(
                          Pattern.Wildcard,
                          SIR.Error("Unexpected case", anns),
                          anns
                        )
                    else cases

                val casesIter = enhancedCases.iterator

                while casesIter.hasNext do
                    casesIter.next() match
                        case c @ SIR.Case(Pattern.Constr(constrDecl, _, _), _, _) =>
                            matchedConstructors += constrDecl.name // collect all matched constructors
                            expandedCases += c
                        case SIR.Case(Pattern.Wildcard, rhs, anns) =>
                            // If we have a wildcard case, it must be the last one
                            if idx != enhancedCases.length - 1 then
                                throw new IllegalArgumentException(
                                  s"Wildcard case must be the last and only one in match expression"
                                )
                            else
                                // Convert Wildcard to the rest of the cases/constructors
                                val missingConstructors = allConstructors.filter(c =>
                                    !matchedConstructors.contains(c.name)
                                )
                                missingConstructors.foreach { constrDecl =>
                                    val bindings = constrDecl.params.map(p =>
                                        s"__scalus_unused_binding_${p.name}"
                                    )
                                    // TODO: extract rhs to a let binding before the match
                                    // so we don't have to repeat it for each case
                                    // also we have no way to know type-arguments, so use abstract type-vars (will use FreeUnificator)
                                    val typeArgs =
                                        constrDecl.typeParams.map(_ => SIRType.FreeUnificator)
                                    expandedCases += SIR.Case(
                                      Pattern.Constr(constrDecl, bindings, typeArgs),
                                      rhs,
                                      anns
                                    )
                                    matchedConstructors += constrDecl.name // collect all matched constructors
                                }
                    idx += 1
                end while
                // Sort the cases by the same order as the constructors
                val orderedCases = constructors.map { constr =>
                    val optExpandedCase = expandedCases.find(_.pattern match {
                        case Pattern.Constr(constrDecl, _, _) => constrDecl.name == constr.name
                        case _                                => false
                    })
                    optExpandedCase.getOrElse(
                      throw new IllegalArgumentException(
                        s"Missing case for constructor ${constr.name} at ${anns.pos.file}: ${anns.pos.startLine}, ${anns.pos.startColumn}"
                      )
                    )
                }.toList

                val casesTerms = orderedCases.map {
                    case SIR.Case(Pattern.Constr(constr, bindings, _), body, anns) =>
                        constr.params match
                            case Nil => lowerInner(body)
                            case _ =>
                                bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                    Term.LamAbs(binding, acc)
                                }
                    case SIR.Case(Pattern.Wildcard, _, _) =>
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Wildcard case must have been eliminated at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                }
                if constructors.size == 1 && constructors.head.params.size == 1 then
                    assert(enhancedCases.size == 1)
                    assert(casesTerms.size == 1)
                    enhancedCases.head match
                        case SIR.Case(Pattern.Constr(constrDecl, bindings, _), body, _) =>
                            // newtype match
                            //   case Newtype(a) -> expr
                            // lowers to (\a -> expr) newtype
                            Term.Apply(casesTerms.head, scrutineeTerm)
                        case SIR.Case(Pattern.Wildcard, body, _) =>
                            // newtype match
                            //   case _ -> expr
                            // lowers to expr
                            lowerInner(body)
                else Term.Case(scrutineeTerm, casesTerms)
            case SIR.Var(name, _, _)            => Term.Var(NamedDeBruijn(name))
            case SIR.ExternalVar(_, name, _, _) => Term.Var(NamedDeBruijn(name))
            case SIR.Let(NonRec, bindings, body, anns) =>
                val loweredBody = lowerInner(body)
                val letResult = bindings.foldRight(loweredBody) {
                    case (Binding(name, tp, rhs), body) =>
                        Term.Apply(Term.LamAbs(name, body), lowerInner(rhs))
                }
                letResult
            case SIR.Let(Rec, Binding(name, tp, rhs) :: Nil, body, _) =>
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
            case SIR.Let(Rec, bindings, body, _) =>
                // TODO: implement mutual recursion
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
            case SIR.LamAbs(name, term, _, _)          => Term.LamAbs(name.name, lowerInner(term))
            case SIR.Apply(f, arg, _, _)               => Term.Apply(lowerInner(f), lowerInner(arg))
            case SIR.Select(scrutinee, field, _, anns) =>

                /*  x.field2
                    lowers to
                    (case x [\f1 f2 ... -> f2])

                    newtype.field1
                    lowers to
                    newtype
                 */

                @tailrec
                def find(sirType: SIRType): ConstrDecl =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _, _) => constrDecl
                        case SIRType.SumCaseClass(decl, _) =>
                            if decl.constructors.length == 1 then decl.constructors.head
                            else
                                val pos = anns.pos
                                throw new IllegalArgumentException(
                                  s"Expected case class type, got ${sirType} in expression: ${sir.show} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                                )
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            val pos = anns.pos
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                            )

                val constrDecl = find(scrutinee.tp)
                val fieldIndex = constrDecl.params.indexWhere(_.name == field)
                if fieldIndex == -1 then
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Field $field not found in constructor ${constrDecl} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                val instance = lowerInner(scrutinee)

                if constrDecl.params.size == 1 then instance
                else
                    val s0 = Term.Var(NamedDeBruijn(field))
                    val lam = constrDecl.params.foldRight(s0) { case (f, acc) =>
                        Term.LamAbs(f.name, acc)
                    }
                    Term.Case(instance, List(lam))
            case SIR.Const(const, _, _) => Term.Const(const)
            case SIR.And(lhs, rhs, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Or(lhs, rhs, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty),
                    rhs,
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Not(term, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty),
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.IfThenElse(cond, t, f, _, _) =>
                !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Builtin(bn, _, _) => builtinTerms(bn)
            case SIR.Error(msg, _, _) =>
                if generateErrorTraces
                then !(builtinTerms(DefaultFun.Trace) $ lowerInner(msg) $ ~Term.Error)
                else Term.Error
            case SIR.Cast(term, tp, anns) => lowerInner(term)
