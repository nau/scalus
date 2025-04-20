package scalus
package sir

import scalus.sir.Recursivity.*
import scalus.sir.SIR.Pattern
import scalus.uplc.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.HashMap

private case class Lowering(
    toData: Long => Seq[Term] => Term,
    select: Long => Term => Term,
    genMatch: (
        matchExpr: SIR,
        constructors: Seq[ConstrDecl],
        cases: Seq[SIR.Case],
    ) => Term
)

/** Lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  */
class SimpleSirToUplcV3Lowering(sir: SIR, generateErrorTraces: Boolean = false):

    private val builtinTerms = {
        def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
            case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
            case _                    => term

        Meaning.allBuiltins.BuiltinMeanings.map((bi, rt) =>
            bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi))
        )
    }

    extension (fun: DefaultFun) private def $(arg: Term): Term = builtinTerms(fun) $ arg

    private var zCombinatorNeeded: Boolean = false
    private val decls = HashMap.empty[String, DataDecl]

    def lower(): Term = {
        val term = lowerInner(sir)
        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term
    }

    private def constrData(tag: Long, args: Seq[Term]): Term = {
        ConstrData $ Term.Const(Constant.Integer(tag)) $ args.foldRight(
          MkNilData $ Term.Const(Constant.Unit)
//          Term.Const(Constant.List(DefaultUni.List(DefaultUni.Data), Nil))
        ) { (arg, ls) =>
            MkCons $ arg $ ls
        }
    }

    @tailrec
    private def toData(arg: Term, tp: SIRType): Term = {
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                IfThenElse $ arg $ constrData(1, Nil) $ constrData(0, Nil)
            case SIRType.Integer    => IData $ arg
            case SIRType.ByteString => BData $ arg
            case SIRType.String =>
                BData $ (EncodeUtf8 $ arg)
            case SIRType.Unit => constrData(0, Nil)
            case SIRType.BLS12_381_G1_Element =>
                toData(Bls12_381_G1_compress $ arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                toData(Bls12_381_G2_compress $ arg, SIRType.ByteString)
            case _: SIRType.CaseClass =>
                println(s"toData: ${tp.show}")
                arg
            case _: SIRType.SumCaseClass =>
                println(s"toData: ${tp.show}")
                arg
            case _: SIRType.TypeVar =>
                println(s"toData: ${tp.show}")
                arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp} of term: ${arg.show}"
                )
    }

    private def unconstr(term: Term): Term = UnConstrData $ term

    private def fromData(arg: Term, tp: SIRType): Term = {
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                val tag = FstPair $ unconstr(arg)
                EqualsInteger $ tag $ Term.Const(Constant.Integer(1))
            case SIRType.Integer =>
                UnIData $ arg
            case SIRType.ByteString =>
                UnBData $ arg
            case SIRType.String =>
                DecodeUtf8 $ (UnBData $ arg)
            case SIRType.Unit => Term.Const(Constant.Unit)
            case SIRType.BLS12_381_G1_Element =>
                Bls12_381_G1_uncompress $ fromData(arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                Bls12_381_G2_uncompress $ fromData(arg, SIRType.ByteString)
            case _: SIRType.CaseClass    => arg
            case _: SIRType.SumCaseClass => arg
            case _: SIRType.TypeVar      => arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )
    }

    private def getFieldByIndex(args: Term, fieldIndex: Long, tp: SIRType) = {
        var expr = args
        var i = 0
        while i < fieldIndex do
            expr = TailList $ expr
            i += 1
        val data = HeadList $ expr
        fromData(data, tp)
    }

    /** Generates [[Term]] like this:
      * {{{
      *  if tag == 0
      *  then { (field1, field2, ...) => caseBody} (fromData(args.head), fromData(args.tail.head), ...)
      *  else if tag == 1 then ...
      *  else Error("MatchError")
      * }}}
      */

    private def genMatch(
        constructors: Seq[ConstrDecl],
        cases: Seq[SIR.Case],
        args: Term,
        tag: Term
    ): Term = {
        val mapping = constructors.zipWithIndex.map { case (c, i) => (c.name, i) }.toMap
        val matchErrorTerm = lowerInner(
          SIR.Error(s"MatchError: unknown constructor tag", null)
        )
        cases.foldRight(matchErrorTerm) {
            case (SIR.Case(Pattern.Constr(constr, bindings, _), body), resultTerm) =>
                val idx = mapping(constr.name)
                val bodyTerm = lowerInner(body)
                val bodyWithBindings = bindings.zipWithIndex
                    .zip(constr.params)
                    .foldRight(bodyTerm):
                        case (((name, idx), TypeBinding(_, tp)), term) =>
                            val value = getFieldByIndex(args, idx, tp)
                            lam(name)(term) $ value
                val cond = EqualsInteger $ idx.asTerm $ tag
                !(IfThenElse $ cond $ ~bodyWithBindings $ ~resultTerm)
            case _ => matchErrorTerm
        }
    }

    /** Lowering for a newtype-like types: single argument case class, like
      * [[scalus.ledger.api.v3.TxId]] or [[scalus.ledger.api.v1.PubKeyHash]]
      */
    private val newtypeLowering: Lowering = Lowering(
      toData = { tag => args => args.head },
      select = {
          case 0 => identity
          case i => throw new IllegalArgumentException(s"Invalid field index $i for TxId")
      },
      genMatch = (
          matchExpr: SIR,
          constructors: Seq[ConstrDecl],
          cases: Seq[SIR.Case],
      ) => {
          val scrutineeTerm = lowerInner(matchExpr)
          cases match
              case SIR.Case(Pattern.Constr(constr, bindings, _), body) :: Nil =>
                  λ(bindings.head)(lowerInner(body)) $ scrutineeTerm
              case _ =>
                  throw new IllegalArgumentException(
                    s"Expected single case"
                  )
      }
    )

    // fresh name idx
    private var idx = 0

    private def freshName(suffix: String): String = {
        val name = s"__scalus__${suffix}_$idx"
        idx += 1
        name
    }

    // let __scalus_$suffix = value in f(Var("__scalus_$suffix"))
    private def let(suffix: String, value: Term)(f: Term => Term): Term = {
        val name = freshName(suffix)
        λλ(name)(f) $ value
    }

    private val listLowering: Lowering = Lowering(
      toData = {
          case 0 => args => MkNilData $ Term.Const(Constant.Unit)
          case 1 => args => MkCons $ args.head $ args(1)
      },
      select = _ => ???,
      genMatch = (
          matchExpr: SIR,
          constructors: Seq[ConstrDecl],
          cases: Seq[SIR.Case],
      ) => {
          val listTerm = UnListData $ lowerInner(matchExpr)
          cases match
              case List(
                    SIR.Case(Pattern.Constr(_, _, _), nilBody),
                    SIR.Case(Pattern.Constr(_, List(head, tail), _), consBody)
                  ) =>
                  val nilTerm = lowerInner(nilBody)
                  val consTerm = let("list", listTerm) { list =>
                      λ(head, tail)(lowerInner(consBody)) $ (HeadList $ list) $ (TailList $ list)
                  }
                  !(ChooseList $ listTerm $ ~nilTerm $ ~consTerm)
              case _ => ???
      }
    )

    private val mapping: Map[String, Lowering] = Map(
      "scalus.prelude.List" -> listLowering,
      "scalus.ledger.api.v1.PubKeyHash" -> newtypeLowering,
      "scalus.ledger.api.v3.TxId" -> newtypeLowering,
    )

    private def lowerInner(sir: SIR): Term = {
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args, tp, anns) =>
                val tag = data.constructors.indexWhere(_.name == name, 0)
                if tag == -1 then
                    throw new IllegalArgumentException(s"Constructor $name not found in $data")
                val loweredArgs = args.map: arg =>
                    println(s"lowering ${arg.show}: ${tp}")
                    toData(lowerInner(arg), arg.tp)
                if mapping.contains(data.name)
                then
                    val term = mapping(data.name).toData(tag)(loweredArgs)
                    term
                else constrData(tag, loweredArgs)
            case m @ SIR.Match(scrutinee, cases, tp, anns) =>
                val scrutineeTerm = lowerInner(scrutinee)

                def find(sirType: SIRType): (String, Seq[ConstrDecl]) =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _, _) =>
                            (constrDecl.name, Seq(constrDecl))
                        case SIRType.SumCaseClass(decl, _) =>
                            (decl.name, decl.constructors.toSeq)
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show}"
                            )

                val (name, constructors) = find(scrutinee.tp)

                // 1. If we have a wildcard case, it must be the last one
                // 2. Validate we don't have any errors
                // 3. Convert Wildcard to the rest of the cases/constructors
                // 4. Sort the cases by constructor name

                var idx = 0

                val allConstructors = constructors.toSet
                val matchedConstructors = mutable.HashSet.empty[String]
                val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]
                val isUnchecked = anns.data.contains("unchecked")
                val enhanchedCases =
                    if isUnchecked && cases.length < allConstructors.size then
                        cases :+ SIR.Case(
                          Pattern.Wildcard,
                          SIR.Error("Unexpected case", anns)
                        )
                    else cases

                val casesIter = enhanchedCases.iterator

                while casesIter.hasNext do
                    casesIter.next() match
                        case c @ SIR.Case(Pattern.Constr(constrDecl, _, _), _) =>
                            matchedConstructors += constrDecl.name // collect all matched constructors
                            expandedCases += c
                        case SIR.Case(Pattern.Wildcard, rhs) =>
                            // If we have a wildcard case, it must be the last one
                            if idx != enhanchedCases.length - 1 then
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

                mapping.get(name) match
                    case Some(lowering) =>
                        lowering.genMatch(scrutinee, constructors, orderedCases)
                    case None =>
                        val pair = unconstr(scrutineeTerm)
                        λλ("pair") { pair =>
                            λλ("tag") { tag =>
                                λλ("args") { args =>
                                    genMatch(constructors, cases, args, tag)
                                } $ (SndPair $ pair)
                            } $ (FstPair $ pair)
                        } $ pair
            case SIR.Var(name, _, _)            => Term.Var(NamedDeBruijn(name))
            case SIR.ExternalVar(_, name, _, _) => Term.Var(NamedDeBruijn(name))
            case SIR.Let(NonRec, bindings, body, _) =>
                bindings.foldRight(lowerInner(body)) { case (Binding(name, rhs), body) =>
                    Term.Apply(Term.LamAbs(name, body), lowerInner(rhs))
                }
            case SIR.Let(Rec, Binding(name, rhs) :: Nil, body, _) =>
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
            case SIR.Let(Rec, bindings, body, anns) =>
                // TODO: implement mutual recursion
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
            case SIR.LamAbs(name, term, _anns) => Term.LamAbs(name.name, lowerInner(term))
            case SIR.Apply(SIR.Var("__scalus__internal__fromData", _, _), a, sirType, ann) =>
                fromData(lowerInner(a), sirType)
            case SIR.Apply(
                  SIR.Var("__scalus__internal__toData", SIRType.Fun(sirType, _), _),
                  a,
                  _,
                  _ann
                ) =>
                toData(lowerInner(a), sirType)
            // f(arg)
            case SIR.Apply(f, arg, _, _) => Term.Apply(lowerInner(f), lowerInner(arg))
            // record.field
            case SIR.Select(scrutinee, field, _, _) =>
                @tailrec
                def find(sirType: SIRType): (String, ConstrDecl) =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _, _) => (constrDecl.name, constrDecl)
                        case SIRType.SumCaseClass(decl, _) =>
                            decl.constructors match
                                case head :: Nil => (decl.name, head)
                                case _ =>
                                    throw new IllegalArgumentException(
                                      s"Expected single constructor, got ${decl.constructors} in expression: ${sir.show}"
                                    )
                        case SIRType.TypeLambda(_, t) => find(t)
                        case _ =>
                            throw new IllegalArgumentException(
                              s"Expected case class type, got ${sirType} in expression: ${sir.show}"
                            )

                def lowerSelect(fullname: String, constrDecl: ConstrDecl) = {
                    val fieldIndex = constrDecl.params.indexWhere(_.name == field)
                    if fieldIndex == -1 then
                        throw new IllegalArgumentException(
                          s"Field $field not found in constructor ${constrDecl.name}"
                        )
                    val instance = lowerInner(scrutinee)

                    mapping.get(fullname) match
                        case Some(lowering) =>
                            lowering.select(fieldIndex)(instance)
                        case None =>
                            val args = SndPair $ unconstr(instance)
                            getFieldByIndex(args, fieldIndex, constrDecl.params(fieldIndex).tp)
                }
                val (name, constrDecl) = find(scrutinee.tp)
                lowerSelect(name, constrDecl)
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
                !(IfThenElse $ lowerInner(cond) $ ~lowerInner(t) $ ~lowerInner(f))
            case SIR.Builtin(bn, _, _) => builtinTerms(bn)
            case SIR.Error(msg, _, _) =>
                if generateErrorTraces
                then !(Trace $ Term.Const(Constant.String(msg)) $ ~Term.Error)
                else Term.Error
    }
