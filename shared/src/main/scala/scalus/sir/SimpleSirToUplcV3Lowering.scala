package scalus
package sir

import scalus.sir.Recursivity.*
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
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
class SimpleSirToUplcV3Lowering(sir: SIR, generateErrorTraces: Boolean = false):

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

    private def constr(tag: Long, args: Seq[Term]): Term =
        builtinTerms(ConstrData) $ Term.Const(Constant.Integer(tag)) $ args.foldRight(
          builtinTerms(MkNilData) $ Term.Const(Constant.Unit)
//          Term.Const(Constant.List(DefaultUni.List(DefaultUni.Data), Nil))
        ) { (arg, ls) =>
            builtinTerms(MkCons) $ arg $ ls
        }

    @tailrec
    private def toData(arg: Term, tp: SIRType): Term =
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                builtinTerms(IfThenElse) $ arg $ constr(1, Nil) $ constr(0, Nil)
            case SIRType.Integer    => builtinTerms(IData) $ arg
            case SIRType.ByteString => builtinTerms(BData) $ arg
            case SIRType.String =>
                builtinTerms(BData) $ (builtinTerms(EncodeUtf8) $ arg)
            case SIRType.Unit => constr(0, Nil)
            case SIRType.BLS12_381_G1_Element =>
                toData(builtinTerms(Bls12_381_G1_compress) $ arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                toData(builtinTerms(Bls12_381_G2_compress) $ arg, SIRType.ByteString)
            case _: SIRType.CaseClass => arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )

    private def unconstr(term: Term): Term = builtinTerms(UnConstrData) $ term

    private def fromData(arg: Term, tp: SIRType): Term =
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                val tag = builtinTerms(FstPair) $ unconstr(arg)
                builtinTerms(EqualsInteger) $ tag $ Term.Const(Constant.Integer(1))
            case SIRType.Integer =>
                builtinTerms(UnIData) $ arg
            case SIRType.ByteString =>
                builtinTerms(UnBData) $ arg
            case SIRType.String =>
                builtinTerms(DecodeUtf8) $ (builtinTerms(UnBData) $ arg)
            case SIRType.Unit => Term.Const(Constant.Unit)
            case SIRType.BLS12_381_G1_Element =>
                builtinTerms(Bls12_381_G1_uncompress) $ fromData(arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                builtinTerms(Bls12_381_G2_uncompress) $ fromData(arg, SIRType.ByteString)
            case _: SIRType.CaseClass => arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )

    private def getFieldByIndex(args: Term, fieldIndex: Long, tp: SIRType) = {
        var expr = args
        var i = 0
        while i < fieldIndex do
            expr = builtinTerms(TailList) $ expr
            i += 1
        val data = builtinTerms(HeadList) $ expr
        fromData(data, tp)
    }

    private def lowerInner(sir: SIR): Term =
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args) =>
                val tag = data.constructors.indexWhere(_.name == name, 0)
                if tag == -1 then
                    throw new IllegalArgumentException(s"Constructor $name not found in $data")
                val ctorParams = data.constructors(tag).params
                val loweredArgs = args
                    .zip(ctorParams)
                    .map:
                        case (arg, TypeBinding(_, tp)) => toData(lowerInner(arg), tp)
                constr(tag, loweredArgs)
            case SIR.Match(scrutinee, cases, tp) =>
                val scrutineeTerm = lowerInner(scrutinee)
                val pair = unconstr(scrutineeTerm)
                λ("__pair") {
                    λ("__tag") {
                        λ("__args") {
                            var term = lowerInner(SIR.Error("MatchError", null))
                            for (cs, idx) <- cases.zipWithIndex do
                                val bodyTerm = lowerInner(cs.body)
                                val bindings = cs.bindings.zipWithIndex
                                    .zip(cs.constr.params)
                                    .foldRight(bodyTerm):
                                        case (((name, idx), TypeBinding(_, tp)), term) =>
                                            val value = getFieldByIndex(vr"__args", idx, tp)
                                            lam(name)(term) $ value

                                term =
                                    val cond = builtinTerms(EqualsInteger) $ vr"__tag" $ idx.asTerm
                                    !(builtinTerms(IfThenElse) $ cond $ ~bindings $ ~term)

                            println(term.showHighlighted)
                            term
                        } $ (builtinTerms(SndPair) $ vr"__pair")
                    } $ (builtinTerms(FstPair) $ vr"__pair")
                } $ pair
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
            // f(arg)
            case SIR.Apply(f, arg, _) => Term.Apply(lowerInner(f), lowerInner(arg))
            // record.field
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

                    val args = builtinTerms(SndPair) $ unconstr(instance)
                    getFieldByIndex(args, fieldIndex, constrDecl.params(fieldIndex).tp)
                }
                lowerSelect(find(scrutinee.tp))
            case SIR.Const(const, _) => Term.Const(const)
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
            case SIR.IfThenElse(cond, t, f, _) =>
                !(builtinTerms(IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Builtin(bn, _) => builtinTerms(bn)
            case SIR.Error(msg, _) =>
                if generateErrorTraces
                then
                    !(builtinTerms(Trace) $ Term.Const(
                      Constant.String(msg)
                    ) $ ~Term.Error)
                else Term.Error
