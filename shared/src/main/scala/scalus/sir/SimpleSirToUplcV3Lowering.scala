package scalus
package sir

import scalus.Compiler.compile
import scalus.builtin.Builtins
import scalus.builtin.Data
import scalus.macros.Macros
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
        builtinTerms(DefaultFun.ConstrData) $ Term.Const(Constant.Integer(tag)) $ args.foldRight(
          builtinTerms(DefaultFun.MkNilData) $ Term.Const(Constant.Unit)
//          Term.Const(Constant.List(DefaultUni.List(DefaultUni.Data), Nil))
        ) { (arg, ls) =>
            builtinTerms(DefaultFun.MkCons) $ arg $ ls
        }

    @tailrec
    private def toData(arg: Term, tp: SIRType): Term =
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                builtinTerms(DefaultFun.IfThenElse) $ arg $ constr(1, Nil) $ constr(0, Nil)
            case SIRType.Integer    => builtinTerms(DefaultFun.IData) $ arg
            case SIRType.ByteString => builtinTerms(DefaultFun.BData) $ arg
            case SIRType.String =>
                builtinTerms(DefaultFun.BData) $ (builtinTerms(DefaultFun.EncodeUtf8) $ arg)
            case SIRType.Unit => constr(0, Nil)
            case SIRType.BLS12_381_G1_Element =>
                toData(builtinTerms(DefaultFun.Bls12_381_G1_compress) $ arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                toData(builtinTerms(DefaultFun.Bls12_381_G2_compress) $ arg, SIRType.ByteString)
            case _: SIRType.CaseClass => arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )

    private def fromData(arg: Term, tp: SIRType): Term =
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                val tag =
                    builtinTerms(DefaultFun.FstPair) $ (builtinTerms(DefaultFun.UnConstrData) $ arg)
                builtinTerms(DefaultFun.EqualsInteger) $ tag $ Term.Const(Constant.Integer(1))
            case SIRType.Integer =>
                builtinTerms(DefaultFun.UnIData) $ arg
            case SIRType.ByteString =>
                builtinTerms(DefaultFun.UnBData) $ arg
            case SIRType.String =>
                builtinTerms(DefaultFun.DecodeUtf8) $ (builtinTerms(DefaultFun.UnBData) $ arg)
            case SIRType.Unit => Term.Const(Constant.Unit)
            case SIRType.BLS12_381_G1_Element =>
                builtinTerms(DefaultFun.Bls12_381_G1_uncompress) $ fromData(arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                builtinTerms(DefaultFun.Bls12_381_G2_uncompress) $ fromData(arg, SIRType.ByteString)
            case _: SIRType.CaseClass => arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )

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
                println(data)
                constr(tag, loweredArgs)
            case SIR.Match(scrutinee, cases, tp) =>
                ???
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

                    val args = builtinTerms(DefaultFun.SndPair) $ (builtinTerms(
                      DefaultFun.UnConstrData
                    ) $ instance)
                    var expr = args
                    var i = 0
                    while i < fieldIndex do
                        expr = builtinTerms(DefaultFun.TailList) $ expr
                        i += 1
                    val data = builtinTerms(DefaultFun.HeadList) $ expr
                    fromData(data, constrDecl.params(fieldIndex).tp)
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
