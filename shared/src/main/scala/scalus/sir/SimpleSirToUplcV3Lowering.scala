package scalus
package sir

import scalus.Compiler.compile
import scalus.builtin.given
import scalus.builtin.Data.ToData
import scalus.ledger.api.v3.TxId
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
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.uplc.Inliner
import scalus.uplc.eval.PlutusVM

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

case class Asdf(
    toData: Seq[Term] => Term,
    select: Int => Term => Term,
    genMatch: SIR.Match => Term
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

    private var zCombinatorNeeded: Boolean = false
    private val decls = HashMap.empty[String, DataDecl]

    def lower(): Term = {
        val term = lowerInner(sir)
        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term
    }

    private def constrData(tag: Long, args: Seq[Term]): Term = {
        builtinTerms(ConstrData) $ Term.Const(Constant.Integer(tag)) $ args.foldRight(
          builtinTerms(MkNilData) $ Term.Const(Constant.Unit)
//          Term.Const(Constant.List(DefaultUni.List(DefaultUni.Data), Nil))
        ) { (arg, ls) =>
            builtinTerms(MkCons) $ arg $ ls
        }
    }

    @tailrec
    private def toData(arg: Term, tp: SIRType): Term = {
        tp match
            case SIRType.Data => arg
            case SIRType.Boolean =>
                builtinTerms(IfThenElse) $ arg $ constrData(1, Nil) $ constrData(0, Nil)
            case SIRType.Integer    => builtinTerms(IData) $ arg
            case SIRType.ByteString => builtinTerms(BData) $ arg
            case SIRType.String =>
                builtinTerms(BData) $ (builtinTerms(EncodeUtf8) $ arg)
            case SIRType.Unit => constrData(0, Nil)
            case SIRType.BLS12_381_G1_Element =>
                toData(builtinTerms(Bls12_381_G1_compress) $ arg, SIRType.ByteString)
            case SIRType.BLS12_381_G2_Element =>
                toData(builtinTerms(Bls12_381_G2_compress) $ arg, SIRType.ByteString)
            case _: SIRType.CaseClass =>
                println(s"toData: ${tp.show}")
                arg
            case _: SIRType.SumCaseClass =>
                println(s"toData: ${tp.show}")
                arg
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported type: ${tp.show} of term: ${arg.show}"
                )
    }

    private def unconstr(term: Term): Term = builtinTerms(UnConstrData) $ term

    private def fromData(arg: Term, tp: SIRType): Term = {
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
    }

    private def getFieldByIndex(args: Term, fieldIndex: Long, tp: SIRType) = {
        var expr = args
        var i = 0
        while i < fieldIndex do
            expr = builtinTerms(TailList) $ expr
            i += 1
        val data = builtinTerms(HeadList) $ expr
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
    private def genMatch(cases: Seq[SIR.Case], args: Term, tag: Term): Term = {
        var term = lowerInner(SIR.Error("MatchError", null))
        for (cs, idx) <- cases.zipWithIndex do
            val bodyTerm = lowerInner(cs.body)
            val bindings = cs.bindings.zipWithIndex
                .zip(cs.constr.params)
                .foldRight(bodyTerm):
                    case (((name, idx), TypeBinding(_, tp)), term) =>
                        val value = getFieldByIndex(args, idx, tp)
                        lam(name)(term) $ value

            term =
                val cond = builtinTerms(EqualsInteger) $ idx.asTerm $ tag
                !(builtinTerms(IfThenElse) $ cond $ ~bindings $ ~term)
        term
    }

    private val mapping: Map[String, Asdf] = Map(
      "scalus.ledger.api.v3.TxId" -> Asdf(
        toData = { args => args.head },
        select = {
            case 0 => identity
            case i => throw new IllegalArgumentException(s"Invalid field index $i for TxId")

        },
        genMatch = { case SIR.Match(scrutinee, cases, _) =>
            val scrutineeTerm = lowerInner(scrutinee)
            cases match
                case SIR.Case(constr, bindings, _, body) :: Nil =>
                    λ(cases.head.bindings.head)(lowerInner(body)) $ scrutineeTerm
                case _ => throw new IllegalArgumentException("Expected single case for TxId")
        }
      )
    )

    private def lowerInner(sir: SIR): Term = {
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args) =>
                println(s"Constr: $name, ${data.name}, $args")
                val tag = data.constructors.indexWhere(_.name == name, 0)
                if tag == -1 then
                    throw new IllegalArgumentException(s"Constructor $name not found in $data")
                val loweredArgs = args.map: arg =>
                    toData(lowerInner(arg), arg.tp)
                if mapping.contains(data.name)
                then
                    val term = mapping(data.name).toData(loweredArgs)
                    println(s"Term: ${term.showHighlighted}")
                    term
                else constrData(tag, loweredArgs)
            case m @ SIR.Match(scrutinee, cases, tp) =>
                val scrutineeTerm = lowerInner(scrutinee)
                scrutinee.tp match
                    case SIRType.SumCaseClass(decl, _) =>
                        mapping.get(decl.name) match
                            case Some(asdf) =>
                                asdf.genMatch(m)
                            case None =>
                                val pair = unconstr(scrutineeTerm)
                                λλ("pair") { pair =>
                                    λλ("tag") { tag =>
                                        λλ("args") { args =>
                                            genMatch(cases, args, tag)
                                        } $ (builtinTerms(SndPair) $ pair)
                                    } $ (builtinTerms(FstPair) $ pair)
                                } $ pair
                    case _ =>
                        throw new IllegalArgumentException(
                          s"Expected case class type, got ${scrutinee.tp} in expression: ${sir.show}"
                        )
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
                def find(sirType: SIRType): (String, ConstrDecl) =
                    sirType match
                        case SIRType.CaseClass(constrDecl, _) => (constrDecl.name, constrDecl)
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
                        case Some(asdf) =>
                            asdf.select(fieldIndex)(instance)
                        case None =>
                            val args = builtinTerms(SndPair) $ unconstr(instance)
                            getFieldByIndex(args, fieldIndex, constrDecl.params(fieldIndex).tp)
                }
                val (name, constrDecl) = find(scrutinee.tp)
                lowerSelect(name, constrDecl)
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
    }