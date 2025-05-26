package scalus.sir.lowering

import scalus.sir.lowering.{LoweredValue, LoweringContext, SIRTypeUplcBooleanGenerator, SIRTypeUplcGenerator}
import scalus.sir.*
import scalus.sir.SIR.Pattern
import scalus.sir.SIR.Pattern.Constr
import scalus.sir.SIRVarStorage.{DEFAULT, Data, ScottEncoding}
import scalus.sir.lowering.Lowering.lowerSIR
import scalus.uplc.*
import scalus.uplc.TermDSL.*

trait SIRTypeUplcGenerator {

    /** Get default representation. Assumed that input parameters of functions and vars and are in
      * this representation.
      */
    def defaultRepresentation: LoweredValueRepresentation

    def toRepresentation(input: LoweredValue, representation: LoweredValueRepresentation)(using
        lctx: LoweringContext
    ): LoweredValue

    /** Generate constructor for this type. Always called on DataDecl.tp
      */
    def genConstr(constr: SIR.Constr)(using
        LoweringContext
    ): LoweredValue

    /** Parse constructor for this type, Called during generation of math statement.
      * @param unconstr - pattern, names in pattern can be used when performing match.
      * @param body
      * @param x$3
      * @return
      */
    def genCaseLambda(
        unconstr: SIR.Pattern.Constr,
        body: SIR,
        patternRepresentation: LoweredValueRepresentation,
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val paramNames = unconstr.constr.params.map(_.name)
        lctx.lower(body)
        val uplc = Term.lam(paramNames*)(lctx.lower(body).term)
    }

    def genSelect(sel: SIR.Select)(using LoweringContext): LoweredValue

    def genMatch(matchData: SIR.Match)(using LoweringContext): LoweredValue

}

object SIRTypeUplcGenerator {

    def apply(tp: SIRType): SIRTypeUplcGenerator = {
        tp match
            case SIRType.Boolean =>
                SIRTypeUplcBooleanGenerator
            case SIRType.Integer =>
                SIRTypeUplcIntegerGenerator
            case _ =>
                ???
    }

}

import Lowering.*

object SIRTypeUplcBooleanGenerator extends SIRTypeUplcGenerator {

    override def uplcToData(input: Term)(using lctx: LoweringContext): Term = {
        DefaultFun.IfThenElse.tpf $ input $
            LoweredValue.intConstant(1).asData.term $
            LoweredValue.intConstant(0).asData.term
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term = {
        DefaultFun.IfThenElse.tpf $ (
          DefaultFun.EqualsInteger.tpf $ (DefaultFun.UnIData.tpf $ input) $ Term
              .Const(Constant.Integer(0))
        ) $ (Term.Const(Constant.Bool(false))) $ (Term.Const(Constant.Bool(true)))

    }

    def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        throw LoweringException("Constr can generated for boolean type", constr.anns.pos)

    override def genSelect(sel: SIR.Select)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(s"Boolean type have no field ${sel.field}", sel.anns.pos)
    }

    override def genMatch(matchData: SIR.Match)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // TODO: add support
        throw LoweringException(s"Boolean type have no match ${matchData}", matchData.anns.pos)
    }

}

object SIRTypeUplcIntegerGenerator extends SIRTypeUplcGenerator {

    override def uplcToData(input: Term)(using lctx: LoweringContext): Term = {
        DefaultFun.IData.tpf $ input
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term = {
        DefaultFun.UnIData.tpf $ input
    }

    def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        throw LoweringException("Constr can generated for integer type", constr.anns.pos)

    override def genSelect(sel: SIR.Select)(using LoweringContext): LoweredValue =
        throw LoweringException(s"Integer type have no field ${sel.field}", sel.anns.pos)

    override def genMatch(matchData: SIR.Match)(using LoweringContext): LoweredValue =
        throw LoweringException(s"Integer type have no match ${matchData}", matchData.anns.pos)

}

/** Internal representation - Plutus List
  * @param elementType
  */
class SIRTypeUplcListGenerator(elementType: SIRType) extends SIRTypeUplcGenerator {

    override def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def uplcToData(input: Term)(using lctx: LoweringContext): Term = {
        DefaultFun.ListData.tpf $ input
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term =
        DefaultFun.UnListData.tpf $ input

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        val term = constr.name match
            case "scalus.prelude.List$.Nil" =>
                if constr.args.nonEmpty then
                    throw LoweringException("Non-empy Nil constructor", constr.anns.pos)
                DefaultFun.MkNilData.tpf
            case "scalus.prelude.List$.Cons" =>
                if constr.args.size != 2 then
                    throw LoweringException("Non-empy Nil constructor", constr.anns.pos)
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                DefaultFun.MkCons.tpf $ head.term $ tail.term
            case _ =>
                throw LoweringException(
                  s"Unknown constructor ${constr.name} for List",
                  constr.anns.pos
                )
        LoweredValue(constr, term, SIRVarStorage.ScottEncoding)
    }

    override def genSelect(sel: SIR.Select)(using
        lctx: LoweringContext
    ): LoweredValue =
        throw LoweringException(
          s"Unknown field ${sel.field} for List, which have no fields",
          sel.anns.pos
        )

    override def genMatch(matchData: SIR.Match)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Nil, Cons
        var nilCase: Option[SIR.Case] = None
        var consCase: Option[SIR.Case] = None
        var wildcardCase: Option[SIR.Case] = None
        var noBindingInConsCase = false
        matchData.cases.foreach { cs =>
            cs.pattern match
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == "scalus.prelude.List$.Nil" =>
                    nilCase = Some(cs)
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == "scalus.prelude.List$.Cons" =>
                    consCase = Some(cs)
                case SIR.Pattern.Wildcard =>
                    wildcardCase = Some(cs)
                case _ =>
                    throw LoweringException(s"Unknown pattern ${cs.pattern}", cs.anns.pos)
        }
        if nilCase.isEmpty then
            if wildcardCase.nonEmpty then nilCase = wildcardCase
            else
                // TODO:  generate code to throw runtime exceptions ?
                throw LoweringException("No Nil case in match", matchData.anns.pos)
        if consCase.isEmpty then
            if wildcardCase.nonEmpty then
                consCase = wildcardCase
                noBindingInConsCase = true
            else throw LoweringException("No Cons case in match", matchData.anns.pos)

        val loweredScrutinee = lctx.lower(matchData.scrutinee).asTerm

        val (consHead, consTail) = consCase.get.pattern match
            case SIR.Pattern.Constr(constrDecl, List(h, t), _) =>
                (h, t)
            case _ =>
                throw LoweringException(
                  s"Unknown pattern ${consCase.get.pattern} for List",
                  consCase.get.anns.pos
                )

        val loweredConsBody = lctx.lower(consCase.get.body)
        val br = loweredConsBody.representation
        val loweredNilBody = lctx.lower(nilCase.get.body).toRepresentation(br)

        val term =
            if noBindingInConsCase then
                Term.Force(
                  DefaultFun.ChooseList.tpf $ loweredScrutinee.term $
                      Term.Delay(loweredNilBody.term) $
                      Term.Delay(loweredConsBody.term)
                )
            else if lctx.plutusVersion >= 3 then
                val listVarName = lctx.uniqueVarName("_list")
                val listVar = Term.Var(NamedDeBruijn(listVarName))
                val lambdaConsCase = Term.LamAbs(
                  listVarName,
                  DefaultFun.ChooseList.tpf $ listVar $ Term.Delay(loweredNilBody.term) $
                      Term.Delay(
                        lam(consHead, consTail)(
                          loweredConsBody.term
                        ) $ (DefaultFun.HeadList.tpf $ listVar) $ (DefaultFun.TailList.tpf $ listVar)
                      )
                )
                lambdaConsCase $ loweredScrutinee.term
            // TODO: enable when CaseList become available.
            // else if (plutusVersion >= 4) then
            //    // Question - are we need wrap CaseList in Force ?
            //    (DefaultFun.CaseList.tpf $ matchData.scrutinee.asTerm
            //        $ Term.Delay(loweredNilBody.term)
            //        $ lam(consPatternNames: _*)(loweredConsBody))
            else
                throw LoweringException(
                  s"Unsupported plutus version ${lctx.plutusVersion}",
                  matchData.anns.pos
                )

        LoweredValue(matchData, term, br)
    }

}

/** Internal representation - Plutus List
  * @param elemTp
  */
class SIRTypeUplcConsGenerator(elemTp: SIRType) extends SIRTypeUplcGenerator {

    override def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def uplcToData(input: Term)(using lctx: LoweringContext): Term = {
        val listTerm = DefaultFun.ListData.tpf $ input
        val xVar = Term.Var(NamedDeBruijn("_x"))
        val checkList = Term.LamAbs(
          "_x",
          DefaultFun.ChooseList.tpf $ xVar $ ~genError("Empty list in cons") $ ~xVar
        )
        checkList $ listTerm
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term = {
        val listTerm = DefaultFun.UnListData.tpf $ input
        val xVar = Term.Var(NamedDeBruijn("_x"))
        val checkList = Term.LamAbs(
          "_x",
          DefaultFun.ChooseList.tpf $ xVar $ ~genError("Empty list in cons") $ ~xVar
        )
        checkList $ listTerm
    }

    override def genSelect(sel: SIR.Select)(using lctx: LoweringContext): LoweredValue = {
        sel.field match
            case "head" =>
                val dataTerm = DefaultFun.HeadList.tpf $ lctx.lower(sel.scrutinee).asTerm.term
                LoweredValue(sel, dataTerm, SIRVarStorage.Data)
            case "tail" =>
                val listTerm = DefaultFun.TailList.tpf $ lctx.lower(sel.scrutinee).asTerm.term
                LoweredValue(sel, listTerm, SIRVarStorage.ScottEncoding)
            case _ =>
                throw LoweringException(s"Unknown field ${sel.field} for Cons", sel.anns.pos)
    }

    override def genMatch(matchData: SIR.Match)(using lctx: LoweringContext): LoweredValue = {
        // TODO: think, accept 3
        ???
    }

}

// gemerator for
//   case class AA(flag: Boolean, a: BigInt)
//   Term representation is
//   Varints:
//            Constr(0, flag, a)
//            List
//   Data representation:
//            Data
object AA_Generator_Constr extends SIRTypeUplcGenerator {

    val name = "scalus.sir.SIRUplcV3LoweringSpec$.AA"

    override def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def uplcToData(input: Term)(using lctx: LoweringContext): Term = {
        Term.Case(
          input,
          List(
            Term.Case(
              input,
              List(
                lam("a", "b")(
                  DefaultFun.MkCons.tpf $ SIRTypeUplcBooleanGenerator.uplcToData(vr("a"))
                      $ (DefaultFun.MkCons.tpf $ vr("b") $ DefaultFun.MkNilData.tpf)
                )
              )
            )
          )
        )
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term = {
        val listVarName = lctx.uniqueVarName("_list")
        val listVar = Term.Var(NamedDeBruijn(listVarName))
        val listToConstr = Term.LamAbs(
          listVarName,
          Term.Constr(
            0,
            List(DefaultFun.HeadList.tpf $ listVar, DefaultFun.TailList.tpf $ listVar)
          )
        )
        listToConstr $ (DefaultFun.UnListData.tpf $ input)
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        val term = Term.Constr(
          0,
          List(
            lctx.lower(constr.args.head).asTerm.term,
            lctx.lower(constr.args.tail.head).asTerm.term
          )
        )
        LoweredValue(constr, term, SIRVarStorage.ScottEncoding)
    }

    override def genSelect(sel: SIR.Select)(using lctx: LoweringContext): LoweredValue = {
        val loweredScrutinee = lctx.lower(sel.scrutinee).asTerm
        loweredScrutinee.representation match
            case SIRVarStorage.Data =>
                val listTerm = DefaultFun.UnListData.tpf $ loweredScrutinee.term
                val term = sel.field match
                    case "flag" =>
                        DefaultFun.HeadList.tpf $ listTerm
                    case "a" =>
                        DefaultFun.HeadList.tpf $ (DefaultFun.TailList.tpf $ listTerm)
                    case _ =>
                        throw LoweringException(s"Unknown field ${sel.field} for A", sel.anns.pos)
                LoweredValue(sel, term, SIRVarStorage.Data)
            case SIRVarStorage.ScottEncoding =>
                val caseFun = sel.field match
                    case "flag" => lam("flag", "a") { vr("flag") }
                    case "a"    => lam("flag", "a") { vr("a") }
                    case _ =>
                        throw LoweringException(s"Unknown field ${sel.field} for A", sel.anns.pos)
                val term = Term.Case(loweredScrutinee.term, List(caseFun))
                LoweredValue(sel, term, SIRVarStorage.ScottEncoding)
    }

    override def genMatch(matchData: SIR.Match)(using lctx: LoweringContext): LoweredValue = {
        // one case - 0,  so we can just apply one.
        if matchData.cases.length != 1 then
            throw LoweringException(
              s" AA pattern should have only one case, but ${matchData.cases.length} found",
              matchData.anns.pos
            )
        val loweredScrutine = lctx.lower(matchData.scrutinee)
        val caseData = matchData.cases.head
        val unconstr = caseData.pattern match
            case unconstr @ SIR.Pattern.Constr(constrDecl, List(flag, a), _) => unconstr
            case _ =>
                throw LoweringException(
                  s"Unknown pattern ${caseData.pattern} for AA",
                  caseData.anns.pos
                )
        val loweredBodyLambda = genCaseLambda(
          unconstr,
          caseData.body,
          loweredScrutine.representation,
          caseData.anns.pos
        )
        loweredScrutine.representation match
            case SIRVarStorage.ScottEncoding =>
                val term = Term.Case(loweredScrutine.term, List(loweredBodyLambda))
                LoweredValue(matchData, term, SIRVarStorage.ScottEncoding)
            case SIRVarStorage.Data =>
                val listTerm = DefaultFun.UnListData.tpf $ loweredScrutine.term
                val listName = lctx.uniqueVarName("_list")
                val listVar = Term.Var(NamedDeBruijn(listName))
                val listAcceptor = Term.lam(listName)(
                  loweredBodyLambda $ (DefaultFun.HeadList.tpf $ listVar) $ (
                    DefaultFun.HeadList.tpf $ (DefaultFun.TailList.tpf $ listVar)
                  )
                )
                val term = Term.Apply(listAcceptor, listTerm)
                LoweredValue(matchData, term, loweredBodyLambda)

    }

}

object AA_Generator_List extends SIRTypeUplcGenerator {

    val name = "scalus.sir.SIRUplcV3LoweringSpec$.AA"

    override def defaultRepresentation: SIRVarStorage = SIRVarStorage.ScottEncoding

    override def toData(input: LoweredValue)(using lctx: LoweringContext): LoweredValue = {
        input.representation match
            case SIRVarStorage.Data => input
            case SIRVarStorage.ScottEncoding =>
                val term = DefaultFun.ListData.tpf $ input.term
                LoweredValue(input.sir, term, SIRVarStorage.Data)
    }

}

/** Internal representation - Plutus List
  * @param elemTp
  */
class SIRCaseClassUplcGenerator_List(tpc: SIRType.CaseClass) extends SIRTypeUplcGenerator {

    override def defaultRepresentation: SIRVarStorage = SIRVarStorage.Data

    override def toData(input: LoweredValue)(using lctx: LoweringContext): LoweredValue = {
        input.representation match
            case SIRVarStorage.Data => input
            case SIRVarStorage.ScottEncoding =>
                val term = DefaultFun.UnListData.tpf $ input.term
                LoweredValue(input.sir, term, SIRVarStorage.Data)
    }

}
