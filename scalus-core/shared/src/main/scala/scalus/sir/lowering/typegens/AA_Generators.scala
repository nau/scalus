package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
//import scalus.uplc.{DefaultFun, NamedDeBruijn, Term}
//import scalus.uplc.TermDSL.{lam, vr}

// gemerator for
//   case class AA(flag: Boolean, a: BigInt)
//   Term representation is
//   Varints:
//            Constr(0, flag, a)
//            List
//   Data representation:
//            Data
object AA_Generator extends SIRTypeUplcGenerator {

    val name = "scalus.sir.SIRUplcV3LoweringSpec$.AA"

    override def defaultRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.UplcConstr

    override def defaultDataRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.PacketDataConstr

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        ???
    }

    override def genSelect(sel: SIR.Select)(using lctx: LoweringContext): LoweredValue = {
        ???
        /*
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
                
         */
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = ???

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
        // one case - 0,  so we can just apply one.
        /*
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
         */
    }

}
