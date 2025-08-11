package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*

/** List(Pair(List,List))
  */
object SumPairDataListSirTypeGenerator extends SumListCommonSirTypeGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataPairList
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataAssocMap
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataAssocMap
    }

    override def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation = {
        SumCaseClassRepresentation.SumDataPairList
    }

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        val constrDecl = SIRType
            .retrieveConstrDecl(tp)
            .getOrElse(
              throw LoweringException(
                s"SumPair shoul have a pari or tuple type representation, we have  ${tp.show}",
                pos
              )
            )
        if constrDecl.name == "scalus.builtin.Pair" || constrDecl.name == "scala.Tuple2" then
            ProductCaseClassRepresentation.PairData
        else
            throw LoweringException(
              s"SumPair shoul have a pair or tuple type representation, we have  ${tp.show}",
              pos
            )
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue = {
        lvBuiltinApply0(
          SIRBuiltins.mkNilPairData,
          resType,
          SumCaseClassRepresentation.SumDataPairList,
          pos
        )

    }

}
