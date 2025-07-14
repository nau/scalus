package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.SIRType.isProd
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.sir.lowering.SumCaseClassRepresentation.SumDataList

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
        SumCaseClassRepresentation.SumDataPairList
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

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // we have only one representation, so
        (input.representation, representation) match {
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.mapData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataAssocMap,
                  pos
                )
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataList
                ) =>
                //   when it potenitallu can be used -- when one part of the program know, that element
                //     is a list of pairs, but another part - does not know.
                //     (pass to foldLeft, foldLeft expect List as dat)
                // we should change stdlib, do not run such conversions
                //  TODO: output performance warning during compilation.

                ???
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
            case (SumCaseClassRepresentation.SumDataPairList, TypeVarRepresentation(_)) =>
                // TODO: think, are we should convert to SumDataList ?
                input
            case (TypeVarRepresentation(_), SumCaseClassRepresentation.SumDataPairList) =>
                RepresentationProxyLoweredValue(
                  input,
                  SumCaseClassRepresentation.SumDataPairList,
                  pos
                )
            case _ =>
                throw LoweringException(
                  s"SumPairDataListSirTypeGenerator does not support ${input.representation} to $representation conversion",
                  pos
                )
        }
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
