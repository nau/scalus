package scalus.sir.lowering.typegens

import org.typelevel.paiges.Doc

import scalus.sir.{SIRType, *}
import scalus.sir.lowering.*
import scalus.sir.lowering.Lowering.tpf
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.{DefaultFun, Term}

/** Internal representation - Plutus List, element type should be data-compatibe List[E] when E is
  * data-compatible type is mapped to this type.
  */
object SumDataListSirTypeGenerator extends SumListCommonSirTypeGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        // TODO: pass position
        SumCaseClassRepresentation.SumDataList
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataList

    override def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataList

    override def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        lctx.typeGenerator(tp).defaultDataRepresentation(tp)
    }

    override def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue = {
        lvBuiltinApply0(
          SIRBuiltins.mkNilData,
          SIRType.List.Nil,
          SumCaseClassRepresentation.SumDataList,
          pos
        )
    }


    // def uplcToData(input: Term): Term = {
    //    DefaultFun.ListData.tpf $ input
    // }

    // def dataToUplc(input: Term): Term =
    //    DefaultFun.UnListData.tpf $ input

}
