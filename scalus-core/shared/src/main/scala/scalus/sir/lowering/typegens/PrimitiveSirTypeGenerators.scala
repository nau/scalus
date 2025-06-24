package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.Lowering.tpf
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.{Constant, DefaultFun, Term}

trait PrimitiveSirTypeGenerator extends SirTypeUplcGenerator {

    def defaultRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    def defaultDataRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation =
        PrimitiveRepresentation.PackedData

    def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = PrimitiveRepresentation.PackedData

    def isDataSupported(tp: SIRType)(using LoweringContext): Boolean = true

    def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue =

        new RepresentationProxyLoweredValue(input, outputRepresentation, pos) {

            override def termInternal(gctx: TermGenerationContext): Term =
                val inputTerm = input.termInternal(gctx)
                genTranslateTermRepresentation(
                  input,
                  inputTerm,
                  outputRepresentation
                )(using
                  gctx
                )

        }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        throw new LoweringException("Primitive value can't be upcasted", pos)
    }

    def genTranslateTermRepresentation(
        inputValue: LoweredValue,
        input: Term,
        outputRepresentation: LoweredValueRepresentation,
    )(using gctx: TermGenerationContext): Term = {
        (inputValue.representation, outputRepresentation) match
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.PackedData) =>
                uplcToData(input)
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.Constant) =>
                dataToUplc(input)
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.PackedData) =>
                input
            case _ =>
                println(s"input value ${inputValue} ")
                println(s"create at:")
                inputValue.createdEx.printStackTrace()
                throw LoweringException(
                  s"Unsupported conversion for ${inputValue.sirType.show} from ${inputValue.representation} to $outputRepresentation",
                  inputValue.pos
                )
    }

    def uplcToData(input: Term): Term

    def dataToUplc(input: Term): Term

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        throw LoweringException("Constr can generated for primitive type", constr.anns.pos)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(s"Primitive type have no field ${sel.field}", sel.anns.pos)
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // TODO: add support
        throw LoweringException(s"Boolean type have no match ${matchData}", matchData.anns.pos)
    }

}

object SIRTypeUplcBooleanGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.IfThenElse.tpf $ input $
            (DefaultFun.IData.tpf $ Term.Const(Constant.Integer(1))) $
            (DefaultFun.IData.tpf $ Term.Const(Constant.Integer(0)))
    }

    override def dataToUplc(input: Term): Term = {
        DefaultFun.IfThenElse.tpf $ (
          DefaultFun.EqualsInteger.tpf $ (DefaultFun.UnIData.tpf $ input) $ Term
              .Const(Constant.Integer(0))
        ) $ Term.Const(Constant.Bool(false)) $ Term.Const(Constant.Bool(true))

    }

}

object SIRTypeUplcIntegerGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.IData.tpf $ input
    }

    override def dataToUplc(input: Term): Term = {
        DefaultFun.UnIData.tpf $ input
    }

}

object SIRTypeUplcByteStringGenerator extends PrimitiveSirTypeGenerator {

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {

        if input.representation == outputRepresentation then input
        else if input.representation == PrimitiveRepresentation.Constant && outputRepresentation == PrimitiveRepresentation.PackedData
        then lvBuiltinApply(SIRBuiltins.bData, input, input.sirType, outputRepresentation, pos)
        else if input.representation == PrimitiveRepresentation.PackedData && outputRepresentation == PrimitiveRepresentation.Constant
        then lvBuiltinApply(SIRBuiltins.unBData, input, input.sirType, outputRepresentation, pos)
        else
            throw LoweringException(
              s"String type can't be converted to $outputRepresentation",
              pos
            )

    }

    override def uplcToData(input: Term): Term = {
        DefaultFun.BData.tpf $ input
    }

    override def dataToUplc(input: Term): Term = {
        DefaultFun.UnBData.tpf $ input
    }

}

object SIRTypeUplcStringGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.BData.tpf $ (DefaultFun.EncodeUtf8.tpf $ input)
    }

    override def dataToUplc(input: Term): Term = {
        DefaultFun.DecodeUtf8.tpf $ (DefaultFun.UnBData.tpf $ input)
    }

}

object SIRTypeUplcUnitGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.MkNilData.tpf $ Term.Const(Constant.Unit)
    }

    override def dataToUplc(input: Term): Term = {
        Term.Const(Constant.Unit)
    }

}

object SIRTypeUplcDataGenerator extends PrimitiveSirTypeGenerator {

    // data is invariant.
    override def uplcToData(input: Term): Term = {
        input
    }

    override def dataToUplc(input: Term): Term = {
        input
    }

}
