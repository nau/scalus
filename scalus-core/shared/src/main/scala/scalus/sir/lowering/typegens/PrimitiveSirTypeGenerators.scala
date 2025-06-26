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
    ): LoweredValue = {
        (input.representation, outputRepresentation) match
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.PackedData) =>
                input
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.PackedData) =>
                new RepresentationProxyLoweredValue(input, outputRepresentation, pos) {
                    override def termInternal(gctx: TermGenerationContext): Term =
                        uplcToData(input.termInternal(gctx))
                }
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.Constant) =>
                new RepresentationProxyLoweredValue(input, outputRepresentation, pos) {
                    override def termInternal(gctx: TermGenerationContext): Term =
                        dataToUplc(input.termInternal(gctx))
                }
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.Constant) =>
                if isBuiltin then input
                else
                    new RepresentationProxyLoweredValue(
                      input,
                      outputRepresentation,
                      pos
                    ) {
                        override def termInternal(gctx: TermGenerationContext): Term =
                            dataToUplc(input.termInternal(gctx))
                    }
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.PackedData) =>
                RepresentationProxyLoweredValue(input, outputRepresentation, pos)
            case (PrimitiveRepresentation.Constant, TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else
                    new RepresentationProxyLoweredValue(
                      input,
                      outputRepresentation,
                      pos
                    ) {
                        override def termInternal(gctx: TermGenerationContext): Term =
                            uplcToData(input.termInternal(gctx))
                    }
            case (PrimitiveRepresentation.PackedData, TypeVarRepresentation(isBuiltin)) =>
                input
            case (TypeVarRepresentation(inBuiltin), TypeVarRepresentation(outBuiltin)) =>
                if outBuiltin then input
                else if inBuiltin then
                    // impossible, but let it will be here
                    new RepresentationProxyLoweredValue(input, outputRepresentation, pos) {
                        override def termInternal(gctx: TermGenerationContext): Term =
                            uplcToData(input.termInternal(gctx))
                    }
                else input
            case (_, _) =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $outputRepresentation",
                  pos
                )
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        throw new LoweringException("Primitive value can't be upcasted", pos)
    }

    def uplcToData(input: Term): Term

    def dataToUplc(input: Term)(using LoweringContext): Term

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

    override def dataToUplc(input: Term)(using LoweringContext): Term = {
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

    override def dataToUplc(input: Term)(using LoweringContext): Term = {
        DefaultFun.UnIData.tpf $ input
    }

}

object SIRTypeUplcByteStringGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.BData.tpf $ input
    }

    override def dataToUplc(input: Term)(using LoweringContext): Term = {
        DefaultFun.UnBData.tpf $ input
    }

}

object SIRTypeUplcStringGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToData(input: Term): Term = {
        DefaultFun.BData.tpf $ (DefaultFun.EncodeUtf8.tpf $ input)
    }

    override def dataToUplc(input: Term)(using LoweringContext): Term = {
        DefaultFun.DecodeUtf8.tpf $ (DefaultFun.UnBData.tpf $ input)
    }

}

object SIRTypeUplcUnit1Generator extends PrimitiveSirTypeGenerator {

    override def defaultRepresentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException("unit type can't be represented as packed data")

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def isDataSupported(tp: SIRType)(using LoweringContext): Boolean =
        false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.Constant) =>
                input
            case (PrimitiveRepresentation.Constant, PrimitiveRepresentation.PackedData) =>
                throw LoweringException(
                  "Unit type can't be converted to packed data",
                  pos
                )
            case (PrimitiveRepresentation.PackedData, _) =>
                throw LoweringException(
                  "Unit type have no packed data representation",
                  pos
                )
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.PackedData) =>
                // impossible, but let it be here
                input
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.Constant) =>
                input
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.PackedData) =>
                throw LoweringException(
                  "Unit type can't be converted to packed data",
                  pos
                )
            case (PrimitiveRepresentation.Constant, TypeVarRepresentation(isBuiltin)) =>
                input
            case (PrimitiveRepresentation.PackedData, TypeVarRepresentation(isBuiltin)) =>
                // impossible, but let it be here
                throw LoweringException(
                  "Unit have no packed data representation",
                  pos
                )

    }

    override def uplcToData(input: Term): Term = {

        ???
    }

    override def dataToUplc(input: Term)(using lctx: LoweringContext): Term = {

        ???
    }

}

object SIRTypeUplcDataGenerator extends PrimitiveSirTypeGenerator {

    // data is invariant.
    override def uplcToData(input: Term): Term = {
        input
    }

    override def dataToUplc(input: Term)(using LoweringContext): Term = {
        input
    }

}
