package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*

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
                uplcToDataValue(input, pos)
            case (PrimitiveRepresentation.PackedData, PrimitiveRepresentation.Constant) =>
                dataToUplcValue(input, pos)
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.Constant) =>
                if isBuiltin then input
                else dataToUplcValue(input, pos)
            case (TypeVarRepresentation(isBuiltin), PrimitiveRepresentation.PackedData) =>
                if isBuiltin then uplcToDataValue(input, pos)
                else input
            case (PrimitiveRepresentation.Constant, TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else uplcToDataValue(input, pos)
            case (PrimitiveRepresentation.PackedData, TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then dataToUplcValue(input, pos)
                else input
            case (TypeVarRepresentation(inBuiltin), TypeVarRepresentation(outBuiltin)) =>
                if outBuiltin then input
                else if inBuiltin then {
                    // impossible, but let it will be here
                    RepresentationProxyLoweredValue(
                      uplcToDataValue(input, pos),
                      outputRepresentation,
                      pos
                    )
                } else input
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

    def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using LoweringContext): LoweredValue

    def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using LoweringContext): LoweredValue

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue =
        throw LoweringException("Constr can generated for primitive type", constr.anns.pos)

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(s"Primitive type have no field ${sel.field}", sel.anns.pos)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // TODO: add support
        throw LoweringException(
          s"Primitive type ${matchData.tp.show}  have no match ${matchData}",
          matchData.anns.pos
        )
    }

}

object SIRTypeUplcBooleanGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        lvIfThenElse(
          input,
          lvBuiltinApply(
            SIRBuiltins.iData,
            lvIntConstant(1, pos),
            SIRType.Boolean,
            PrimitiveRepresentation.PackedData,
            pos
          ),
          lvBuiltinApply(
            SIRBuiltins.iData,
            lvIntConstant(0, pos),
            SIRType.Boolean,
            PrimitiveRepresentation.PackedData,
            pos
          ),
          pos
        )

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        lvIfThenElse(
          lvEqualsInteger(
            lvBuiltinApply(
              SIRBuiltins.unIData,
              input,
              SIRType.Integer,
              PrimitiveRepresentation.Constant,
              pos
            ),
            lvIntConstant(0, pos),
            pos
          ),
          lvBoolConstant(false, pos),
          lvBoolConstant(true, pos),
          pos
        )

}

object SIRTypeUplcIntegerGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        lvBuiltinApply(
          SIRBuiltins.iData,
          input,
          SIRType.Integer,
          PrimitiveRepresentation.PackedData,
          pos
        )
    }

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        lvBuiltinApply(
          SIRBuiltins.unIData,
          input,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          pos
        )

}

object SIRTypeUplcByteStringGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        lvBuiltinApply(
          SIRBuiltins.bData,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.PackedData,
          pos
        )

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        lvBuiltinApply(
          SIRBuiltins.unBData,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          pos
        )

}

object SIRTypeUplcStringGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        lvBuiltinApply(
          SIRBuiltins.bData,
          lvBuiltinApply(
            SIRBuiltins.encodeUtf8,
            input,
            SIRType.ByteString,
            PrimitiveRepresentation.Constant,
            pos
          ),
          SIRType.String,
          PrimitiveRepresentation.PackedData,
          pos
        )
    }

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        lvBuiltinApply(
          SIRBuiltins.decodeUtf8,
          lvBuiltinApply(
            SIRBuiltins.unBData,
            input,
            SIRType.ByteString,
            PrimitiveRepresentation.Constant,
            pos
          ),
          SIRType.String,
          PrimitiveRepresentation.Constant,
          pos
        )
    }

}

object SIRTypeUplcDataGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        RepresentationProxyLoweredValue(input, PrimitiveRepresentation.PackedData, pos)

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        RepresentationProxyLoweredValue(input, PrimitiveRepresentation.Constant, pos)

}

object BLS12_381_G1_SirTypeGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val bs = lvBuiltinApply(
          SIRBuiltins.bls12_381_G1_compress,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          pos
        )
        lvBuiltinApply(
          SIRBuiltins.bData,
          bs,
          SIRType.BLS12_381_G1_Element,
          PrimitiveRepresentation.PackedData,
          pos
        )
    }

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val bs = lvBuiltinApply(
          SIRBuiltins.unBData,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          pos
        )
        lvBuiltinApply(
          SIRBuiltins.bls12_381_G1_uncompress,
          input,
          SIRType.BLS12_381_G1_Element,
          PrimitiveRepresentation.Constant,
          pos
        )
    }

}

object BLS12_381_G2_SirTypeGenerator extends PrimitiveSirTypeGenerator {

    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val bs = lvBuiltinApply(
          SIRBuiltins.bls12_381_G2_compress,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          pos
        )
        lvBuiltinApply(
          SIRBuiltins.bData,
          bs,
          SIRType.BLS12_381_G2_Element,
          PrimitiveRepresentation.PackedData,
          pos
        )
    }

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val bs = lvBuiltinApply(
          SIRBuiltins.unBData,
          input,
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          pos
        )
        lvBuiltinApply(
          SIRBuiltins.bls12_381_G2_uncompress,
          input,
          SIRType.BLS12_381_G2_Element,
          PrimitiveRepresentation.Constant,
          pos
        )
    }

}

object BLS12_381_MLResultSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = PrimitiveRepresentation.Constant

    override def defaultDataRepresentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        throw IllegalArgumentException("MLResultGenerator does not support data representation")

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        PrimitiveRepresentation.Constant

    override def isDataSupported(tp: SIRType)(using
        lctx: LoweringContext
    ): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        if input.representation == outputRepresentation then input
        else
            throw LoweringException(
              s"MLResultGenerator can't convert from ${input.sirType.show} to $outputRepresentation",
              pos
            )
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"MLResultGenerator can't generate constructor for ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"BLS12_381_Result have no fields (reading field ${sel.field})",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"BLS12_381_Result can't be a match scrutinee",
          matchData.anns.pos
        )
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"BLS12_381_Result can't be upcasted to ${targetType.show}",
          pos
        )

    }

}
