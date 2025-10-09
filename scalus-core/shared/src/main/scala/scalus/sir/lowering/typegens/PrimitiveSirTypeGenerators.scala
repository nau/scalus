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

    /** Boolean represented in data as Constr 0 [] and Constr 1 [] (see definition in plutus:
      * https://github.com/IntersectMBO/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Instances.hs#L24C1-L25C1)
      *
      * @param input
      * @param pos
      * @return
      */
    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =

        val asInt = lvIfThenElse(
          input,
          lvIntConstant(1, pos),
          lvIntConstant(0, pos),
          pos
        )
        lvBuiltinApply2(
          SIRBuiltins.constrData,
          asInt,
          lvBuiltinApply0(
            SIRBuiltins.mkNilData,
            SIRType.BuiltinList(SIRType.Data),
            PrimitiveRepresentation.Constant,
            pos
          ),
          SIRType.Boolean,
          PrimitiveRepresentation.PackedData,
          pos
        )

    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =

        val unconstr = lvBuiltinApply(
          SIRBuiltins.unConstrData,
          input,
          SIRType.BuiltinPair(
            SIRType.Integer,
            SIRType.BuiltinList(SIRType.Data)
          ),
          PrimitiveRepresentation.Constant,
          pos
        )
        val asInt = lvBuiltinApply(
          SIRBuiltins.fstPair,
          unconstr,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          pos
        )
        lvIfThenElse(
          lvEqualsInteger(
            asInt,
            lvIntConstant(0, pos),
            pos
          ),
          lvBoolConstant(false, pos),
          lvBoolConstant(true, pos),
          pos
        )

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val scrutineeInConstRepr = loweredScrutinee.toRepresentation(
          PrimitiveRepresentation.Constant,
          matchData.scrutinee.anns.pos
        )

        def processCases(cases: List[SIR.Case], matchedValues: Set[Boolean]): LoweredValue =
            cases match {
                case Nil =>
                    if isUnchecked then
                        lctx.lower(
                          SIR.Error("Non-exhaustive pattern match for Boolean", matchData.anns),
                          optTargetType
                        )
                    else
                        throw LoweringException(
                          s"Non-exhaustive pattern match for Boolean",
                          matchData.anns.pos
                        )
                case SIR.Case(SIR.Pattern.Const(constValue), body, anns) :: rest =>
                    constValue.uplcConst match {
                        case scalus.uplc.Constant.Bool(boolValue) =>
                            val newMatched = matchedValues + boolValue
                            // Check if after this case all boolean values are covered
                            val isExhaustive =
                                newMatched.contains(true) && newMatched.contains(false)

                            if rest.isEmpty && isExhaustive then {
                                // Last case and exhaustive - just return the body
                                lctx.lower(body, optTargetType)
                            } else {
                                // Need to generate if-then-else
                                val thenBranch = lctx.lower(body, optTargetType)
                                val elseBranch = processCases(rest, newMatched)
                                // if constValue is true: if scrutinee then body else rest
                                // if constValue is false: if scrutinee then rest else body
                                if boolValue then
                                    lvIfThenElse(
                                      scrutineeInConstRepr,
                                      thenBranch,
                                      elseBranch,
                                      anns.pos
                                    )
                                else
                                    lvIfThenElse(
                                      scrutineeInConstRepr,
                                      elseBranch,
                                      thenBranch,
                                      anns.pos
                                    )
                            }
                        case _ =>
                            throw LoweringException(
                              s"Expected Boolean constant, got ${constValue.uplcConst}",
                              anns.pos
                            )
                    }

                case SIR.Case(SIR.Pattern.Wildcard, body, anns) :: rest =>
                    if rest.nonEmpty then
                        throw LoweringException(
                          s"Wildcard pattern must be the last case",
                          anns.pos
                        )
                    lctx.lower(body, optTargetType)

                case SIR.Case(SIR.Pattern.Constr(_, _, _), _, anns) :: _ =>
                    throw LoweringException(
                      s"Constructor pattern not supported for Boolean",
                      anns.pos
                    )
            }

        processCases(matchData.cases, Set.empty)
    }

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

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val scrutineeInConstRepr = loweredScrutinee.toRepresentation(
          PrimitiveRepresentation.Constant,
          matchData.scrutinee.anns.pos
        )

        def processCases(cases: List[SIR.Case]): LoweredValue = cases match {
            case Nil =>
                if isUnchecked then
                    lctx.lower(
                      SIR.Error("Non-exhaustive pattern match for Integer", matchData.anns),
                      optTargetType
                    )
                else
                    throw LoweringException(
                      s"Non-exhaustive pattern match for Integer",
                      matchData.anns.pos
                    )
            case SIR.Case(SIR.Pattern.Const(constValue), body, anns) :: rest =>
                val loweredConst = lctx.lower(constValue)
                val constInConstRepr = loweredConst.toRepresentation(
                  PrimitiveRepresentation.Constant,
                  anns.pos
                )
                val comparison = lvEqualsInteger(scrutineeInConstRepr, constInConstRepr, anns.pos)
                val thenBranch = lctx.lower(body, optTargetType)
                val elseBranch = processCases(rest)
                lvIfThenElse(comparison, thenBranch, elseBranch, anns.pos)

            case SIR.Case(SIR.Pattern.Wildcard, body, anns) :: rest =>
                if rest.nonEmpty then
                    throw LoweringException(
                      s"Wildcard pattern must be the last case",
                      anns.pos
                    )
                lctx.lower(body, optTargetType)

            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, anns) :: _ =>
                throw LoweringException(
                  s"Constructor pattern not supported for Integer",
                  anns.pos
                )
        }

        processCases(matchData.cases)
    }

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

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val scrutineeInConstRepr = loweredScrutinee.toRepresentation(
          PrimitiveRepresentation.Constant,
          matchData.scrutinee.anns.pos
        )

        def processCases(cases: List[SIR.Case]): LoweredValue = cases match {
            case Nil =>
                if isUnchecked then
                    lctx.lower(
                      SIR.Error("Non-exhaustive pattern match for ByteString", matchData.anns),
                      optTargetType
                    )
                else
                    throw LoweringException(
                      s"Non-exhaustive pattern match for ByteString",
                      matchData.anns.pos
                    )
            case SIR.Case(SIR.Pattern.Const(constValue), body, anns) :: rest =>
                val loweredConst = lctx.lower(constValue)
                val constInConstRepr = loweredConst.toRepresentation(
                  PrimitiveRepresentation.Constant,
                  anns.pos
                )
                val comparison = lvBuiltinApply2(
                  SIRBuiltins.equalsByteString,
                  scrutineeInConstRepr,
                  constInConstRepr,
                  SIRType.Boolean,
                  PrimitiveRepresentation.Constant,
                  anns.pos
                )
                val thenBranch = lctx.lower(body, optTargetType)
                val elseBranch = processCases(rest)
                lvIfThenElse(comparison, thenBranch, elseBranch, anns.pos)

            case SIR.Case(SIR.Pattern.Wildcard, body, anns) :: rest =>
                if rest.nonEmpty then
                    throw LoweringException(
                      s"Wildcard pattern must be the last case",
                      anns.pos
                    )
                lctx.lower(body, optTargetType)

            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, anns) :: _ =>
                throw LoweringException(
                  s"Constructor pattern not supported for ByteString",
                  anns.pos
                )
        }

        processCases(matchData.cases)
    }

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

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val scrutineeInConstRepr = loweredScrutinee.toRepresentation(
          PrimitiveRepresentation.Constant,
          matchData.scrutinee.anns.pos
        )

        def processCases(cases: List[SIR.Case]): LoweredValue = cases match {
            case Nil =>
                if isUnchecked then
                    lctx.lower(
                      SIR.Error("Non-exhaustive pattern match for String", matchData.anns),
                      optTargetType
                    )
                else
                    throw LoweringException(
                      s"Non-exhaustive pattern match for String",
                      matchData.anns.pos
                    )
            case SIR.Case(SIR.Pattern.Const(constValue), body, anns) :: rest =>
                val loweredConst = lctx.lower(constValue)
                val constInConstRepr = loweredConst.toRepresentation(
                  PrimitiveRepresentation.Constant,
                  anns.pos
                )
                val comparison = lvBuiltinApply2(
                  SIRBuiltins.equalsString,
                  scrutineeInConstRepr,
                  constInConstRepr,
                  SIRType.Boolean,
                  PrimitiveRepresentation.Constant,
                  anns.pos
                )
                val thenBranch = lctx.lower(body, optTargetType)
                val elseBranch = processCases(rest)
                lvIfThenElse(comparison, thenBranch, elseBranch, anns.pos)

            case SIR.Case(SIR.Pattern.Wildcard, body, anns) :: rest =>
                if rest.nonEmpty then
                    throw LoweringException(
                      s"Wildcard pattern must be the last case",
                      anns.pos
                    )
                lctx.lower(body, optTargetType)

            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, anns) :: _ =>
                throw LoweringException(
                  s"Constructor pattern not supported for String",
                  anns.pos
                )
        }

        processCases(matchData.cases)
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
