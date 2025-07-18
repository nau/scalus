package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*

object FunSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        collect(tp) match {
            case Some((typeVars, input, output)) =>
                LambdaRepresentation(
                  lctx.typeGenerator(input).defaultRepresentation(input),
                  lctx.typeGenerator(output).defaultRepresentation(output)
                )

            case None =>
                throw IllegalStateException(
                  s"Function type generator can't be used for type ${tp.show}",
                )
        }

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentaion acceoss function type generator"
        )

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation = {
        // unchanged function.
        defaultRepresentation(tp)
    }

    override def isDataSupported(tp: SIRType)(using LoweringContext): Boolean = false

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        if input.representation == outputRepresentation then input
        else {
            val (typeVars, tpIn, tpOut) = collect(input.sirType)
                .getOrElse(
                  throw LoweringException(
                    s"Function type generator can't convert from ${input.sirType.show} to $outputRepresentation",
                    pos
                  )
                )
            (input.representation, outputRepresentation) match
                case (
                      LambdaRepresentation(inRepr1, outRepr1),
                      LambdaRepresentation(inRepr2, outRepr2)
                    ) =>
                    if inRepr1.isCompatible(inRepr2) && outRepr1.isCompatible(outRepr2) then
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    else
                        val newInName = lctx.uniqueVarName("x")
                        lvLamAbs(
                          newInName,
                          tpIn,
                          inRepr2,
                          x =>
                              lvApply(
                                input,
                                x.toRepresentation(inRepr1, pos),
                                pos,
                                Some(tpOut),
                                Some(outRepr1)
                              ).toRepresentation(outRepr2, pos),
                          pos
                        )
                case (
                      inLambda: LambdaRepresentation,
                      TypeVarRepresentation(isBuiltin)
                    ) =>
                    if isBuiltin then input
                    else
                        RepresentationProxyLoweredValue(
                          input,
                          outputRepresentation,
                          pos
                        )
                case (
                      TypeVarRepresentation(isBuiltin),
                      LambdaRepresentation(outInRepr, outOutRepr)
                    ) =>
                    new RepresentationProxyLoweredValue(
                      input,
                      outputRepresentation,
                      pos
                    )
                case (TypeVarRepresentation(isBuiltin1), TypeVarRepresentation(isBuiltin2)) =>
                    if isBuiltin2 then input
                    else
                        new RepresentationProxyLoweredValue(
                          input,
                          outputRepresentation,
                          pos
                        )
                case _ =>
                    throw LoweringException(
                      s"Unfunctional representation target (${outputRepresentation}) during conversion for ${input.sirType.show} from ${input.representation} to $outputRepresentation",
                      pos
                    )

        }
    }

    private def collect(tp: SIRType): Option[(Set[SIRType.TypeVar], SIRType, SIRType)] = {
        tp match {
            case SIRType.Fun(input, output) =>
                Some((Set.empty, input, output))
            case SIRType.TypeLambda(typeVars, body) =>
                collect(body).map { case (bTypeVars, input, output) =>
                    (typeVars.toSet ++ bTypeVars, input, output)
                }
            case proxy: SIRType.TypeProxy => None
            case tv: SIRType.TypeVar =>
                Some(Set.empty, SIRType.FreeUnificator, SIRType.FreeUnificator)
            case SIRType.TypeProxy(ref) => collect(ref)
            case _                      => None
        }
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException("Function type can't be upcasted", pos)
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          "Constr can't be generated for function type",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"Function type have no fields",
          sel.anns.pos
        )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"Function can't be matched",
          matchData.anns.pos
        )
    }

    private def retrieveSirFun(tp: SIRType): Option[SIRType.Fun] = {
        tp match {
            case SIRType.Fun(input, output)  => Some(SIRType.Fun(input, output))
            case SIRType.TypeLambda(_, body) => retrieveSirFun(body)
            case _                           => None
        }
    }

}
