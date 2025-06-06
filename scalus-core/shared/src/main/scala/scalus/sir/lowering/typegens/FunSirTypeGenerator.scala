package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*

case class FunSirTypeGenerator(genInput: SirTypeUplcGenerator, genOutput: SirTypeUplcGenerator)
    extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        LambdaRepresentation(
          genInput.defaultRepresentation,
          genOutput.defaultRepresentation
        )

    override def defaultDataRepresentation: LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentaion acceoss function type generator"
        )

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        (input.representation, outputRepresentation) match {
            case (
                  LambdaRepresentation(inputFrom, inputTo),
                  LambdaRepresentation(outputFrom, outputTo)
                ) =>
                if inputFrom == outputFrom && inputTo == outputTo then input
                else
                    val sirFunType = retrieveSirFun(input.sirType).getOrElse(
                      throw LoweringException(
                        s"Expected function type, got ${input.sirType.show}",
                        pos
                      )
                    )
                    lvLamAbs(
                      lctx.uniqueVarName("lambdaArg"),
                      sirFunType.in,
                      outputFrom,
                      v =>
                          lvApply(
                            input,
                            v.toRepresentation(inputFrom, pos),
                            pos,
                            Some(sirFunType.out),
                            Some(inputTo)
                          ).toRepresentation(outputTo, pos),
                      pos
                    )
            case _ =>
                throw LoweringException(
                  s"Expected function representation, got ${input.representation} and $outputRepresentation",
                  pos
                )
        }
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

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
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
