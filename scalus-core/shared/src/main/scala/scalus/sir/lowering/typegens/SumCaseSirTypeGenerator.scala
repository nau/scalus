package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*

class SumCaseSirTypeGenerator(tp: SIRType.SumCaseClass) extends SIRTypeUplcGenerator {

    import scalus.sir.lowering.SumCaseClassRepresentation.*

    override def defaultRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.DataConstr

    override def defaultDataRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.DataConstr

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (DataConstr, DataConstr) =>
                input
            case (DataConstr, DataList) =>
                ???
            case (DataConstr, PackedDataList) =>
                ???
            case (DataConstr, UplcConstr) =>
                ???
            case (DataConstr, UplcConstrOnData) =>
                ???
            case (DataConstr, ScottEncoding) =>
                ???
            case (DataList, DataConstr) =>
                ???
            case (DataList, DataList) =>
                input
            case (DataList, PackedDataList) =>
                ???
            case (DataList, UplcConstr) =>
                ???
            case (DataList, UplcConstrOnData) =>
                ???
            case (DataList, ScottEncoding) =>
                ???
            case (PackedDataList, _) =>
                ???
        }
    }

    override def genSelect(sel: SIR.Select)(using lctx: LoweringContext): LoweredValue = {
        throw LoweringException(
          s"Cannot generate select for ${tp.decl.name} as it is a sum type",
          sel.anns.pos
        )
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"Cannot generate constructor for ${tp.decl.name} as it is a sum type",
          constr.anns.pos
        )
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        loweredScrutinee.representation match {
            case DataConstr =>
                genMatchDataConstr(
                  matchData,
                  loweredScrutinee,
                )
            case DataList =>
                SumDataListGenerator.genMatch(matchData, loweredScrutinee)
            case PackedDataList =>
                SumDataListGenerator.genMatch(matchData, loweredScrutinee)
            case UplcConstr =>
                genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                )
            case UplcConstrOnData =>
                ???
            case ScottEncoding =>
                ???
        }

    }

    /** Prepares ordered cases (the same as in enum definition) for match expression.
      * @param matchData
      * @param loweredScrutinee
      * @param lctx
      * @return
      */
    private def prepareOrderedCased(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): List[SIR.Case] = {
        val constructors = findConstructors(tp, matchData.anns.pos)
        ???
    }

    def genMatchDataConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {

        ???
    }

    def genMatchUplcConstr(
        mathData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        ???
    }

    private def findConstructors(sirType: SIRType, pos: SIRPosition): Seq[ConstrDecl] = {
        sirType match
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                optParent match
                    case None         => Seq(constrDecl)
                    case Some(parent) => findConstructors(parent, pos)
            case SIRType.SumCaseClass(decl, _) =>
                decl.constructors
            case SIRType.TypeLambda(_, t) => findConstructors(t, pos)
            case _ =>
                throw new IllegalArgumentException(
                  s"Expected case class type, got ${sirType} at match at ${pos}"
                )
    }

}
