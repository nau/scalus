package scalus.sir.lowering.typegens

import scala.collection.mutable

import scalus.sir.*
import scalus.sir.SIR.Pattern
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

    /** Prepares cases withour wildcards, ordered the same as in enum definition, for match
      * expression,
      *
      * @param matchData
      * @param loweredScrutinee
      * @param lctx
      * @return
      */
    private def prepareOrderedCased(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): List[SIR.Case] = {

        val cases = matchData.cases
        val anns = matchData.anns

        val constructors = findConstructors(tp, anns.pos)

        // 1. If we have a wildcard case, it must be the last one
        // 2. Validate we don't have any errors
        // 3. Convert Wildcard to the rest of the cases/constructors
        // 4. Sort the cases by constructor name

        var idx = 0

        val allConstructors = constructors.toSet
        val matchedConstructors = mutable.HashSet.empty[String]
        val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]

        // when we have a deconstruction like this:
        // val Some(x) = expr
        // Scala compiler generates an @unchecked annotation
        // and code like this:
        // val x = expr match
        //   case Some(x) => x
        // }
        // which doesn't have a wildcard case
        // so we need to add a wildcard case in this case ;)
        val isUnchecked = anns.data.contains("unchecked")
        val enhancedCases =
            if isUnchecked && cases.length < allConstructors.size then
                cases :+ SIR.Case(
                  SIR.Pattern.Wildcard,
                  SIR.Error("Unexpected case", anns),
                  anns
                )
            else cases

        val casesIter = enhancedCases.iterator

        while casesIter.hasNext do
            casesIter.next() match
                case c @ SIR.Case(SIR.Pattern.Constr(constrDecl, _, _), _, _) =>
                    matchedConstructors += constrDecl.name // collect all matched constructors
                    expandedCases += c
                case SIR.Case(SIR.Pattern.Wildcard, rhs, anns) =>
                    // If we have a wildcard case, it must be the last one
                    if idx != enhancedCases.length - 1 then
                        throw new IllegalArgumentException(
                          s"Wildcard case must be the last and only one in match expression"
                        )
                    else
                        // Convert Wildcard to the rest of the cases/constructors
                        val missingConstructors =
                            allConstructors.filter(c => !matchedConstructors.contains(c.name))
                        missingConstructors.foreach { constrDecl =>
                            val bindings =
                                constrDecl.params.map(p => s"__scalus_unused_binding_${p.name}")
                            // TODO: extract rhs to a let binding before the match
                            // so we don't have to repeat it for each case
                            // also we have no way to know type-arguments, so use abstract type-vars (will use FreeUnificator)
                            val typeArgs =
                                constrDecl.typeParams.map(_ => SIRType.FreeUnificator)
                            expandedCases += SIR.Case(
                              Pattern.Constr(constrDecl, bindings, typeArgs),
                              rhs,
                              anns
                            )
                            matchedConstructors += constrDecl.name // collect all matched constructors
                        }
            idx += 1
        end while
        // Sort the cases by the same order as the constructors
        val orderedCases = constructors.map { constr =>
            val optExpandedCase = expandedCases.find(_.pattern match {
                case Pattern.Constr(constrDecl, _, _) => constrDecl.name == constr.name
                case _                                => false
            })
            optExpandedCase.getOrElse(
              throw new IllegalArgumentException(
                s"Missing case for constructor ${constr.name} at ${anns.pos.file}: ${anns.pos.startLine}, ${anns.pos.startColumn}"
              )
            )
        }.toList

        orderedCases
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
