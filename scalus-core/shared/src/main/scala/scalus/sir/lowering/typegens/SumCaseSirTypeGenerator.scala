package scalus.sir.lowering.typegens

import scala.collection.mutable
import scalus.sir.{SIRType, *}
import scalus.sir.SIR.Pattern
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.Term

object SumCaseSirTypeGenerator extends SirTypeUplcGenerator {

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
            case (DataConstr, PairIntDataList) =>
                lvBuiltinApply(SIRBuiltins.unConstrData, input, input.sirType, PairIntDataList, pos)
            case (DataConstr, DataList) =>
                ???
            case (DataConstr, PackedDataList) =>
                val asDataList = toRepresentation(input, DataList, pos)
                lvBuiltinApply(SIRBuiltins.listData, asDataList, input.sirType, PackedDataList, pos)
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
                lvBuiltinApply(SIRBuiltins.listData, input, input.sirType, PackedDataList, pos)
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

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetDataDecl = retrieveDataDecl(targetType, pos)
        val dataConstructors = targetDataDecl.constructors
        val inputDataDecl = retrieveDataDecl(input.sirType, pos)
        val constrName = SIRType.syntheticNarrowConstrDeclName(inputDataDecl.name)
        val constrIndex = dataConstructors.indexWhere(_.name == constrName)
        if constrIndex < 0 then {
            throw LoweringException(
              s"Expected case class ${inputDataDecl.name} with constr ${constrName}, but it is not found in data declaration",
              pos
            )
        }
        val inputDataRepr = input.toRepresentation(SumCaseClassRepresentation.DataConstr, pos)

        val inPos = pos

        val constrProduct = new ProxyLoweredValue(inputDataRepr) {

            override def sirType: SIRType =
                targetDataDecl.constrType(constrName)

            override def representation: LoweredValueRepresentation =
                ProductCaseClassRepresentation.OneElementWrapper(inputDataRepr.representation)

            override def pos: SIRPosition = inPos

            override def termInternal(gctx: TermGenerationContext): Term =
                inputDataRepr.termInternal(gctx)
        }

        val constrProductDataConstr =
            constrProduct.toRepresentation(ProductCaseClassRepresentation.DataConstr, pos)

        val retval = new ProxyLoweredValue(constrProductDataConstr) {
            override def sirType: SIRType = targetType
            override def representation: LoweredValueRepresentation =
                SumCaseClassRepresentation.DataConstr

            override def pos: SIRPosition = inPos

            override def termInternal(gctx: TermGenerationContext): Term =
                constrProductDataConstr.termInternal(gctx)
        }

        retval
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"Cannot generate select for ${sel.tp} as it is a sum type",
          sel.anns.pos
        )
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"Cannot generate constructor for ${constr.tp} in sum type generator",
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
                SumDataListSirTypeGenerator.genMatch(matchData, loweredScrutinee)
            case PackedDataList =>
                SumDataListSirTypeGenerator.genMatch(matchData, loweredScrutinee)
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
    )(using LoweringContext): List[SIR.Case] = {

        val cases = matchData.cases
        val anns = matchData.anns

        val constructors = findConstructors(loweredScrutinee.sirType, anns.pos)

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
        val orderedCases = prepareOrderedCased(matchData, loweredScrutinee)

        val prevScope = lctx.scope

        val scrutineeVarId = lctx.uniqueVarName("_match_scrutinee")
        val scrutineeVar = lvNewLazyIdVar(
          scrutineeVarId,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          loweredScrutinee.toRepresentation(PairIntDataList, matchData.scrutinee.anns.pos),
          matchData.scrutinee.anns.pos
        )

        val constrIdxVarId = lctx.uniqueVarName("_match_constr_idx")
        val constrIdxVar = lvNewLazyIdVar(
          constrIdxVarId,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          lvBuiltinApply(
            SIRBuiltins.fstPair,
            scrutineeVar,
            SIRType.Integer,
            PrimitiveRepresentation.Constant,
            matchData.scrutinee.anns.pos
          ),
          matchData.scrutinee.anns.pos
        )

        val dataListVarId = lctx.uniqueVarName("_match_datalist")
        val dataListVar = lvNewLazyIdVar(
          dataListVarId,
          SIRType.List(SIRType.Data),
          SumCaseClassRepresentation.DataList,
          lvBuiltinApply(
            SIRBuiltins.sndPair,
            scrutineeVar,
            SIRType.List(SIRType.Data),
            SumCaseClassRepresentation.DataList,
            matchData.scrutinee.anns.pos
          ),
          matchData.scrutinee.anns.pos
        )

        val lastTerm = lctx.lower(
          SIR.Error(
            s"Incorrect constructor index for type ${loweredScrutinee.sirType}",
            matchData.anns
          )
        )

        val retval = orderedCases.zipWithIndex.foldRight(lastTerm) {
            case ((sirCase, caseIndex), state) =>
                val body = genMatchDataConstrCase(sirCase, dataListVar)
                lvIfThenElse(
                  lvEqualsInteger(
                    constrIdxVar,
                    lvIntConstant(caseIndex, sirCase.anns.pos),
                    body.pos
                  ),
                  body,
                  state,
                  sirCase.anns.pos
                )
        }

        lctx.scope = prevScope

        retval
    }

    def genMatchDataConstrCase(
        sirCase: SIR.Case,
        dataListVar: IdentifiableLoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        val prevScope = lctx.scope

        val constrPattern = sirCase.pattern match
            case p: Pattern.Constr => p
            case _ =>
                throw new LoweringException(
                  s"Expected constructor pattern, got ${sirCase.pattern}",
                  sirCase.anns.pos
                )

        val dataListId = dataListVar.id

        val listDataType = SIRType.List(SIRType.Data)

        // The sence of this fold is to add to hre scope all bindingd vars
        val (lastTail, n) =
            constrPattern.bindings.zip(constrPattern.typeBindings).foldLeft((dataListVar, 0)) {
                case ((currentTail, idx), (name, tp)) =>
                    val prevId = currentTail.id
                    val tpDataRepresentation =
                        lctx.typeGenerator(tp).defaultDataRepresentation
                    val bindedVar = lvNewLazyNamedVar(
                      name,
                      tp,
                      tpDataRepresentation,
                      lvBuiltinApply(
                        SIRBuiltins.headList,
                        currentTail,
                        tp,
                        tpDataRepresentation,
                        sirCase.anns.pos
                      ),
                      sirCase.anns.pos
                    )
                    val tailId = s"${dataListId}_b${}"
                    // mb we already have this id in the scope
                    val tailVar = lctx.scope.get(tailId, SumCaseClassRepresentation.DataList) match
                        case Some(v) => v
                        case None =>
                            lvNewLazyIdVar(
                              tailId,
                              listDataType,
                              SumCaseClassRepresentation.DataList,
                              lvBuiltinApply(
                                SIRBuiltins.tailList,
                                currentTail,
                                listDataType,
                                SumCaseClassRepresentation.DataList,
                                sirCase.anns.pos
                              ),
                              sirCase.anns.pos
                            )
                    (tailVar, idx + 1)
            }

        // now with the all named variable in the scope we can generate the body
        val body = lctx.lower(sirCase.body)

        lctx.scope = prevScope

        body
    }

    def genMatchUplcConstr(
        mathData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue = {
        ???
    }

    def findConstructors(sirType: SIRType, pos: SIRPosition): Seq[ConstrDecl] = {
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

    def retrieveDataDecl(tp: SIRType, pos: SIRPosition): DataDecl = {
        SIRType.retrieveDataDecl(tp) match
            case Right(decl) => decl
            case Left(msg) =>
                throw LoweringException(
                  s"Can't retrieve data declaration from ${tp.show}: $msg",
                  pos
                )
    }

}
