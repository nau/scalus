package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.sir.lowering.ProductCaseClassRepresentation.{DataConstr, PackedDataList, ProdDataList, ScottEncoding, UplcConstr}
import scalus.sir.lowering.SumCaseClassRepresentation.SumDataList
import scalus.uplc.*

object ProductCaseSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.ProdDataList

    override def defaultDataRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedDataList

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (ProdDataList, ProdDataList) => input
            case (ProdDataList, PackedDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.PackedDataList,
                  pos
                )
            case (ProdDataList, DataConstr) =>
                val constrIndex = retrieveConstrIndex(input.sirType, pos)
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvIntConstant(constrIndex, pos),
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.DataConstr,
                  pos
                )
            case (ProdDataList, UplcConstr) =>
                ???
            case (ProdDataList, ScottEncoding) =>
                ???
            case (ProdDataList, outRep @ ProductCaseClassRepresentation.OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (PackedDataList, ProdDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
            case (PackedDataList, PackedDataList) =>
                input
            case (PackedDataList, UplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (PackedDataList, ScottEncoding) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(ScottEncoding, pos)
            case (
                  PackedDataList,
                  outputRep @ ProductCaseClassRepresentation.OneElementWrapper(_)
                ) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(outputRep, pos)
            case (DataConstr, ProdDataList) =>
                val pairIntDataList = lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.Pair(SIRType.Integer, SIRType.Data),
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
                lvBuiltinApply(
                  SIRBuiltins.sndPair,
                  pairIntDataList,
                  input.sirType,
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
            case (DataConstr, PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (DataConstr, DataConstr) =>
                input
            case (DataConstr, UplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (UplcConstr, ProdDataList) => ???
            case (UplcConstr, PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (UplcConstr, DataConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(DataConstr, pos)
            case (UplcConstr, UplcConstr) =>
                input
            case (ScottEncoding, ProdDataList) =>
                ???
            case (ScottEncoding, PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (ScottEncoding, DataConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(DataConstr, pos)
            case (ScottEncoding, UplcConstr) =>
                ???
            case (ScottEncoding, ScottEncoding) =>
                input
            case (ProductCaseClassRepresentation.OneElementWrapper(_), ProdDataList) =>
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  input,
                  lvBuiltinApply0(
                    SIRBuiltins.mkNilData,
                    SIRType.List(SIRType.Data),
                    PrimitiveRepresentation.Constant,
                    pos
                  ),
                  input.sirType,
                  ProdDataList,
                  pos
                )
            case (ProductCaseClassRepresentation.OneElementWrapper(_), PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (ProductCaseClassRepresentation.OneElementWrapper(_), DataConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(DataConstr, pos)
            case (ProductCaseClassRepresentation.OneElementWrapper(_), UplcConstr) =>
                ???
            case (ProductCaseClassRepresentation.OneElementWrapper(_), ScottEncoding) =>
                ???
            case _ =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val asDataConstr = input.toRepresentation(
          ProductCaseClassRepresentation.DataConstr,
          pos
        )
        new ProxyLoweredValue(asDataConstr) {
            override def sirType: SIRType = targetType

            override def representation: LoweredValueRepresentation =
                SumCaseClassRepresentation.DataConstr

            override def termInternal(gctx: TermGenerationContext): Term = {
                asDataConstr.termInternal(gctx)
            }
        }
    }

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val argTypeGens = constr.args.map(_.tp).map(lctx.typeGenerator)
        val isDataSupported = constr.args.zip(argTypeGens).forall { case (arg, typeGen) =>
            typeGen.isDataSupported(arg.tp)
        }
        val loweredArgs = constr.args.map(arg => lctx.lower(arg))
        if !isDataSupported then genConstrUplcConstr(constr)
        else
            // check majority
            val nDataCentric = loweredArgs.count(_.representation.isDataCentric)
            if nDataCentric >= loweredArgs.size / 2 then
                genConstrDataConstr(constr, loweredArgs, argTypeGens)
            else
                // also check data becoud genContrUplcConstr is not implemented yet
                genConstrDataConstr(constr, loweredArgs, argTypeGens)
                // genConstrUplcConstr(constr, loweredArgs, argTypeGens)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // use data for now
        // TODO: optimize
        genSelectDataList(sel, loweredScrutinee)
    }

    def genSelectDataList(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val dataListScrutinee = loweredScrutinee.toRepresentation(ProdDataList, sel.anns.pos)
        // val prevScope = lctx.scope
        val list0: IdentifiableLoweredValue =
            if dataListScrutinee.isInstanceOf[IdentifiableLoweredValue] then
                dataListScrutinee.asInstanceOf[IdentifiableLoweredValue]
            else
                lvNewLazyIdVar(
                  lctx.uniqueVarName("list_sel"),
                  dataListScrutinee.sirType,
                  SumDataList,
                  dataListScrutinee,
                  sel.anns.pos
                )
        val list0id = list0.id
        val constrDecl = retrieveConstrDecl(loweredScrutinee.sirType, sel.anns.pos)
        val fieldIndex = constrDecl.params.indexWhere(_.name == sel.field)
        if fieldIndex < 0 then
            throw LoweringException(
              s"Unknown field ${sel.field} for ${constrDecl.name}",
              sel.anns.pos
            )
        val selHeadList = (0 until fieldIndex).foldLeft(list0) { (acc, idx) =>
            val tailId = list0id + s"_tail_${idx + 1}"
            val tailLazyVar = lctx.scope.getById(tailId) match
                case Some(v) => v
                case None =>
                    lvNewLazyIdVar(
                      tailId,
                      SIRType.List(SIRType.Data),
                      SumDataList,
                      lvBuiltinApply(
                        SIRBuiltins.tailList,
                        acc,
                        SIRType.List(SIRType.Data),
                        SumDataList,
                        sel.anns.pos
                      ),
                      sel.anns.pos
                    )
            tailLazyVar
        }
        lvBuiltinApply(
          SIRBuiltins.headList,
          selHeadList,
          sel.tp,
          lctx.typeGenerator(sel.tp).defaultDataRepresentation,
          sel.anns.pos
        )
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        genMatchDataList(matchData, loweredScrutinee)
    }

    def genMatchDataList(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val constrDecl = retrieveConstrDecl(loweredScrutinee.sirType, matchData.anns.pos)
        matchData.cases match {
            case oneCase :: Nil =>
                val matchCase = oneCase.pattern match
                    case cs @ SIR.Pattern.Constr(constrDecl1, args, _) =>
                        if constrDecl1.name != constrDecl.name then
                            throw LoweringException(
                              s"Expected constructor ${constrDecl.name}, got ${constrDecl1.name}",
                              matchData.anns.pos
                            )
                        oneCase
                    case SIR.Pattern.Wildcard =>
                        val argsNames = constrDecl.params.map(_.name)
                        val argsTypes = constrDecl.params.map(_.tp)
                        // TODO: add typeArgs to env ?
                        oneCase.copy(pattern = SIR.Pattern.Constr(constrDecl, argsNames, argsTypes))
                val dataList = loweredScrutinee.toRepresentation(
                  ProductCaseClassRepresentation.ProdDataList,
                  loweredScrutinee.pos
                ) match
                    case idv: IdentifiableLoweredValue => idv
                    case other =>
                        lvNewLazyIdVar(
                          lctx.uniqueVarName("_match_data_list"),
                          SIRType.List(SIRType.Data),
                          SumCaseClassRepresentation.SumDataList,
                          other,
                          matchData.anns.pos
                        )
                SumCaseSirTypeGenerator.genMatchDataConstrCase(matchCase, dataList)
            case _ =>
                val myPatternCases = matchData.cases.filter { c =>
                    c.pattern match
                        case SIR.Pattern.Constr(constrDecl1, args, typeArgs) =>
                            constrDecl1.name == constrDecl.name
                        case _ => false
                }
                val myCases =
                    if myPatternCases.isEmpty then
                        matchData.cases.find { c =>
                            c.pattern match
                                case SIR.Pattern.Wildcard => true
                                case _                    => false

                        }.toList
                    else myPatternCases
                if myCases.isEmpty then
                    throw LoweringException(
                      s"Can't find case for ${constrDecl.name}",
                      matchData.anns.pos
                    )
                if myPatternCases.size > 1 then {
                    val casesPositions = myPatternCases.map(_.anns.pos)
                    throw LoweringException(
                      s"More than one case for ${constrDecl.name} found: ${casesPositions.mkString(", ")}",
                      casesPositions.head
                    )
                }
                // TODO: add warnign API to LoweringContext
                println(
                  s"Product case class match should have only one case, but ${matchData.cases.length} found. Non-matched cases will be statically optimized out"
                )
                genMatchDataList(matchData.copy(cases = myCases), loweredScrutinee)
        }
    }

    def genConstrDataConstr(
        constr: SIR.Constr,
        loweredArgs: Seq[LoweredValue],
        argTypeGens: Seq[SirTypeUplcGenerator],
    )(using lctx: LoweringContext): LoweredValue = {
        val dataRepresentations = loweredArgs.zip(argTypeGens).map { case (arg, typeGen) =>
            arg.toRepresentation(typeGen.defaultDataRepresentation, constr.anns.pos)
        }
        // TODO: check UplcConstrOnData, it can be more efficient
        val s0 = lvBuiltinApply0(
          SIRBuiltins.mkNilData,
          SIRType.List(SIRType.Data),
          SumDataList,
          constr.anns.pos
        )
        val dataList = dataRepresentations.foldRight(s0) { (arg, acc) =>
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              arg,
              acc,
              SIRType.List(SIRType.Data),
              SumDataList,
              constr.anns.pos
            )
        }
        val retval = new ProxyLoweredValue(dataList) {
            override def sirType: SIRType = constr.tp

            override def representation: LoweredValueRepresentation = ProdDataList

            override def termInternal(gctx: TermGenerationContext): Term = {
                dataList.termInternal(gctx)
            }

            override def show: String = {
                s"Constr(${constr.tp.show}, ${dataList.show}) at ${sirType.show}"
            }
        }
        retval
    }

    def genConstrUplcConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
    }

    def retrieveConstrIndex(tp: SIRType, pos: SIRPosition): Int = {
        tp match {
            case SIRType.CaseClass(constrDecl, targs, optParent) =>
                optParent match
                    case None => 0
                    case Some(parent) =>
                        val parentDecl = SIRType
                            .retrieveDataDecl(parent)
                            .fold(
                              msg =>
                                  throw LoweringException(
                                    s"Can't retrieve parent decl from ${parent.show}: $msg",
                                    pos
                                  ),
                              identity
                            )
                        val retval = parentDecl.constructors.indexWhere(_.name == constrDecl.name)
                        if retval < 0 then {
                            throw LoweringException(
                              s"Expected case class ${constrDecl.name} with constr ${constrDecl.name}, but it is not found in data declaration",
                              pos
                            )
                        }
                        retval
            case SIRType.TypeLambda(params, body) =>
                retrieveConstrIndex(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveConstrIndex(ref, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${tp.show}",
                  pos
                )
        }
    }

    def retrieveConstrDecl(tp: SIRType, pos: SIRPosition): ConstrDecl = {
        SIRType.retrieveConstrDecl(tp) match
            case Right(decl) => decl
            case Left(msg) =>
                throw LoweringException(
                  s"Can't retrieve constr decl from ${tp.show}: $msg",
                  pos
                )
    }

}
