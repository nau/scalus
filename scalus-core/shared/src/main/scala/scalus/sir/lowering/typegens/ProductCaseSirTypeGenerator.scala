package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.sir.lowering.ProductCaseClassRepresentation.{DataConstr, PackedDataList, ScottEncoding, UplcConstr}
import scalus.sir.lowering.SumCaseClassRepresentation.DataList
import scalus.uplc.*

object ProductCaseSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.DataList

    override def defaultDataRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedDataList

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (DataList, DataList) => input
            case (DataList, PackedDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.PackedDataList,
                  pos
                )
            case (DataList, DataConstr) =>
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvIntConstant(0, pos),
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.DataConstr,
                  pos
                )
            case (DataList, UplcConstr) =>
                ???
            case (DataList, ScottEncoding) =>
                ???
            case (DataList, outRep @ ProductCaseClassRepresentation.OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (PackedDataList, DataList) =>
                lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.DataList,
                  pos
                )
            case (PackedDataList, PackedDataList) =>
                input
            case (PackedDataList, UplcConstr) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (PackedDataList, ScottEncoding) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(ScottEncoding, pos)
            case (
                  PackedDataList,
                  outputRep @ ProductCaseClassRepresentation.OneElementWrapper(_)
                ) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(outputRep, pos)
            case (DataConstr, DataList) =>
                val pairIntDataList = lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.Pair(SIRType.Integer, SIRType.Data),
                  ProductCaseClassRepresentation.DataList,
                  pos
                )
                lvBuiltinApply(
                  SIRBuiltins.sndPair,
                  pairIntDataList,
                  input.sirType,
                  ProductCaseClassRepresentation.DataList,
                  pos
                )
            case (DataConstr, PackedDataList) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (DataConstr, DataConstr) =>
                input
            case (DataConstr, UplcConstr) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (UplcConstr, DataList) => ???
            case (UplcConstr, PackedDataList) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (UplcConstr, DataConstr) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(DataConstr, pos)
            case (UplcConstr, UplcConstr) =>
                input
            case (ScottEncoding, DataList) =>
                ???
            case (ScottEncoding, PackedDataList) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (ScottEncoding, DataConstr) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(DataConstr, pos)
            case (ScottEncoding, UplcConstr) =>
                ???
            case (ScottEncoding, ScottEncoding) =>
                input
            case (ProductCaseClassRepresentation.OneElementWrapper(_), DataList) =>
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
                  DataList,
                  pos
                )
            case (ProductCaseClassRepresentation.OneElementWrapper(_), PackedDataList) =>
                input
                    .toRepresentation(DataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (ProductCaseClassRepresentation.OneElementWrapper(_), DataConstr) =>
                input
                    .toRepresentation(DataList, pos)
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
        val dataListScrutinee = loweredScrutinee.toRepresentation(DataList, sel.anns.pos)
        // val prevScope = lctx.scope
        val list0: IdentifiableLoweredValue =
            if dataListScrutinee.isInstanceOf[IdentifiableLoweredValue] then
                dataListScrutinee.asInstanceOf[IdentifiableLoweredValue]
            else
                lvNewLazyIdVar(
                  lctx.uniqueVarName("list_sel"),
                  dataListScrutinee.sirType,
                  DataList,
                  dataListScrutinee,
                  sel.anns.pos
                )
        val list0id = list0.id
        val (constrDecl, typeArgs) = retrieveConstrDecl(loweredScrutinee.sirType, sel.anns.pos)
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
                      DataList,
                      lvBuiltinApply(
                        SIRBuiltins.tailList,
                        acc,
                        SIRType.List(SIRType.Data),
                        DataList,
                        sel.anns.pos
                      ),
                      sel.anns.pos
                    )
            tailLazyVar
        }
        lvBuiltinApply(SIRBuiltins.headList, selHeadList, sel.tp, DataList, sel.anns.pos)
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        genMatchDataList(matchData, loweredScrutinee)
    }

    def genMatchDataList(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val (constrDecl, typeArgs) =
            retrieveConstrDecl(loweredScrutinee.sirType, matchData.anns.pos)
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
                  ProductCaseClassRepresentation.DataList,
                  loweredScrutinee.pos
                ) match
                    case idv: IdentifiableLoweredValue => idv
                    case other =>
                        lvNewLazyIdVar(
                          lctx.uniqueVarName("_match_data_list"),
                          SIRType.List(SIRType.Data),
                          ProductCaseClassRepresentation.DataList,
                          other,
                          matchData.anns.pos
                        )
                SumCaseSirTypeGenerator.genMatchDataConstrCase(matchCase, dataList)
            case _ =>
                throw LoweringException(
                  s"Product case class match should have only one case, but ${matchData.cases.length} found",
                  matchData.anns.pos
                )
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
          DataList,
          constr.anns.pos
        )
        val dataList = dataRepresentations.foldRight(s0) { (arg, acc) =>
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              arg,
              acc,
              SIRType.List(SIRType.Data),
              DataList,
              constr.anns.pos
            )
        }
        val retval = new ProxyLoweredValue(dataList) {
            override def sirType: SIRType = constr.tp

            override def representation: LoweredValueRepresentation = DataList

            override def termInternal(gctx: TermGenerationContext): Term = {
                dataList.termInternal(gctx)
            }
        }
        retval
    }

    def genConstrUplcConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
    }

    def retrieveConstrDecl(tp: SIRType, pos: SIRPosition): (ConstrDecl, List[SIRType]) = {
        tp match
            case SIRType.CaseClass(constrDecl, typeArgs, parent) =>
                (constrDecl, typeArgs)
            case SIRType.TypeLambda(params, body) =>
                retrieveConstrDecl(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveConstrDecl(ref, pos)
            case _ =>
                throw LoweringException(s"Expected ConstrDecl type, got $tp", pos)
    }

}
