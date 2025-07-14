package scalus.sir.lowering.typegens

import org.typelevel.paiges.Doc
import scalus.sir.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.sir.lowering.*
import scalus.sir.lowering.Lowering.tpf
import scalus.uplc.{DefaultFun, Term}

trait SumListCommonSirTypeGenerator extends SirTypeUplcGenerator {

    def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation

    def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    /** mkNilData and mkNilPairData
      * @param pos
      * @return
      */
    def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.PackedSumDataList,
                  pos
                )
            case (SumCaseClassRepresentation.SumDataList, SumCaseClassRepresentation.SumDataList) =>
                input
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                val elementType = retrieveElementType(
                  input.sirType,
                  pos
                )
                val (typeParams, constrDecl, typeArgs) = SIRType
                    .collectProd(elementType)
                    .getOrElse(
                      throw new LoweringException(
                        s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                        pos
                      )
                    )
                val fun =
                    if constrDecl.name == "scalus.builtin.Pair" then
                        ScalusRuntime.dataListToPairsList
                    else if constrDecl.name == "scala.Tuple2" then
                        ScalusRuntime.dataListToTuplesList
                    else
                        throw new LoweringException(
                          s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                          pos
                        )
                lvApply(fun, input, pos, None, None)
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataList
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataList,
                  pos
                )
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.mapData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataAssocMap,
                  pos
                )
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataList
                ) =>
                //   when it potenitallu can be used -- when one part of the program know, that element
                //     is a list of pairs, but another part - does not know.
                //     (pass to foldLeft, foldLeft expect List as dat)
                // we should change stdlib, do not run such conversions
                val elementType = retrieveElementType(
                  input.sirType,
                  pos
                )
                val (typeParams, constrDecl, typeArgs) = SIRType
                    .collectProd(elementType)
                    .getOrElse(
                      throw new LoweringException(
                        s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                        pos
                      )
                    )
                val fun =
                    if constrDecl.name == "scalus.builtin.Pair" then
                        ScalusRuntime.pairsListToDataList
                    else if constrDecl.name == "scala.Tuple2" then
                        ScalusRuntime.tuplesListToDataList
                    else
                        throw new LoweringException(
                          s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                          pos
                        )
                // we know that function will have appropriate type and representation by definition
                lvApply(fun, input, pos, None, None)
            case (_, tv @ TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else {
                    val inputAsData =
                        input.toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    new RepresentationProxyLoweredValue(inputAsData, tv, pos)
                }
            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else if input.representation == outputRepresentation then input
                else
                    val r0 = RepresentationProxyLoweredValue(
                      input,
                      SumCaseClassRepresentation.PackedSumDataList,
                      pos
                    )
                    r0.toRepresentation(outputRepresentation, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion  for ${input.sirType.show} from ${input.representation} to ${outputRepresentation}",
                  pos
                )
    }

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetElementType = retrieveElementType(
          targetType,
          pos
        )
        input.representation match {
            case SumCaseClassRepresentation.SumDataList |
                SumCaseClassRepresentation.PackedSumDataList =>
                // no changes
                TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
            case SumCaseClassRepresentation.SumDataPairList |
                SumCaseClassRepresentation.SumDataAssocMap =>
                if SirTypeUplcGenerator.isPairOrTuple2(targetElementType) then
                    TypeRepresentationProxyLoweredValue(
                      input,
                      targetType,
                      input.representation,
                      pos
                    )
                else
                    val alignedInput = input.toRepresentation(
                      SumCaseClassRepresentation.SumDataList,
                      pos
                    )
                    TypeRepresentationProxyLoweredValue(
                      alignedInput,
                      targetType,
                      SumCaseClassRepresentation.SumDataList,
                      pos
                    )
            case TypeVarRepresentation(isBuiltin) =>
                val targetRepresentation = {
                    if isBuiltin then defaultRepresentation(input.sirType)
                    else this.defaultTypeVarReperesentation(input.sirType)
                }
                val alignedInput = input.toRepresentation(
                  targetRepresentation,
                  pos
                )
                upcastOne(alignedInput, targetType, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation ${input.representation.show} for List upcast from ${input.sirType.show} to ${targetType.show}",
                  pos
                )
        }
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        constr.name match
            case "scalus.prelude.List$.Nil" =>
                genNil(constr.tp, constr.anns.pos)
                // lvBuiltinApply0(
                //  SIRBuiltins.mkNilData,
                //  SIRType.List.Nil,
                //  SumCaseClassRepresentation.SumDataList,
                //  constr.anns.pos
                // )
            case "scalus.prelude.List$.Cons" =>
                if constr.args.size != 2 then
                    throw LoweringException(
                      s"Constr construnctor with ${constr.args.size} args, should be 2",
                      constr.anns.pos
                    )
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                val elementType = head.sirType
                val headElementRepr = head.toRepresentation(
                  defaultElementRepresentation(elementType, constr.anns.pos),
                  constr.anns.pos
                )
                val tailElementRepr =
                    tail.toRepresentation(defaultListRepresentation, constr.anns.pos)
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  headElementRepr,
                  tailElementRepr,
                  SIRType.List.Cons(elementType),
                  defaultListRepresentation,
                  constr.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown constructor ${constr.name} for List",
                  constr.anns.pos
                )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val scrutineeDataRepr = loweredScrutinee.toRepresentation(
          SumCaseClassRepresentation.SumDataList,
          sel.anns.pos
        )
        sel.field match {
            case "head" =>
                lvBuiltinApply(
                  SIRBuiltins.headList,
                  scrutineeDataRepr,
                  sel.tp,
                  lctx.typeGenerator(sel.tp).defaultDataRepresentation(sel.tp),
                  sel.anns.pos
                )
            case "tail" =>
                lvBuiltinApply(
                  SIRBuiltins.tailList,
                  scrutineeDataRepr,
                  sel.tp,
                  SumCaseClassRepresentation.SumDataList,
                  sel.anns.pos
                )
            case "isNull" =>
                // isNull is not a field, but a method, that returns true if list is empty
                lvBuiltinApply(
                  SIRBuiltins.nullList,
                  scrutineeDataRepr,
                  SIRType.Boolean,
                  PrimitiveRepresentation.Constant,
                  sel.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown field ${sel.field} for List, which have 'head' and 'tail' fields and isNull method",
                  sel.anns.pos
                )
        }

    }

    def retrieveElementType(tp: SIRType, pos: SIRPosition)(using lctx: LoweringContext): SIRType = {
        tp match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                typeArgs.head
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if constrDecl.name == "scalus.prelude.List$.Nil" then SIRType.FreeUnificator
                else if constrDecl.name == "scalus.prelude.List$.Cons" then typeArgs.head
                else
                    throw LoweringException(
                      s"Unknown case class ${constrDecl.name} for List",
                      pos
                    )
            case SIRType.TypeLambda(params, body) =>
                retrieveElementType(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveElementType(ref, pos)
            case _ =>
                throw LoweringException(
                  s"Cannot retrieve element type from ${tp.show}, expected List type",
                  pos
                )
        }
    }

    case class ListMatchLoweredValue(
        listInput: LoweredValue,
        consHead: IdentifiableLoweredValue,
        consTail: IdentifiableLoweredValue,
        consBody: LoweredValue,
        nilBody: LoweredValue,
        sirType: SIRType,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    ) extends ComplexLoweredValue(Set(consHead, consTail), listInput, consBody, nilBody) {

        override def termInternal(gctx: TermGenerationContext): Term = {
            !(DefaultFun.ChooseList.tpf $ listInput.termWithNeededVars(gctx)
                $ ~nilBody.termWithNeededVars(gctx)
                $ ~consBody.termWithNeededVars(gctx))
        }

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
            Doc.text("ListMatch") + PrettyPrinter.inBraces(
              listInput.docRef(ctx) + Doc.space + Doc.text("match") +
                  (
                    (
                      Doc.text("Cons") + PrettyPrinter.inParens(
                        consHead.docRef(ctx) + Doc.text(",") + consTail.docRef(ctx)
                      ) + Doc.text(" =>") + consBody.docRef(ctx).nested(2)
                    ).grouped +
                        (Doc.text("Nil") + Doc
                            .text(" =>") + nilBody.docRef(ctx).nested(2)).grouped
                  ).aligned
            ) + Doc.text(":") + Doc.text(sirType.show)
        }

    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Nil, Cons
        var nilCase: Option[SIR.Case] = None
        var consCase: Option[SIR.Case] = None
        var wildcardCase: Option[SIR.Case] = None
        var noBindingInConsCase = false
        matchData.cases.foreach { cs =>
            cs.pattern match
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == "scalus.prelude.List$.Nil" =>
                    nilCase = Some(cs)
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == "scalus.prelude.List$.Cons" =>
                    consCase = Some(cs)
                case SIR.Pattern.Wildcard =>
                    wildcardCase = Some(cs)
                case _ =>
                    throw LoweringException(s"Unknown pattern ${cs.pattern}", cs.anns.pos)
        }
        val isUnchecked = matchData.anns.data.contains("unchecked")
        if nilCase.isEmpty then
            if wildcardCase.nonEmpty then nilCase = wildcardCase
            else if !isUnchecked then
                throw LoweringException("No Nil case in match", matchData.anns.pos)
        if consCase.isEmpty then
            if wildcardCase.nonEmpty then
                consCase = wildcardCase
                noBindingInConsCase = true
            else if !isUnchecked then
                throw LoweringException("No Cons case in match", matchData.anns.pos)

        val (consHeadName, consTailName) = consCase.get.pattern match
            case SIR.Pattern.Constr(constrDecl, List(h, t), _) =>
                (h, t)
            case SIR.Pattern.Constr(_, _, _) =>
                throw LoweringException(
                  s"Cons case should have two bindings, but found ${consCase.get.pattern}",
                  consCase.get.anns.pos
                )
            case SIR.Pattern.Wildcard =>
                ("_head", "_tail")

        val listInputId = lctx.uniqueVarName("listInput")
        val listType = matchData.scrutinee.tp
        val listInput = new VariableLoweredValue(
          id = listInputId,
          name = listInputId,
          sir = SIR.Var(
            listInputId,
            matchData.scrutinee.tp,
            matchData.anns
          ),
          representation = defaultListRepresentation,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              defaultListRepresentation,
              matchData.anns.pos
            )
          )
        )

        val elementType = retrieveElementType(matchData.scrutinee.tp, matchData.anns.pos)

        val consHeadRepresentation = defaultElementRepresentation(elementType, matchData.anns.pos)

        val consHead = new VariableLoweredValue(
          id = lctx.uniqueVarName("consHead"),
          name = consHeadName,
          sir = SIR.Var(
            consHeadName,
            elementType,
            matchData.anns
          ),
          representation = consHeadRepresentation,
          optRhs = Some(
            lvBuiltinApply(
              SIRBuiltins.headList,
              listInput,
              elementType,
              consHeadRepresentation,
              matchData.anns.pos
            )
          )
        )

        val consTail = new VariableLoweredValue(
          id = lctx.uniqueVarName("consTail"),
          name = consTailName,
          sir = SIR.Var(
            consTailName,
            listType,
            matchData.anns
          ),
          representation = defaultListRepresentation,
          optRhs = Some(
            lvBuiltinApply(
              SIRBuiltins.tailList,
              listInput,
              listType,
              defaultListRepresentation,
              matchData.anns.pos
            )
          )
        )

        val prevScope = lctx.scope
        lctx.scope = lctx.scope.addAll(
          List(listInput, consHead, consTail)
        )

        val loweredConsBody =
            lctx.lower(consCase.get.body).maybeUpcast(matchData.tp, consCase.get.anns.pos)
        val bodyRepresentation = loweredConsBody.representation
        lctx.scope = prevScope
        val loweredNilBody =
            lctx.lower(nilCase.get.body)
                .maybeUpcast(matchData.tp, nilCase.get.anns.pos)
                .toRepresentation(bodyRepresentation, nilCase.get.anns.pos)

        if SIRType.isProd(loweredScrutinee.sirType) then
            val constrDecl = SIRType
                .retrieveConstrDecl(loweredScrutinee.sirType)
                .getOrElse(
                  throw LoweringException(
                    s"Cannot retrieve constrDecl from ${loweredScrutinee.sirType.show}",
                    matchData.anns.pos
                  )
                )
            if constrDecl.name == "scalus.prelude.List$.Nil" then {
                // we can generate only Nil case.
                println("warning: unused case Cons in List match will be removed")
                loweredNilBody
            } else if constrDecl.name == "scalus.prelude.List$.Cons" then {
                println("warning: unused case Nil in List match will be removed")
                loweredConsBody
            } else
                throw LoweringException(
                  s"Unknown list constructior ${constrDecl.name}",
                  matchData.anns.pos
                )
        else

            val resType = matchData.tp
            resType match
                case tv: SIRType.TypeVar =>
                    println(s"typevar in List match at ${matchData.anns.pos.show}")
                case _ =>

            if resType == SIRType.FreeUnificator then
                throw LoweringException(
                  s"match branches return unrelated types: ${loweredConsBody.sirType.show} and ${loweredNilBody.sirType.show}",
                  matchData.anns.pos
                )

            val consBodyR = loweredConsBody.maybeUpcast(resType, matchData.anns.pos)
            val resRepresentation = consBodyR.representation
            val nilBodyR = loweredNilBody
                .maybeUpcast(resType, loweredNilBody.pos)
                .toRepresentation(resRepresentation, loweredNilBody.pos)

            val retval = ListMatchLoweredValue(
              listInput,
              consHead,
              consTail,
              consBodyR,
              nilBodyR,
              resType,
              resRepresentation,
              matchData.anns.pos
            )

            retval
    }

}
