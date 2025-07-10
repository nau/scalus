package scalus.sir.lowering.typegens

import org.typelevel.paiges.Doc

import scalus.sir.{SIRType, *}
import scalus.sir.lowering.*
import scalus.sir.lowering.Lowering.tpf
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.{DefaultFun, Term}

/** Internal representation - Plutus List, element type should be data-compatibe List[E] when E is
  * data-compatible type is mapped to this type.
  */
object SumDataListSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.SumDataList

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedSumDataList

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
                      SumCaseClassRepresentation.SumDataList,
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
        input.representation match {
            case SumCaseClassRepresentation.SumDataList |
                SumCaseClassRepresentation.PackedSumDataList =>
                // no changes
                TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
            case _ =>
                // we need to upcast to PackedSumDataList
                upcastOne(
                  input.toRepresentation(
                    SumCaseClassRepresentation.SumDataList,
                    pos
                  ),
                  targetType,
                  pos
                )
        }
    }

    def uplcToData(input: Term): Term = {
        DefaultFun.ListData.tpf $ input
    }

    def dataToUplc(input: Term): Term =
        DefaultFun.UnListData.tpf $ input

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        constr.name match
            case "scalus.prelude.List$.Nil" =>
                lvBuiltinApply0(
                  SIRBuiltins.mkNilData,
                  SIRType.List.Nil,
                  SumCaseClassRepresentation.SumDataList,
                  constr.anns.pos
                )
            case "scalus.prelude.List$.Cons" =>
                if constr.args.size != 2 then
                    throw LoweringException(
                      s"Constr construnctor with ${constr.args.size} args, should be 2",
                      constr.anns.pos
                    )
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                val elementType = head.sirType
                val headDataRepr = head.toRepresentation(
                  lctx.typeGenerator(elementType).defaultDataRepresentation(elementType),
                  head.pos
                )
                val tailDataRepr =
                    tail.toRepresentation(SumCaseClassRepresentation.SumDataList, tail.pos)
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  headDataRepr,
                  tailDataRepr,
                  SIRType.List.Cons(elementType),
                  SumCaseClassRepresentation.SumDataList,
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

        override def docDef(style: PrettyPrinter.Style): Doc = {
            Doc.text("ListMatch") + PrettyPrinter.inBraces(
              listInput.docRef(style) + Doc.space + Doc.text("match") +
                  (
                    (
                      Doc.text("Cons") + PrettyPrinter.inParens(
                        consHead.docRef(style) + Doc.text(",") + consTail.docRef(style)
                      ) + Doc.text(" =>") + consBody.docRef(style).nested(2)
                    ).grouped +
                        (Doc.text("Nil") + Doc
                            .text(" =>") + nilBody.docRef(style).nested(2)).grouped
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
          representation = SumCaseClassRepresentation.SumDataList,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              SumCaseClassRepresentation.SumDataList,
              matchData.anns.pos
            )
          )
        )

        val elementType = retrieveElementType(matchData.scrutinee.tp, matchData.anns.pos)

        val consHeadRepresentation =
            lctx.typeGenerator(elementType).defaultDataRepresentation(elementType)

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
          representation = SumCaseClassRepresentation.SumDataList,
          optRhs = Some(
            lvBuiltinApply(
              SIRBuiltins.tailList,
              listInput,
              listType,
              SumCaseClassRepresentation.SumDataList,
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
            val nilBodyR =
                loweredNilBody
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
