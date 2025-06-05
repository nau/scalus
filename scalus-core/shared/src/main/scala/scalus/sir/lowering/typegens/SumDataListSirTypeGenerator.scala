package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.Lowering.tpf
import scalus.uplc.{DefaultFun, Term}

/** Internal representation - Plutus List, element type should be data-compatibe List[E] when E is
  * data-compatible type is mapped to this type.
  */
object SumDataListGenerator extends SIRTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.DataList

    override def defaultDataRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.PackedDataList

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        new RepresentationProxyLoweredValue(
          input,
          outputRepresentation,
          pos
        ) {
            override def termInternal(gctx: TermGenerationContext): Term = {
                genTranslateTermRepresentation(
                  input.termWithNeddedVars(gctx),
                  input.representation,
                  outputRepresentation,
                  this.pos
                )(using gctx)
            }
        }
    }

    def genTranslateTermRepresentation(
        input: Term,
        inputRepresentation: LoweredValueRepresentation,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using gctx: TermGenerationContext): Term = {
        (inputRepresentation, outputRepresentation) match
            case (SumCaseClassRepresentation.DataList, SumCaseClassRepresentation.DataList) =>
                input
            case (SumCaseClassRepresentation.DataList, SumCaseClassRepresentation.PackedDataList) =>
                uplcToData(input)
            case (SumCaseClassRepresentation.PackedDataList, SumCaseClassRepresentation.DataList) =>
                dataToUplc(input)
            case (
                  SumCaseClassRepresentation.PackedDataList,
                  SumCaseClassRepresentation.PackedDataList
                ) =>
                input
            case _ =>
                throw LoweringException(
                  s"Unsupported conversion from $inputRepresentation to $outputRepresentation",
                  pos
                )
    }

    def uplcToData(input: Term): Term = {
        DefaultFun.ListData.tpf $ input
    }

    def dataToUplc(input: Term): Term =
        DefaultFun.UnListData.tpf $ input

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        constr.name match
            case "scalus.prelude.List$.Nil" =>
                StaticLoweredValue(
                  constr,
                  DefaultFun.MkNilData.tpf,
                  SumCaseClassRepresentation.DataList
                )
            case "scalus.prelude.List$.Cons" =>
                if constr.args.size != 2 then
                    throw LoweringException("Non-empy Nil constructor", constr.anns.pos)
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                val elementType = head.sirType
                val headDataRepr = head.toRepresentation(
                  lctx.typeGenerator(elementType).defaultDataRepresentation,
                  head.pos
                )
                val tailDataRepr =
                    tail.toRepresentation(SumCaseClassRepresentation.DataList, tail.pos)
                new LoweredValue {
                    override def sirType: SIRType = constr.tp

                    override def pos: SIRPosition = constr.anns.pos

                    override def termInternal(gctx: TermGenerationContext): Term = {
                        val headTerm = headDataRepr.termWithNeddedVars(gctx)
                        val tailTerm = tailDataRepr.termWithNeddedVars(gctx)
                        DefaultFun.MkCons.tpf $ headTerm $ tailTerm
                    }

                    override def representation: LoweredValueRepresentation =
                        SumCaseClassRepresentation.DataList

                    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
                        headDataRepr.dominatingUplevelVars ++ tailDataRepr.dominatingUplevelVars

                    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
                        headDataRepr.usedUplevelVars ++ tailDataRepr.usedUplevelVars

                    override def addDependent(value: IdentifiableLoweredValue): Unit = {
                        headDataRepr.addDependent(value)
                        tailDataRepr.addDependent(value)
                    }
                }
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
          SumCaseClassRepresentation.DataList,
          sel.anns.pos
        )
        sel.field match {
            case "head" =>
                new ProxyLoweredValue(scrutineeDataRepr) {
                    override def sirType: SIRType = sel.tp

                    override def pos: SIRPosition = sel.anns.pos

                    override def termInternal(gctx: TermGenerationContext): Term = {
                        val listTerm = scrutineeDataRepr.termWithNeddedVars(gctx)
                        DefaultFun.HeadList.tpf $ listTerm
                    }

                    override def representation: LoweredValueRepresentation = {
                        lctx.typeGenerator(sel.tp).defaultDataRepresentation
                    }

                }
            case "tail" =>
                new ProxyLoweredValue(scrutineeDataRepr) {
                    override def sirType: SIRType = sel.tp

                    override def pos: SIRPosition = sel.anns.pos

                    override def termInternal(gctx: TermGenerationContext): Term = {
                        val listTerm = scrutineeDataRepr.termWithNeddedVars(gctx)
                        DefaultFun.TailList.tpf $ listTerm
                    }

                    override def representation: LoweredValueRepresentation =
                        SumCaseClassRepresentation.DataList

                }
            case "isNull" =>
                // isNull is not a field, but a method, that returns true if list is empty
                new ProxyLoweredValue(scrutineeDataRepr) {
                    override def sirType: SIRType = SIRType.Boolean

                    override def pos: SIRPosition = sel.anns.pos

                    override def termInternal(gctx: TermGenerationContext): Term = {
                        val listTerm = scrutineeDataRepr.termWithNeddedVars(gctx)
                        DefaultFun.NullList.tpf $ listTerm
                    }

                    override def representation: LoweredValueRepresentation =
                        PrimitiveRepresentation.Constant

                }
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
            case SIR.Pattern.Wildcard =>
                ("_head", "_tail")

        val listInputId = lctx.uniqueVarName("listInput")
        val listInput = new VariableLoweredValue(
          id = listInputId,
          name = listInputId,
          sir = SIR.Var(
            listInputId,
            matchData.scrutinee.tp,
            matchData.anns
          ),
          representation = SumCaseClassRepresentation.DataList,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              SumCaseClassRepresentation.DataList,
              matchData.anns.pos
            )
          )
        )

        val elementType = retrieveElementType(matchData.scrutinee.tp, matchData.anns.pos)

        val consHead = new VariableLoweredValue(
          id = lctx.uniqueVarName("consHead"),
          name = consHeadName,
          sir = SIR.Var(
            consHeadName,
            elementType,
            matchData.anns
          ),
          representation = lctx.typeGenerator(elementType).defaultDataRepresentation,
          optRhs = Some(
            new ProxyLoweredValue(listInput) {
                override def sirType: SIRType = elementType

                override def pos: SIRPosition = matchData.anns.pos

                override def termInternal(gctx: TermGenerationContext): Term =
                    DefaultFun.HeadList.tpf $ listInput.termWithNeddedVars(gctx)

                override def representation: LoweredValueRepresentation =
                    lctx.typeGenerator(elementType).defaultDataRepresentation

            }
          )
        )

        val consTail = new VariableLoweredValue(
          id = lctx.uniqueVarName("consTail"),
          name = consTailName,
          sir = SIR.Var(
            consTailName,
            elementType,
            matchData.anns
          ),
          representation = SumCaseClassRepresentation.DataList,
          optRhs = Some(
            new ProxyLoweredValue(listInput) {
                override def sirType: SIRType = elementType

                override def pos: SIRPosition = matchData.anns.pos

                override def termInternal(gctx: TermGenerationContext): Term =
                    DefaultFun.TailList.tpf $ listInput.termWithNeddedVars(gctx)

                override def representation: LoweredValueRepresentation =
                    SumCaseClassRepresentation.DataList

            }
          )
        )

        val prevScope = lctx.scope
        lctx.scope = lctx.scope.addAll(
          List(listInput, consHead, consTail)
        )
        val loweredConsBody = lctx.lower(consCase.get.body)
        val bodyRepresentation = loweredConsBody.representation
        lctx.scope = prevScope
        val loweredNilBody =
            lctx.lower(nilCase.get.body).toRepresentation(bodyRepresentation, nilCase.get.anns.pos)

        val usedVarsCount = Lowering.filterAndCountVars(
          x => x.id != listInput.id && x.id != consHead.id && x.id != consTail.id,
          loweredScrutinee,
          loweredConsBody,
          loweredNilBody
        )

        val cDominatedVars = usedVarsCount.filter { case (v, c) =>
            v.directDepended.size > 1 && c > 1
        }.keySet
        val cUsedVars = usedVarsCount.keySet

        val retval = new LoweredValue {
            override def sirType: SIRType = matchData.tp

            override def pos: SIRPosition = matchData.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term = {
                val nGctx = gctx.copy(generatedVars = gctx.generatedVars + listInput.id)
                Term.LamAbs(
                  listInput.id,
                  DefaultFun.ChooseList.tpf $ listInput.termWithNeddedVars(nGctx)
                      $ Term.Delay(loweredNilBody.termWithNeddedVars(nGctx))
                      $ Term.Delay(loweredConsBody.termWithNeddedVars(nGctx))
                )
            }

            override def representation: LoweredValueRepresentation =
                bodyRepresentation

            override def dominatingUplevelVars: Set[IdentifiableLoweredValue] = cDominatedVars

            override def usedUplevelVars: Set[IdentifiableLoweredValue] = cUsedVars

            override def addDependent(value: IdentifiableLoweredValue): Unit = {
                loweredScrutinee.addDependent(value)
                loweredNilBody.addDependent(value)
                loweredConsBody.addDependent(value)
            }
        }

        retval
    }

}
