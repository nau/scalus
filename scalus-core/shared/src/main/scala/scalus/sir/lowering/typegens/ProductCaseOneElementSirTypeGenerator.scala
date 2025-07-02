package scalus.sir.lowering.typegens

import scalus.sir.SIRType.Data
import scalus.sir.{SIRType, *}
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.sir.lowering.ProductCaseClassRepresentation.{ProdDataConstr, ProdDataList}
import scalus.uplc.Term

case class ProductCaseOneElementSirTypeGenerator(
    argGenerator: SirTypeUplcGenerator,
) extends SirTypeUplcGenerator {

    override def defaultRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultRepresentation(tp)
        )

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultDataRepresentation(tp)
        )

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultTypeVarReperesentation(tp)
        )

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean =
        argGenerator.isDataSupported(tp)

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match
            case (
                  ProductCaseClassRepresentation.OneElementWrapper(argRepr),
                  ProductCaseClassRepresentation.OneElementWrapper(newArgRepr)
                ) =>
                if argRepr == newArgRepr then input
                else
                    val newArg = argLoweredValue(input).toRepresentation(newArgRepr, pos)
                    new ProxyLoweredValue(newArg) {
                        override def sirType: SIRType = input.sirType
                        override def pos: SIRPosition = input.pos
                        override def representation: LoweredValueRepresentation =
                            ProductCaseClassRepresentation.OneElementWrapper(newArgRepr)
                        override def termInternal(gctx: TermGenerationContext): Term =
                            newArg.termInternal(gctx)
                    }
            case (ProductCaseClassRepresentation.OneElementWrapper(argRepr), ProdDataList) =>
                val argInData = argLoweredValue(input).toRepresentation(
                  argGenerator.defaultDataRepresentation(input.sirType),
                  pos
                )
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  argInData,
                  lvBuiltinApply0(
                    SIRBuiltins.mkNilData,
                    SIRType.List(Data),
                    SumCaseClassRepresentation.SumDataList,
                    pos
                  ),
                  input.sirType,
                  ProdDataList,
                  pos
                )
            case (ProductCaseClassRepresentation.OneElementWrapper(argRepr), ProdDataConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(ProdDataConstr, pos)
            case (
                  ProductCaseClassRepresentation.OneElementWrapper(argRepr),
                  outRepr @ TypeVarRepresentation(isBuiltin)
                ) =>
                if isBuiltin then input
                else
                    import ProductCaseOneElementSirTypeGenerator.*
                    if argRepr.isCompatible(representation) then
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    else
                        val argValue = argLoweredValue(input)
                        val newArg = argGenerator.toRepresentation(argValue, representation, pos)
                        val inPos = pos
                        new ProxyLoweredValue(newArg) {
                            override def sirType: SIRType = input.sirType
                            override def representation: LoweredValueRepresentation = outRepr
                            override def pos: SIRPosition = inPos
                            override def termInternal(gctx: TermGenerationContext): Term =
                                newArg.termInternal(gctx)
                        }
            case (
                  inRepr @ TypeVarRepresentation(isBuiltin),
                  outRepr @ ProductCaseClassRepresentation.OneElementWrapper(argRepr)
                ) =>
                if isBuiltin then new RepresentationProxyLoweredValue(input, representation, pos)
                else
                    import ProductCaseOneElementSirTypeGenerator.*
                    val argValue = argLoweredValue(input)
                    if argRepr.isCompatible(argValue.representation) then
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    else
                        // we need to convert the argument to the new representation
                        val newArg = argGenerator.toRepresentation(argValue, argRepr, pos)
                        val inPos = pos
                        val retval = new ProxyLoweredValue(newArg) {
                            override def sirType = input.sirType
                            override def representation: LoweredValueRepresentation = outRepr
                            override def pos: SIRPosition = inPos
                            override def termInternal(gctx: TermGenerationContext): Term =
                                newArg.termInternal(gctx)
                        }
                        retval
            case (
                  inRepr @ ProductCaseClassRepresentation.OneElementWrapper(argRepr),
                  outRepr @ TypeVarRepresentation(isBuiltin)
                ) =>
                if isBuiltin then input
                else
                    import ProductCaseOneElementSirTypeGenerator.*
                    val argValue = argLoweredValue(input)
                    if argRepr.isCompatible(outRepr) then
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    else
                        // we need to convert the argument to the new representation
                        val newArg = argGenerator.toRepresentation(argValue, outRepr, pos)
                        val inPos = pos
                        new ProxyLoweredValue(newArg) {
                            override def sirType: SIRType = input.sirType
                            override def representation: LoweredValueRepresentation = outRepr
                            override def pos: SIRPosition = inPos
                            override def termInternal(gctx: TermGenerationContext): Term =
                                newArg.termInternal(gctx)
                        }
            case (_, _) =>
                ProductCaseSirTypeGenerator.toRepresentation(input, representation, pos)

    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {

        val dataDecl = SIRType
            .retrieveDataDecl(targetType)
            .fold(
              msg =>
                  throw LoweringException(
                    s"Can't retrieve decl from ${targetType.show}: $msg",
                    pos
                  ),
              identity
            )
        val constrDecl = SIRType
            .retrieveConstrDecl(input.sirType)
            .fold(
              msg =>
                  throw LoweringException(
                    s"Can't retrieve constr decl from ${input.sirType.show}: $msg",
                    pos
                  ),
              identity
            )

        val constrIndex = dataDecl.constructors.indexWhere(_.name == constrDecl.name)
        if constrIndex < 0 then {
            throw LoweringException(
              s"Expected case class ${dataDecl.name} with constr ${constrDecl.name}, but it is not found in data declaration",
              pos
            )
        }

        val constrIndexConstant = lvIntConstant(3, pos)
        val argValueIn = argLoweredValue(input)
        val argValue = argValueIn.toRepresentation(
          argGenerator.defaultDataRepresentation(argValueIn.sirType),
          pos
        )
        val prodArgs = lvBuiltinApply2(
          SIRBuiltins.mkCons,
          argValue,
          lvBuiltinApply0(
            SIRBuiltins.mkNilData,
            SIRType.List(Data),
            SumCaseClassRepresentation.SumDataList,
            pos
          ),
          SIRType.List(Data),
          SumCaseClassRepresentation.SumDataList,
          pos
        )

        lvBuiltinApply2(
          SIRBuiltins.constrData,
          constrIndexConstant,
          prodArgs,
          targetType,
          SumCaseClassRepresentation.DataConstr,
          pos
        )

    }

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        if constr.args.size != 1 then
            throw LoweringException(
              s"Expected one argument for product case class, got ${constr.args.size}",
              constr.anns.pos
            )
        val loweredArg = lctx.lower(constr.args.head)
        WrappedArg(constr, loweredArg)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        val sirCaseClass = retrieveCaseClassSirType(loweredScrutinee.sirType, loweredScrutinee.pos)
        val name = sirCaseClass.constrDecl.params.head.name
        if sel.field != name then
            throw LoweringException(
              s"Expected select on ${name}, got ${sel.field}",
              sel.anns.pos
            )
        else argLoweredValue(loweredScrutinee)
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        val sirCaseClass = retrieveCaseClassSirType(loweredScrutinee.sirType, loweredScrutinee.pos)
        val name = sirCaseClass.constrDecl.params.head.name
        matchData.cases match {
            case Nil =>
                throw LoweringException("Empty match cases", matchData.anns.pos)
            case SIR.Case(pattern, body, anns) :: Nil =>
                pattern match
                    case SIR.Pattern.Constr(constr, bindings, typeBindings) =>
                        if constr.name == sirCaseClass.constrDecl.name then
                            val prevScope = lctx.scope
                            val arg = argLoweredValue(loweredScrutinee)
                            lvNewLazyNamedVar(
                              bindings.head,
                              typeBindings.head,
                              arg.representation,
                              arg,
                              anns.pos
                            )
                            val loweredBody = lctx.lower(body)
                            lctx.scope = prevScope
                            loweredBody
                        else
                            throw LoweringException(
                              s"Expected case class ${sirCaseClass.constrDecl.name}, got ${constr.name}",
                              anns.pos
                            )
                    case SIR.Pattern.Wildcard =>
                        lctx.lower(body)
                    case _ =>
                        throw LoweringException(
                          s"Expected single case with select on ${name}, got ${pattern}",
                          anns.pos
                        )

            case _ =>
                throw LoweringException(
                  s"Expected single case with select on ${name}, got ${matchData.cases}",
                  matchData.anns.pos
                )
        }
    }

    private def argLoweredValue(input: LoweredValue)(using LoweringContext): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        input match {
            case WrappedArg(constr, arg) => arg
            case other =>
                val argType = retrieveArgType(other.sirType, other.pos)
                ArgProxyLoweredValue(
                  other,
                  argType,
                  input.representation match {
                      case ProductCaseClassRepresentation.OneElementWrapper(argRepr) =>
                          argRepr
                      case TypeVarRepresentation(isBuiltin) =>
                          if isBuiltin then argGenerator.defaultRepresentation(argType)
                          else argGenerator.defaultTypeVarReperesentation(argType)
                      case _ =>
                          throw LoweringException(
                            s"Expected OneElementWrapper representation, got ${input.representation}",
                            other.pos
                          )
                  },
                  other.pos
                )
        }
    }

}

object ProductCaseOneElementSirTypeGenerator {

    case class WrappedArg(constr: SIR.Constr, arg: LoweredValue) extends ProxyLoweredValue(arg) {

        override def sirType: SIRType = constr.tp

        override def pos: SIRPosition = constr.anns.pos

        override def representation: LoweredValueRepresentation =
            ProductCaseClassRepresentation.OneElementWrapper(arg.representation)

        override def termInternal(gctx: TermGenerationContext): Term =
            arg.termInternal(gctx)

    }

    case class ArgProxyLoweredValue(
        wrapper: LoweredValue,
        tp: SIRType,
        repr: LoweredValueRepresentation,
        inPos: SIRPosition,
    ) extends ProxyLoweredValue(wrapper) {

        override def sirType: SIRType = tp

        override def pos: SIRPosition = inPos

        override def representation: LoweredValueRepresentation = repr

        override def termInternal(gctx: TermGenerationContext): Term =
            wrapper.termInternal(gctx)

    }

    private def retrieveCaseClassSirType(
        rType: SIRType,
        pos: SIRPosition
    ): SIRType.CaseClass = {
        rType match
            case r @ SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if r.constrDecl.params.size != 1 then
                    throw LoweringException(
                      s"Expected one parameter for product case class, got ${r.constrDecl.params.size}",
                      pos
                    )
                r
            case SIRType.TypeProxy(ref) =>
                retrieveCaseClassSirType(ref, pos)
            case SIRType.TypeLambda(params, body) =>
                retrieveCaseClassSirType(body, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${rType.show}",
                  pos
                )
    }

    def retrieveArgType(rType: SIRType, pos: SIRPosition): SIRType = {
        rType match {
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                val param = constrDecl.params.head
                SIRType.substitute(param.tp, constrDecl.typeParams.zip(typeArgs).toMap, Map.empty)
            case SIRType.TypeProxy(ref) =>
                retrieveArgType(ref, pos)
            case SIRType.TypeLambda(params, body) =>
                retrieveArgType(body, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${rType.show}",
                  pos
                )
        }
    }

}
