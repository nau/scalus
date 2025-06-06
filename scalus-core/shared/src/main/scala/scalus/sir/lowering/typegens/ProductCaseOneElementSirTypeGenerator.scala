package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.Term

case class ProductCaseOneElementSirTypeGenerator(argGenerator: SirTypeUplcGenerator)
    extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultRepresentation
        )

    override def defaultDataRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultDataRepresentation
        )

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        ProductCaseSirTypeGenerator.toRepresentation(input, representation, pos)
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
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

    private def argLoweredValue(input: LoweredValue): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        input match {
            case WrappedArg(constr, arg) => arg
            case other =>
                ArgProxyLoweredValue(
                  other,
                  retrieveArgType(other.sirType, other.pos),
                  input.representation match {
                      case ProductCaseClassRepresentation.OneElementWrapper(argRepr) =>
                          argRepr
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

        override def representation: LoweredValueRepresentation =
            ProductCaseClassRepresentation.OneElementWrapper(repr)

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
