package scalus.sir.lowering

import scalus.sir.*

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation

sealed trait SumCaseClassRepresentation extends LoweredValueRepresentation

object SumCaseClassRepresentation {
    case object DataConstr extends SumCaseClassRepresentation

    case object V3ConstrEncoding extends SumCaseClassRepresentation

    case object ScottEncoding extends SumCaseClassRepresentation
}

sealed trait ProductCaseClassRepresentation extends LoweredValueRepresentation

object ProductCaseClassRepresentation {
    case object PackedData extends ProductCaseClassRepresentation

    case object DataList extends ProductCaseClassRepresentation

    case object V3ConstrEncoding extends ProductCaseClassRepresentation

    case object ScottEncoding extends ProductCaseClassRepresentation

    case class OneElelmentWrapper(representation: LoweredValueRepresentation)
}

case class LambdaRepresentaion(
    input: LoweredValueRepresentation,
    output: LoweredValueRepresentation
) extends LoweredValueRepresentation

sealed trait PrimitiveRepresentation extends LoweredValueRepresentation

object PrimitiveRepresentation {
    case object PackedData extends PrimitiveRepresentation

    case object Constant extends PrimitiveRepresentation
}

/** TypeVarRepresentation is used for type variables. Usually this is a synonym for some other
  * specific-type representation.
  */
case object TypeVarDataRepresentation extends LoweredValueRepresentation

object LoweredValueRepresentation {

    def constRepresentation(tp: SIRType)(using lc: LoweringContext): LoweredValueRepresentation = {
        tp match
            case SIRType.SumCaseClass(decl, typeArgs) =>
                if lc.plutusVersion >= 3 then SumCaseClassRepresentation.V3ConstrEncoding
                else SumCaseClassRepresentation.ScottEncoding
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                if lc.plutusVersion >= 3 then ProductCaseClassRepresentation.V3ConstrEncoding
                else ProductCaseClassRepresentation.ScottEncoding
            case SIRType.TypeLambda(params, body) =>
                constRepresentation(body)
            case SIRType.Integer | SIRType.Data | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit =>
                PrimitiveRepresentation.Constant
            case SIRType.Fun(in, out) =>
                LambdaRepresentaion(
                  constRepresentation(in),
                  constRepresentation(out)
                )
            case SIRType.TypeVar(_, _)  => TypeVarDataRepresentation
            case SIRType.FreeUnificator => TypeVarDataRepresentation
            case SIRType.TypeProxy(ref) =>
                constRepresentation(ref)
    }

    
    
}
