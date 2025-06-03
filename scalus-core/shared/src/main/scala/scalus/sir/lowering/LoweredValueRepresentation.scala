package scalus.sir.lowering

import scalus.sir.*

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation

sealed trait SumCaseClassRepresentation extends LoweredValueRepresentation

object SumCaseClassRepresentation {

    /** Representation for sum case classes that are represented as a Data with DataConstr and and
      * DatsaUnconstr operators to work with the data. the index of the constructor and x is a
      * field.
      */
    case object DataConstr extends SumCaseClassRepresentation

    /** Representation for sum case classes that are represented as a list of data elements. unlike
      * `DataConstr`, this representation does not use a constructor tag, but use unList and
      * unListData to work with the data.
      */
    case object DataList extends SumCaseClassRepresentation

    /** packed in data representation as a list of data elements. i.e. unListData for unpacking into
      * DataList
      */
    case object PackedDataList extends SumCaseClassRepresentation

    /** Representation as tern Constr(i,x1,...,xn) where i is the index of the constructor and x is
      * a field
      */
    case object UplcConstr extends SumCaseClassRepresentation

    /** Representation as Constr(i,x1,...,xn) where i is the index of the constructor and x is a
      * field reprented as data.
      */
    case object UplcConstrOnData extends SumCaseClassRepresentation

    case object ScottEncoding extends SumCaseClassRepresentation
}

sealed trait ProductCaseClassRepresentation extends LoweredValueRepresentation

object ProductCaseClassRepresentation {
    case object PackedDataList extends ProductCaseClassRepresentation

    case object DataList extends ProductCaseClassRepresentation

    case object PacketDataConstr extends ProductCaseClassRepresentation

    /** Data.Unconstr will give as a pair from data and index of the constructor.
      */
    case object DataConstr extends ProductCaseClassRepresentation

    case object UplcConstr extends ProductCaseClassRepresentation

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
                if lc.plutusVersion >= 3 then SumCaseClassRepresentation.DataConstr
                else SumCaseClassRepresentation.ScottEncoding
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                if lc.plutusVersion >= 3 then ProductCaseClassRepresentation.DataConstr
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
