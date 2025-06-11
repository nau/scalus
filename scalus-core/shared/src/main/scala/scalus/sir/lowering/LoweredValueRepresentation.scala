package scalus.sir.lowering

import scalus.sir.*

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation {
    def isPackedData: Boolean
    def isDataCentric: Boolean
}

sealed trait SumCaseClassRepresentation(
    override val isPackedData: Boolean,
    override val isDataCentric: Boolean
) extends LoweredValueRepresentation

object SumCaseClassRepresentation {

    /** Representation for sum case classes that are represented as a Data with DataConstr and
      * DataUnconstr operators to work with the data. the index of the constructor and x is a field.
      */
    case object DataConstr extends SumCaseClassRepresentation(true, true)

    /** Representation for sum case classes that are represented as a Pair of Int and DataList.
      */
    case object PairIntDataList extends SumCaseClassRepresentation(false, true)

    /** Representation for sum case classes that are represented as a list of data elements. unlike
      * `DataConstr`, this representation does not use a constructor tag, but use unList and
      * unListData to work with the data.
      */
    case object SumDataList extends SumCaseClassRepresentation(false, true)

    /** packed in data representation as a list of data elements. i.e. unListData for unpacking into
      * DataList
      */
    case object PackedSumDataList extends SumCaseClassRepresentation(true, true)

    /** Representation as tern Constr(i,x1,...,xn) where i is the index of the constructor and x is
      * a field
      */
    case object UplcConstr extends SumCaseClassRepresentation(false, false)

    /** Representation as Constr(i,x1,...,xn) where i is the index of the constructor and x is a
      * field represented as data.
      */
    case object UplcConstrOnData extends SumCaseClassRepresentation(false, true)

    case object ScottEncoding extends SumCaseClassRepresentation(false, false)
}

sealed trait ProductCaseClassRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation

object ProductCaseClassRepresentation {
    case object PackedDataList extends ProductCaseClassRepresentation(true, true)

    case object ProdDataList extends ProductCaseClassRepresentation(false, true)

    /** Data.Unconstr will give us a pair from data and index of the constructor.
      */
    case object DataConstr extends ProductCaseClassRepresentation(true, true)

    case object UplcConstr extends ProductCaseClassRepresentation(false, false)

    case object ScottEncoding extends ProductCaseClassRepresentation(false, false)

    case class OneElementWrapper(representation: LoweredValueRepresentation)
        extends ProductCaseClassRepresentation(
          representation.isPackedData,
          representation.isDataCentric
        )

    // TODO: implement
    // case class PairWrapper(
    //    first: LoweredValueRepresentation,
    //    second: LoweredValueRepresentation
    // ) extends ProductCaseClassRepresentation

}

case class LambdaRepresentation(
    input: LoweredValueRepresentation,
    output: LoweredValueRepresentation
) extends LoweredValueRepresentation {
    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false
}

sealed trait PrimitiveRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation

object PrimitiveRepresentation {
    case object PackedData extends PrimitiveRepresentation(true, true)

    case object Constant extends PrimitiveRepresentation(false, false)
}

/** TypeVarRepresentation is used for type variables. Usually this is a synonym for some other
  * specific-type representation.
  *
  * for now, assume that TypeVars can't be only serializable to data.
  */
case object TypeVarDataRepresentation extends LoweredValueRepresentation {
    override def isPackedData: Boolean = true

    override def isDataCentric: Boolean = true
}

case object ErrorRepresentation extends LoweredValueRepresentation {
    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false
}

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
                LambdaRepresentation(
                  constRepresentation(in),
                  constRepresentation(out)
                )
            case SIRType.TypeVar(_, _)  => TypeVarDataRepresentation
            case SIRType.FreeUnificator => TypeVarDataRepresentation
            case SIRType.TypeProxy(ref) =>
                constRepresentation(ref)
            case SIRType.TypeNothing => ErrorRepresentation
    }

}
