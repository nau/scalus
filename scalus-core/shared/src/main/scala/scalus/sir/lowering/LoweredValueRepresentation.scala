package scalus.sir.lowering

import scalus.sir.*

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation {
    def isPackedData: Boolean
    def isDataCentric: Boolean
    def isCompatible(repr: LoweredValueRepresentation): Boolean =
        this == repr
}

sealed trait SumCaseClassRepresentation(
    override val isPackedData: Boolean,
    override val isDataCentric: Boolean
) extends LoweredValueRepresentation

object SumCaseClassRepresentation {

    /** Representation for sum case classes that are represented as a Data with DataConstr and
      * DataUnconstr operators to work with the data. the index of the constructor and x is a field.
      */
    case object DataConstr extends SumCaseClassRepresentation(true, true) {
        override def isCompatible(repr: LoweredValueRepresentation): Boolean =
            repr match {
                case DataConstr               => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }
    }

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

}

sealed trait ProductCaseClassRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation

object ProductCaseClassRepresentation {

    case object PackedDataList extends ProductCaseClassRepresentation(true, true)

    case object ProdDataList extends ProductCaseClassRepresentation(false, true)

    /** Data.Unconstr will give us a pair from data and index of the constructor.
      */
    case object ProdDataConstr extends ProductCaseClassRepresentation(true, true) {

        override def isCompatible(repr: LoweredValueRepresentation): Boolean =
            repr match {
                case ProdDataConstr           => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }
    }

    case object PairIntDataList extends ProductCaseClassRepresentation(false, true)

    case object UplcConstr extends ProductCaseClassRepresentation(false, false)

    case class OneElementWrapper(representation: LoweredValueRepresentation)
        extends ProductCaseClassRepresentation(
          representation.isPackedData,
          representation.isDataCentric
        ) {
        override def isCompatible(repr: LoweredValueRepresentation): Boolean =
            repr match {
                case OneElementWrapper(innerRepr) => representation.isCompatible(innerRepr)
                case other                        => representation.isCompatible(other)
            }
    }

    // TODO: implement
    // case class PairWrapper(
    //    first: LoweredValueRepresentation,
    //    second: LoweredValueRepresentation
    // ) extends ProductCaseClassRepresentation

}

/** Representation for lambda function. By default, lanbda-s accept default reperesentation for
  * input and output types. But when we pass functions to type-parametrized functions, then calling
  * party does not know about real parameter types and can't use default representation, so pass
  * parameters as packed data.
  *
  * So, we translate higher-order functions to packed data representation when pass as arguments to
  * type-parametrized functions.
  */
case class LambdaRepresentation(
    inRepr: LoweredValueRepresentation,
    outRepr: LoweredValueRepresentation
) extends LoweredValueRepresentation {

    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def isCompatible(repr: LoweredValueRepresentation): Boolean = {
        repr match {
            case LambdaRepresentation(in, out) =>
                inRepr.isCompatible(in) && outRepr.isCompatible(out)
            case TypeVarRepresentation(isBuiltin) =>
                isBuiltin || isTypeVarCompatible(inRepr) && isTypeVarCompatible(outRepr)
            case _ => false
        }
    }

    def isTypeVarCompatible(repr: LoweredValueRepresentation): Boolean =
        repr match {
            case TypeVarRepresentation(_) => true
            case LambdaRepresentation(in, out) =>
                isTypeVarCompatible(in) && isTypeVarCompatible(out)
            case _ => repr.isPackedData
        }

}

sealed trait PrimitiveRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation

object PrimitiveRepresentation {
    case object PackedData extends PrimitiveRepresentation(true, true)

    case object Constant extends PrimitiveRepresentation(false, false)
}

/** TypeVarRepresentation is used for type variables. Usually this is a synonym for some other
  * specific-type representation. When this is builtin type variable, it can be freely used in any
  * type representation, but when it is not builtin (scala type-var) it can be used only with packed
  * data representation.
  */
case class TypeVarRepresentation(isBuiltin: Boolean) extends LoweredValueRepresentation {

    // assume that TypeVarDataRepresentation is a packed data.
    //  (this is not true for lambda, will check this in code. Usually in all places we also known type)
    override def isPackedData: Boolean = !isBuiltin

    override def isDataCentric: Boolean = isPackedData
}

case object ErrorRepresentation extends LoweredValueRepresentation {
    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false
}

object LoweredValueRepresentation {

    def constRepresentation(tp: SIRType)(using lc: LoweringContext): LoweredValueRepresentation = {
        tp match
            case SIRType.SumCaseClass(decl, typeArgs) =>
                SumCaseClassRepresentation.DataConstr
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                ProductCaseClassRepresentation.ProdDataConstr
            case SIRType.TypeLambda(params, body) =>
                constRepresentation(body)
            case SIRType.Integer | SIRType.Data | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit =>
                PrimitiveRepresentation.Constant
            case SIRType.Fun(in, out) =>
                val inRepresentation = lc.typeGenerator(in).defaultRepresentation(in)
                val outRepresentation = lc.typeGenerator(out).defaultRepresentation(out)
                LambdaRepresentation(inRepresentation, outRepresentation)
            case tv @ SIRType.TypeVar(_, _, isBuiltin) =>
                // for now we don't allow pass variables to type-lambda.
                lc.typeVars.get(tv) match
                    case Some(tp) => constRepresentation(tp)
                    case None =>
                        TypeVarRepresentation(isBuiltin)
            case SIRType.FreeUnificator =>
                TypeVarRepresentation(isBuiltin = false)
            case SIRType.TypeProxy(ref) =>
                constRepresentation(ref)
            case SIRType.TypeNothing => ErrorRepresentation
    }

}
