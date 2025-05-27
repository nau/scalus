package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

/** SEA of nodes - like representation. E.e. each value is node,
  */
trait LoweredValue {

    def id: String

    /** Associated SIR (for debug)
      * @return
      */
    def sir: SIR

    /** The UPLC term that represents this value.
      */
    def term: Term

    /** The type of representation of this value.
      * @param ctx
      * @return
      */
    def representation: LoweredValueRepresentation

    /** Dependencies
      * @return
      */
    def dependencies: Seq[LoweredValue]

    def toRepresentation(representation: LoweredValueRepresentation)(using
        LoweringContext
    ): LoweredValue

}

class StaticLoweredValue(
    override val id: String,
    override val sir: SIR,
    override val term: Term,
    override val representation: LoweredValueRepresentation,
    dataShouldBeChecked: Boolean = false,
    delayed: Int = 0,
    forced: Boolean = false
) extends LoweredValue {

    def toRepresentation(representation: LoweredValueRepresentation)(using
        LoweringContext
    ): LoweredValue

    override def dependencies: Seq[LoweredValue] = Seq.empty

}

class DependedLoweredValue(
    id: String,
    sir: SIR,
    representation: LoweredValueRepresentation,
    dependencies: Seq[LoweredValue],
    termFun: (Seq[Term] => Term),
) extends LoweredValue

object DependedLoweredValue {

    def apply(
        sir: SIR,
        dependencies: Seq[LoweredValue],
        representation: LoweredValueRepresentation,
        termFun: (Seq[Term] => Term)
    )(using lc: LoweringContext): DependedLoweredValue = {
        val id = lc.uniqueNodeName("dep_")
        val term = termFun(dependencies.map(_.term))
        new DependedLoweredValue(id, sir, representation, dependencies, termFun)
    }

}

object LoweredValue {

    def intConstant(n: Int, anns: AnnotationsDecl)(using lc: LoweringContext): LoweredValue = {
        StaticLoweredValue(
          lc.uniqueNodeName("int_"),
          SIR.Const(Constant.Integer(n), SIRType.Integer, anns),
          Term.Const(Constant.Integer(n)),
          PrimitiveRepresentation.Constant
        )
    }

    def boolConstant(b: Boolean)(using lc: LoweringContext): LoweredValue = {
        StaticLoweredValue(
          lc.uniqueNodeName("bool_"),
          SIR.Const(Constant.Bool(b), SIRType.Boolean, AnnotationsDecl.empty),
          Term.Const(Constant.Bool(b)),
          PrimitiveRepresentation.Constant
        )
    }

}
