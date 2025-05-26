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

    /** usage count.
      */
    def usageCount: Int

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

    private var usageCount: Int = 0

    def toRepresentation(representation: LoweredValueRepresentation)(using
        LoweringContext
    ): LoweredValue

    override def dependencies: Seq[LoweredValue] = Seq.empty

}

class DependedLoweredValue(
    id: String,
    sir: SIR,
    representation: LoweredValueRepresentation
    dependencies: Seq[LoweredValue],
    termFun: (Seq[Term] => Term),
)

object DependedLoweredValue {

    def apply(
        sir: SIR,
        dependencies: Seq[LoweredValue],
        representation: LoweredValueRepresentation,
        termFun: (Seq[Term] => Term)
    )(using LoweringContext): DependedLoweredValue = {
        val id = s"dep_${sir.id}"
        val term = termFun(dependencies.map(_.term))
        new DependedLoweredValue(id, sir, representation, dependencies, term)
    }

}

object LoweredValue {

    def intConstant(n: Int, anns: AnnotationsDecl): LoweredValue = {
        StaticLoweredValue(
          SIR.Const(Constant.Integer(n), SIRType.Integer, anns),
          Term.Const(Constant.Integer(n)),
          PrimitiveRepresentation.Constant
        )
    }

    def boolConstant(b: Boolean): LoweredValue = {
        LoweredValue(
          SIR.Const(Constant.Bool(b), SIRType.Boolean, AnnotationsDecl.empty),
          Term.Const(Constant.Bool(b)),
          PrimitiveRepresentation.Constant
        )
    }

    def namedVar(
        name: String,
        tp: SIRType,
        representation: LoweredValueRepresentation,
        anns: AnnotationsDecl
    ): LoweredValue = {
        LoweredValue(
          SIR.Var(name, tp, anns),
          Term.Var(NamedDeBruijn(name)),
          representation
        )
    }

    def lambda(
        name: String,
        tp: SIRType,
        representation: LoweredValueRepresentation,
        body: LoweredValue,
        anns: AnnotationsDecl
    ): LoweredValue = {
        LoweredValue(
          SIR.LamAbs(SIR.Var(name, tp, anns), body.sir, anns),
          Term.LamAbs(name, body.term),
          LambdaRepresentaion(representation, body.representation)
        )
    }

}
