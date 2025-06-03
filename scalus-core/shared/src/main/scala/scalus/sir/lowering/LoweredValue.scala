package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.Map as MutableMap

/** SEA of nodes - like representation. E.e. each value is node, which manage dependencies.
  */
trait LoweredValue {

    def sirType: SIRType

    def pos: SIRPosition

    /** The UPLC term that represents this value, wrapped in vars if needed.
      * @param gctx - context for term generation
      *  @return generated term wrapped in lambdas with definition of needed uplevel variables
      */
    def termWithNeddedVars(gctx: TermGenerationContext): Term =
        Lowering.generateDominatedUplevelVarsAccess(this)(using gctx)

    /** Generates the term for this value, without any uplevel variables.
      * @param gctx
      * @return
      */
    def termInternal(gctx: TermGenerationContext): Term

    /** The type of representation of this value.
      * @param ctx
      * @return
      */
    def representation: LoweredValueRepresentation

    def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        summon[LoweringContext].typeGenerator(sirType).toRepresentation(this, representation, pos)
    }

    /** Uplevel variables, that shoule be generated before generation of term
      * @return
      */
    def dominatingUplevelVars: Set[IdentifiableLoweredValue]

    def usedUplevelVars: Set[IdentifiableLoweredValue]

    /** add identifiable variable to be updated from this variable */
    def addDependent(
        value: IdentifiableLoweredValue
    ): Unit

}

case class ConstantLoweredValue(
    sir: SIR.Const,
    term: Term.Const,
    representation: LoweredValueRepresentation
) extends LoweredValue {
    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos
    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    override def usedUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    override def termInternal(gctx: TermGenerationContext): Term = term
    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        // constants are not depended on any variables
    }
}

case class StaticLoweredValue(
    sir: AnnotatedSIR,
    term: Term,
    representation: LoweredValueRepresentation
) extends LoweredValue {

    def sirType: SIRType = sir.tp
    def pos: SIRPosition = sir.anns.pos
    def dominatingUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    def usedUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    def termInternal(gctx: TermGenerationContext): Term = term
    def addDependent(value: IdentifiableLoweredValue): Unit = {}

}

/** Values with id which can be represented by variables (if needed).
  *
  * Identificatin is id.
  */
sealed trait IdentifiableLoweredValue extends LoweredValue {
    def id: String

    def optRhs: Option[LoweredValue]

    override def hashCode(): Int = id.hashCode()

    override def equals(obj: Any): Boolean = obj match {
        case that: IdentifiableLoweredValue => this.id == that.id
        case _                              => false
    }

    /** Set of variables, which this value are directly used in. (i.e. the have this value in rhs).
      * non-transitive.
      * @return
      */
    def directDepended: MutableSet[IdentifiableLoweredValue]

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        directDepended.add(value)
    }

}

class VariableLoweredValue(
    val id: String, // unqiue id for variable, to avoid clushes with shallowed names
    val name: String,
    val sir: SIR.Var,
    val representation: LoweredValueRepresentation,
    val otherRepresentations: MutableMap[
      LoweredValueRepresentation,
      DependendVariableLoweredValue
    ] = MutableMap.empty,
    val optRhs: Option[LoweredValue] = None,
    val directDepended: MutableSet[IdentifiableLoweredValue] = MutableSet.empty
) extends IdentifiableLoweredValue {

    optRhs.foreach { rhs =>
        rhs.addDependent(this)
    }

    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        optRhs.map(rhs => rhs.dominatingUplevelVars).getOrElse(Set.empty)

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        Set(this) ++ optRhs.map(rhs => rhs.usedUplevelVars).getOrElse(Set.empty)

    override def termInternal(gctx: TermGenerationContext): Term = {
        if gctx.generatedVars.contains(id) then Term.Var(NamedDeBruijn(id))
        else
            optRhs match {
                case Some(rhs) =>
                    // if we here, it means that we was not included in any domination set,
                    //  so variable used once and we can generate rhs term instead of name.
                    rhs.termWithNeddedVars(gctx)
                case None =>
                    throw new IllegalStateException(
                      s"Variable $name with id $id is not defined and has no rhs to generate term."
                    )
            }
    }

}

/** Represent a variable which is dependent on another variable. i.e. var x1 =
  * (toOtherReperesentation(y))
  */
case class DependendVariableLoweredValue(
    id: String,
    name: String,
    sir: SIR.Var,
    representation: LoweredValueRepresentation,
    rhs: LoweredValue,
    directDepended: MutableSet[IdentifiableLoweredValue] = MutableSet.empty
) extends IdentifiableLoweredValue {

    rhs.addDependent(this)

    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        rhs.dominatingUplevelVars

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        Set(this) ++ rhs.usedUplevelVars

    override def optRhs: Option[LoweredValue] = Some(rhs)

    override def termInternal(gctx: TermGenerationContext): Term = {
        if gctx.generatedVars.contains(id) then Term.Var(NamedDeBruijn(id))
        else rhs.termWithNeddedVars(gctx)

    }

}

trait ProxyLoweredValue(origin: LoweredValue) extends LoweredValue {

    override def sirType: SIRType = origin.sirType

    override def pos: SIRPosition = origin.pos

    override def representation: LoweredValueRepresentation = origin.representation

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        origin.dominatingUplevelVars

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        origin.usedUplevelVars

    override def addDependent(value: IdentifiableLoweredValue): Unit =
        origin.addDependent(value)

}

object LoweredValue {

    def intConstant(value: Int, pos: SIRPosition): ConstantLoweredValue = {
        ConstantLoweredValue(
          SIR.Const(
            Constant.Integer(value),
            SIRType.Integer,
            AnnotationsDecl(pos)
          ),
          Term.Const(Constant.Integer(value)),
          PrimitiveRepresentation.Constant
        )
    }

}
