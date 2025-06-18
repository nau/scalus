package scalus.sir.lowering

import scalus.sir.*
import scalus.sir.lowering.Lowering.tpf
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
    def termWithNeededVars(gctx: TermGenerationContext): Term =
        Lowering.generateDominatedUplevelVarsAccess(this)(using gctx)

    /** Generates the term for this value, without any uplevel variables.
      * @param gctx
      * @return
      */
    def termInternal(gctx: TermGenerationContext): Term

    /** The type of representation of this value.
      */
    def representation: LoweredValueRepresentation

    /** Convert this value to the giveb representation,
      */
    def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        summon[LoweringContext].typeGenerator(sirType).toRepresentation(this, representation, pos)
    }

    def upcastOne(targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        summon[LoweringContext].typeGenerator(sirType).upcastOne(this, targetType, pos)

    def maybeUpcast(targetType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue = {
        SIRUnify.unifyType(sirType, targetType, SIRUnify.Env.empty.withoutUpcasting) match
            case SIRUnify.UnificationSuccess(env, tp) =>
                this
            case SIRUnify.UnificationFailure(_, _, _) =>
                // if we cannot unify types, we need to upcast
                //  to the target type.
                val parentsSeq =
                    SIRUnify.subtypeSeq(sirType, targetType, SIRUnify.Env.empty)
                if parentsSeq.isEmpty then {
                    println(s"sirType = ${sirType.show} (${sirType})")
                    println(s"targetType = ${targetType.show} (${targetType})")
                    val parentsSeq1 = SIRUnify.subtypeSeq(
                      sirType,
                      targetType,
                      SIRUnify.Env.empty.withDebug
                    )
                    println("parentsSeq1 = " + parentsSeq1.mkString(", "))

                    throw LoweringException(
                      s"Cannot upcast ${this.show} to ${targetType.show}",
                      pos
                    )
                } else
                    parentsSeq.tail.foldLeft(this) { (s, e) =>
                        s.upcastOne(e, pos)
                    }
    }

    /** Uplevel variables, that shoule be generated before generation of term
      */
    def dominatingUplevelVars: Set[IdentifiableLoweredValue]

    /** Uplevel variables, that are used in this value.
      */
    def usedUplevelVars: Set[IdentifiableLoweredValue]

    /** add identifiable variable to be updated from this variable */
    def addDependent(
        value: IdentifiableLoweredValue
    ): Unit

    def show: String
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
    override def show: String = s"ConstantLoweredValue($term, $representation) at ˜$pos"
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
    def show: String = {
        s"StaticLoweredValue($term, $representation) at $pos"
    }

}

/** Values with id which can be represented by variables (if needed).
  *
  * Identificatin is id.
  */
sealed trait IdentifiableLoweredValue extends LoweredValue {
    def id: String
    def name: String

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

    // if name == "tx" && representation == ProductCaseClassRepresentation.ProdDataList then {
    //    throw LoweringException(
    //      s"Variable $name with id $id has representation $representation, but it is not allowed.",
    //      sir.anns.pos
    //    )
    // }

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
                    rhs.termWithNeededVars(gctx)
                case None =>
                    throw new IllegalStateException(
                      s"Variable $name with id $id is not defined and has no rhs to generate term."
                    )
            }
    }

    override def show: String = {
        s"VariableLoweredValue(id=$id, name=$name, representation=$representation, optRhs=${optRhs.map(_.show)})"
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
        else rhs.termWithNeededVars(gctx)

    }

    override def show: String = {
        s"DependendVariableLoweredValue(id=$id, name=$name, representation=$representation, rhs=${rhs.show} at = ${sir.anns.pos})"
    }

}

trait ProxyLoweredValue(val origin: LoweredValue) extends LoweredValue {

    override def sirType: SIRType = origin.sirType

    override def pos: SIRPosition = origin.pos

    override def representation: LoweredValueRepresentation = origin.representation

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        origin.dominatingUplevelVars

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        origin.usedUplevelVars

    override def addDependent(value: IdentifiableLoweredValue): Unit =
        origin.addDependent(value)

    override def show: String = s"ProxyLoweredValue(${origin.show})"

}

trait ComplexLoweredValue(ownVars: Set[IdentifiableLoweredValue], subvalues: LoweredValue*)
    extends LoweredValue {

    lazy val usedVarsCount = Lowering.filterAndCountVars(
      v => !ownVars.contains(v),
      subvalues*
    )

    lazy val cUsedVars = usedVarsCount.keySet

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        usedVarsCount.filter { case (v, c) => c > 1 && v.directDepended.size > 1 }.keySet

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        cUsedVars

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        subvalues.foreach(_.addDependent(value))
    }

    override def show: String = toString

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

    /** Builder for LoweredValue, to avoid boilerplate code. Import this object to make available
      */
    object Builder {

        def lvIfThenElse(
            cond: LoweredValue,
            thenBranch: LoweredValue,
            elseBranch: LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {

            // val resType = SIRType.leastUpperBound(thenBranch.sirType, elseBranch.sirType)

            val resType = SIRUnify.unifyType(
              thenBranch.sirType,
              elseBranch.sirType,
              SIRUnify.Env.empty.withUpcasting
            ) match {
                case SIRUnify.UnificationSuccess(_, tp)                => tp
                case failure @ SIRUnify.UnificationFailure(path, l, r) =>
                    // if we cannot unify types, we need to upcast
                    //  to the target type.
                    println("Unification failure: " + failure)
                    SIRType.FreeUnificator
            }

            if resType == SIRType.FreeUnificator then
                throw LoweringException(
                  s"if-then-else branches return unrelated types: ${thenBranch.sirType.show} and ${elseBranch.sirType.show}",
                  inPos
                )

            val thenBranchR = thenBranch.maybeUpcast(resType, inPos)

            val elseBranchR = elseBranch
                .maybeUpcast(resType, inPos)
                .toRepresentation(
                  thenBranchR.representation,
                  inPos
                )

            val condR = cond.toRepresentation(PrimitiveRepresentation.Constant, inPos)

            // TODO: result type shoulb be least upper bound of thenBranch and elseBranch

            new ComplexLoweredValue(Set.empty, condR, thenBranchR, elseBranchR) {
                override def sirType: SIRType = resType
                override def pos: SIRPosition = inPos
                override def representation: LoweredValueRepresentation =
                    thenBranchR.representation

                override def termInternal(gctx: TermGenerationContext): Term = {
                    !(DefaultFun.IfThenElse.tpf $
                        condR.termWithNeededVars(gctx) $
                        ~thenBranchR.termWithNeededVars(gctx) $
                        ~elseBranchR.termWithNeededVars(gctx))
                }

                override def show: String = {
                    s"IfThenElse(${condR.show}, ${thenBranchR.show}, ${elseBranchR.show})"
                }

            }

        }

        def lvApply(
            f: LoweredValue,
            arg: LoweredValue,
            inPos: SIRPosition,
            resTp: Option[SIRType] = None,
            resRepresentation: Option[LoweredValueRepresentation] = None
        )(using
            lctx: LoweringContext
        ): LoweredValue = {

            def firstArgType(tp: SIRType): SIRType = tp match {
                case SIRType.Fun(argTp, _) => argTp
                case SIRType.TypeLambda(params, body) =>
                    firstArgType(body)
                case SIRType.TypeProxy(ref) =>
                    if ref != null then firstArgType(ref.asInstanceOf[SIRType])
                    else
                        throw LoweringException(
                          s"Empty type proxy in function application",
                          inPos
                        )
                case _ =>
                    throw LoweringException(
                      s"Cannot apply function to argument, because function type is not a function: ${tp.show}",
                      inPos
                    )
            }

            val resType = resTp.getOrElse(
              SIRType.calculateApplyType(f.sirType, arg.sirType, lctx.typeVars)
            )

            val targetArgType = firstArgType(f.sirType) match {
                case SIRType.TypeVar(name, optId) => arg.sirType
                case SIRType.FreeUnificator       => arg.sirType
                case other                        => other
            }

            // println(s"targetArgType = ${targetArgType.show}")

            val targetArgRepresentation = lctx.typeGenerator(targetArgType).defaultRepresentation

            // println(s"targetArgRepresentation = ${targetArgRepresentation}")

            val argInDefaultRepresentation =
                if SIRType.isPolyFunOrFun(targetArgType) then arg
                else
                    arg
                        .maybeUpcast(targetArgType, inPos)
                        .toRepresentation(
                          targetArgRepresentation,
                          inPos
                        )

            // println(s"argInDefaultRepresentation = ${argInDefaultRepresentation}")

            new ComplexLoweredValue(Set.empty, f, arg) {

                override def sirType: SIRType = resType

                override def pos: SIRPosition = inPos

                override def representation: LoweredValueRepresentation =
                    resRepresentation.getOrElse(
                      lctx.typeGenerator(resType).defaultRepresentation
                    )

                override def termInternal(gctx: TermGenerationContext): Term = {
                    Term.Apply(
                      f.termWithNeededVars(gctx),
                      argInDefaultRepresentation.termWithNeededVars(gctx)
                    )
                }

                override def show: String = {
                    s"lvApply(${f.show}, ${argInDefaultRepresentation.show}) at $inPos"
                }

            }
        }

        def lvEqualsInteger(x: LoweredValue, y: LoweredValue, inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            new ComplexLoweredValue(Set.empty, x, y) {
                override def sirType: SIRType = SIRType.Boolean

                override def pos: SIRPosition = inPos

                override def representation: LoweredValueRepresentation =
                    PrimitiveRepresentation.Constant

                override def termInternal(gctx: TermGenerationContext): Term = {
                    Term.Apply(
                      Term.Apply(
                        Lowering.forcedBuiltin(DefaultFun.EqualsInteger),
                        x.termWithNeededVars(gctx)
                      ),
                      y.termWithNeededVars(gctx)
                    )
                }
            }
        }

        def lvLamAbs(
            name: String,
            tp: SIRType,
            representation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            lvLamAbs(SIR.Var(name, tp, AnnotationsDecl(inPos)), representation, f, inPos)
        }

        def lvLamAbs(
            sirVar: SIR.Var,
            varRepresentation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            val newVar = new VariableLoweredValue(
              id = lctx.uniqueVarName(sirVar.name),
              name = sirVar.name,
              sir = sirVar,
              representation = varRepresentation
            )
            val prevScope = lctx.scope
            lctx.scope = lctx.scope.add(newVar)
            val body = f(newVar)(using lctx)
            lctx.scope = prevScope
            new ComplexLoweredValue(Set(newVar), body) {
                override def sirType: SIRType = SIRType.Fun(sirVar.tp, body.sirType)

                override def representation: LoweredValueRepresentation =
                    LambdaRepresentation(newVar.representation, body.representation)

                override def pos: SIRPosition = inPos

                override def termInternal(gctx: TermGenerationContext): Term =
                    Term.LamAbs(newVar.id, body.termWithNeededVars(gctx.addGeneratedVar(newVar.id)))
            }
        }

        /** create let and add it to the current scope.
          */
        def lvNewLazyIdVar(
            id: String,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            rhs: LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): IdentifiableLoweredValue = {
            val newVar: VariableLoweredValue = new VariableLoweredValue(
              id = id,
              name = id,
              sir = SIR.Var(id, tp, AnnotationsDecl(inPos)),
              representation = lvr,
              optRhs = Some(rhs(using lctx)),
            )
            lctx.scope = lctx.scope.add(newVar)
            newVar
        }

        def lvNewLazyNamedVar(
            name: String,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            rhs: LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): IdentifiableLoweredValue = {
            val newVar: VariableLoweredValue = new VariableLoweredValue(
              id = lctx.uniqueVarName(name),
              name = name,
              sir = SIR.Var(name, tp, AnnotationsDecl(inPos)),
              representation = lvr,
              optRhs = Some(rhs(using lctx)),
            )
            lctx.scope = lctx.scope.add(newVar)
            newVar
        }

        def lvBuiltinApply(
            fun: SIR.Builtin,
            arg: LoweredValue,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            new ProxyLoweredValue(arg) {
                override def sirType: SIRType = tp

                override def pos: SIRPosition = inPos

                override def termInternal(gctx: TermGenerationContext): Term =
                    Term.Apply(
                      fun.bn.tpf,
                      arg.termWithNeededVars(gctx)
                    )

                override def representation: LoweredValueRepresentation = lvr
            }
        }

        def lvIntConstant(
            value: Int,
            pos: SIRPosition
        )(using lctx: LoweringContext): ConstantLoweredValue = {
            ConstantLoweredValue(
              SIR.Const(Constant.Integer(value), SIRType.Integer, AnnotationsDecl(pos)),
              Term.Const(Constant.Integer(value)),
              PrimitiveRepresentation.Constant
            )
        }

        def lvBuiltinApply0(
            fun: SIR.Builtin,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            StaticLoweredValue(
              SIR.Builtin(fun.bn, tp, AnnotationsDecl(inPos)),
              Lowering.forcedBuiltin(fun.bn) $ Term.Const(Constant.Unit),
              lvr
            )
        }

        def lvDataNil(inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            lvBuiltinApply0(
              SIRBuiltins.mkNilData,
              SIRType.Data,
              PrimitiveRepresentation.PackedData,
              inPos
            )
        }

        def lvBuiltinApply2(
            fun: SIR.Builtin,
            arg1: LoweredValue,
            arg2: LoweredValue,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            new ComplexLoweredValue(Set.empty, arg1, arg2) {
                override def sirType: SIRType = tp

                override def pos: SIRPosition = inPos

                override def termInternal(gctx: TermGenerationContext): Term =
                    Term.Apply(
                      Term.Apply(
                        Lowering.forcedBuiltin(fun.bn),
                        arg1.termWithNeededVars(gctx)
                      ),
                      arg2.termWithNeededVars(gctx)
                    )

                override def representation: LoweredValueRepresentation = lvr
            }
        }

    }

}
