package scalus.sir.lowering

import scalus.sir.{SIRType, *}
import scalus.sir.lowering.Lowering.tpf
import scalus.sir.lowering.typegens.RepresentationProxyLoweredValue
import scalus.uplc.*
import org.typelevel.paiges.Doc
import scalus.sir.PrettyPrinter.pretty

import scala.annotation.tailrec
import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.Map as MutableMap

/** SEA of nodes - like representation. E.e. each value is node, which manage dependencies.
  */
trait LoweredValue {

    val createdEx = new RuntimeException("Lovered value created here")

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
            case SIRUnify.UnificationFailure(path, l, r) =>
                // if we cannot unify types, we need to upcast
                //  to the target type.
                val parentsSeq =
                    SIRUnify.subtypeSeq(sirType, targetType, SIRUnify.Env.empty)
                if parentsSeq.isEmpty then {
                    println(
                      s"first unify failure: path = ${path}, left = ${l}, right = ${r}"
                    )
                    val debugUnification = SIRUnify.unifyType(
                      sirType,
                      targetType,
                      SIRUnify.Env.empty.withoutUpcasting.withDebug
                    )
                    throw LoweringException(
                      s"Cannot upcast ${this.sirType.show} to ${targetType.show}",
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

    def show: String = pretty.render(100)

    /** Pretty print this value with all definitona
      * @param style - style of printing
      * @return
      */
    def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc

    /** Pretty print reference to value
      * @param style - style of printing
      * @return
      */
    def docRef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc

    def pretty: Doc = {
        docDef(PrettyPrinter.Style.Normal)
    }

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

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        (PrettyPrinter.pretty(term, style) + Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter
            .inBrackets(representation.doc)).grouped
    }

    override def docRef(style: PrettyPrinter.Style): Doc = PrettyPrinter.pretty(term, style)

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
    def print: String = {
        s"StaticLoweredValue($term, $representation) at $pos"
    }
    def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        Doc.text("static") + inParens(
          PrettyPrinter.pretty(term, style) + Doc.text(":") + Doc.text(sir.tp.show) + inBrackets(
            Doc.text(representation.toString)
          )
        )
    }

    def docRef(style: PrettyPrinter.Style): Doc = {
        PrettyPrinter.pretty(term, style)
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

    if id == "x$126" then {
        println(
          s"VariableLoweredValue created with id = $id,  representation = $representation, sirType=${sir.tp.show}"
        )
        println(
          s"SIR=${sir} at ${sir.anns.pos.file}:${sir.anns.pos.startLine + 1}"
        )
        // throw RuntimeException("QQQ")
    }

    optRhs.foreach { rhs =>
        rhs.addDependent(this)
    }

    // if id == "scalus.ledger.api.v1.Credential$.given_Eq_Credential252" then {
    //    println(
    //      s"VariableLoweredValue created with id = $id,  representation = $representation, sirType=${sir.tp.show}"
    //    )
    //    println(
    //      s"SIR=${sir} at ${sir.anns.pos.file}:${sir.anns.pos.startLine + 1}"
    //    )
    //    throw new RuntimeException(
    //      s"VariableLoweredValue created with id = $id,  representation = $representation, sirType=${sir.tp}"
    //    )
    // }

    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        optRhs.map(rhs => rhs.dominatingUplevelVars).getOrElse(Set.empty)

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        Set(this) ++ optRhs.map(rhs => rhs.usedUplevelVars).getOrElse(Set.empty)

    override def termInternal(gctx: TermGenerationContext): Term = {
        if id == "x$126" then
            println("generating term for x$126")
            // Thread.dumpStack()
        if gctx.generatedVars.contains(id) then Term.Var(NamedDeBruijn(id))
        else
            optRhs match {
                case Some(rhs) =>
                    // if we here, it means that we was not included in any domination set,
                    //  so variable used once and we can generate rhs term instead of name.
                    rhs.termWithNeededVars(gctx)
                case None =>
                    if gctx.processUndefinedValues then {
                        if gctx.debug then {
                            println(
                              s"VariableLoweredValue: generating term for undefined variable $name with id $id"
                            )
                            if id == "x$1341" then {
                                println(s"generatedVars: ${gctx.generatedVars}")
                            }
                            // createdEx.printStackTrace()
                        }
                        Term.Var(NamedDeBruijn(id))
                    } else
                        throw new IllegalStateException(
                          s"Variable $name with id $id is not defined and has no rhs to generate term."
                        )

            }
    }

    def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        val head = (Doc.text("var") + inParens(
          Doc.text(id) + Doc.text(":") + Doc.text(sirType.show) + inBrackets(
            Doc.text(representation.toString)
          )
        )).grouped
        optRhs match {
            case Some(rhs) =>
                head + Doc.text("=") + rhs.docDef(style)
            case None =>
                head
        }
    }

    def docRef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        Doc.text(id)
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

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        (Doc.text("depvar") + inParens(
          Doc.text(id) + Doc.text(":") + Doc.text(sirType.show) + inBrackets(
            Doc.text(representation.toString)
          )
        )).grouped + Doc.text("=") + rhs.docDef(style)
    }

    override def docRef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        Doc.text(id)
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

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        (Doc.text("proxy") + PrettyPrinter.inParens(
          origin.docDef(style) + Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
            representation.doc
          )
        )).grouped
    }

    override def docRef(style: PrettyPrinter.Style): Doc =
        Doc.text("proxy") + PrettyPrinter.inParens(origin.docRef(style))

}

/** ComplexLoweredValue is a base trait for lowered values that consist of multiple subvalues.
  * ownVars - var that used in subvalues but defined in this value. (i.e. not propagated up in
  * dominatedUplevelVars)
  */
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

    def retrieveSubvalues: Seq[LoweredValue] = subvalues

    override def docRef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        this.docDef(style)
    }

}

case class LambdaLoweredValue(newVar: VariableLoweredValue, body: LoweredValue, inPos: SIRPosition)
    extends ComplexLoweredValue(Set(newVar), body) {
    override def sirType: SIRType = SIRType.Fun(newVar.sirType, body.sirType)

    override def representation: LoweredValueRepresentation = {
        LambdaRepresentation(newVar.representation, body.representation)
    }

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        if newVar.id == "x$1341" then {
            println(
              "printinq termInternal for lvLamAbs with newVar.id = x$1341"
            )
        }
        Term.LamAbs(newVar.id, body.termWithNeededVars(gctx.addGeneratedVar(newVar.id)))
    }

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        val left = Doc.text("(") +
            Doc.text("lam") & Doc.text(newVar.id) + Doc.text(":") + Doc.text(
              sirType.show
            ) + inBrackets(
              representation.doc
            ) + Doc.text(".")
        val right = Doc.text(")")
        body.docRef(style).bracketBy(left, right)
    }

}

abstract class BuiltinApplyLoweredValue(
    fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    args: LoweredValue*
) extends ComplexLoweredValue(Set.empty, args*) {

    override def sirType: SIRType = tp

    override def representation: LoweredValueRepresentation = repr

    override def pos: SIRPosition = inPos

}

case class BuilinApply1LoweredVale(
    fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    arg: LoweredValue
) extends BuiltinApplyLoweredValue(fun, tp, repr, inPos, arg) {

    override def termInternal(gctx: TermGenerationContext): Term =
        Term.Apply(
          fun.bn.tpf,
          arg.termWithNeededVars(gctx)
        )

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        val left = Doc.text(fun.bn.name) + Doc.text("(")
        val right = Doc.text(")")
        arg.docRef(style).bracketBy(left, right).grouped
    }

}

case class BuilinApply2LoweredVale(
    fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    arg1: LoweredValue,
    arg2: LoweredValue
) extends BuiltinApplyLoweredValue(fun, tp, repr, inPos, arg1, arg2) {

    override def termInternal(gctx: TermGenerationContext): Term =
        Term.Apply(
          Term.Apply(
            Lowering.forcedBuiltin(fun.bn),
            arg1.termWithNeededVars(gctx)
          ),
          arg2.termWithNeededVars(gctx)
        )

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        import Doc.*
        text(fun.bn.name) + inParens(
          intercalate(lineOrSpace, List(arg1.docRef(style), arg2.docRef(style)))
        )
    }

}

case class ApplyLoweredValue(
    f: LoweredValue,
    arg: LoweredValue,
    sirType: SIRType,
    repr: LoweredValueRepresentation,
    pos: SIRPosition
) extends ComplexLoweredValue(Set.empty, f, arg) {

    override def representation: LoweredValueRepresentation = repr

    override def termInternal(gctx: TermGenerationContext): Term = {
        Term.Apply(
          f.termWithNeededVars(gctx),
          arg.termWithNeededVars(gctx)
        )
    }

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        import Doc.*
        ((text("App") + inParens(
          f.docRef(style).nested(2) + lineOrSpace + arg.docRef(
            style
          )
        )) + lineOrSpace +
            text(":") + text(sirType.show) + inBrackets(
              representation.doc.nested(2)
            ) + lineOrSpace).grouped.aligned
    }

    override def docRef(style: PrettyPrinter.Style): Doc = {
        (Doc.text("App") + PrettyPrinter.inParens(
          f.docRef(style).nested(2) + Doc.lineOrSpace + arg.docRef(style).nested(2)
        )).grouped
    }

}

case class LetNonRecLoweredValue(
    bindings: Seq[(IdentifiableLoweredValue, LoweredValue)],
    body: LoweredValue,
    inPos: SIRPosition
) extends ComplexLoweredValue(bindings.map(_._1).toSet, (bindings.map(_._2) ++ Seq(body))*) {

    override def sirType: SIRType = body.sirType

    override def representation: LoweredValueRepresentation =
        body.representation

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        val bodyGctx = gctx.copy(
          generatedVars = gctx.generatedVars ++ bindings.map(_._1.id)
        )
        val bodyTerm = body.termWithNeededVars(bodyGctx)
        bindings.foldRight(bodyTerm) { case ((varVal, rhs), term) =>
            Term.Apply(
              Term.LamAbs(varVal.id, term),
              rhs.termWithNeededVars(
                gctx.copy(
                  generatedVars = gctx.generatedVars + varVal.id
                )
              )
            )
        }
    }

    override def docDef(style: PrettyPrinter.Style): Doc = {
        import PrettyPrinter.*

        val bindingsDoc = bindings.map { case (v, rhs) =>
            (v.docRef(style) + Doc.text("=") + rhs.docRef(style).nested(2)).grouped
        }
        val left = Doc.text("let") + Doc.intercalate(
          Doc.lineOrSpace,
          bindingsDoc
        ) + Doc.text("in")
        val right = Doc.empty
        body.docRef(style).bracketBy(left, right)
    }

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        super.addDependent(value)
        bindings.foreach { case (v, rhs) =>
            v.addDependent(value)
        }
    }

}

case class LetRecLoweredValue(
    newVar: IdentifiableLoweredValue,
    rhs: LoweredValue,
    body: LoweredValue,
    inPos: SIRPosition
) extends ComplexLoweredValue(Set(newVar), rhs, body) {

    override def sirType: SIRType = body.sirType

    override def pos: SIRPosition = inPos

    /*  let rec f  = x => f (x + 1)
        in f 0
        (\f -> f 0) (Z (\f. \x. f (x + 1)))
     */
    override def termInternal(gctx: TermGenerationContext): Term = {
        val nGctx = gctx.copy(
          generatedVars = gctx.generatedVars + newVar.id
        )
        val fixed =
            Term.Apply(
              Term.Var(NamedDeBruijn("__z_combinator__")),
              Term.LamAbs(
                newVar.id,
                rhs.termWithNeededVars(nGctx)
              )
            )
        Term.Apply(
          Term.LamAbs(newVar.id, body.termWithNeededVars(nGctx)),
          fixed
        )
    }

    override def representation: LoweredValueRepresentation =
        body.representation

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        val left = Doc.text("(") + Doc.text("letrec") &
            (newVar.docRef(style) + Doc.text("=") + rhs.docRef(style).nested(2)).grouped & Doc.text(
              "in"
            )
        val right = Doc.text(")")
        body.docRef(style).bracketBy(left, right)
    }

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        super.addDependent(value)
        newVar.addDependent(value)
    }

}

case class IfThenElseLoweredValue(
    cond: LoweredValue,
    thenBranch: LoweredValue,
    elseBranch: LoweredValue,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ComplexLoweredValue(Set.empty, cond, thenBranch, elseBranch) {

    override def sirType: SIRType = tp

    override def representation: LoweredValueRepresentation = repr

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        !(DefaultFun.IfThenElse.tpf $
            cond.termWithNeededVars(gctx) $
            ~thenBranch.termWithNeededVars(gctx) $
            ~elseBranch.termWithNeededVars(gctx))
    }

    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        import PrettyPrinter.*
        import Doc.*
        ((text("if") + (lineOrSpace + cond.docRef(style)).nested(2)).grouped
            + (line + text("then") + (Doc.lineOrSpace + thenBranch.docRef(style)).nested(2)).grouped
            + (line + text("else") + (Doc.lineOrSpace + thenBranch.docRef(style)).nested(
              2
            )).grouped).aligned
    }

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

            IfThenElseLoweredValue(
              condR,
              thenBranchR,
              elseBranchR,
              resType,
              thenBranchR.representation,
              inPos
            )

        }

        def lvApply(
            f: LoweredValue,
            arg: LoweredValue,
            inPos: SIRPosition,
            resTp: Option[SIRType] = None,
            resRepr: Option[LoweredValueRepresentation] = None
        )(using
            lctx: LoweringContext
        ): LoweredValue = {

            val prevDebug = lctx.debug
            f match
                case vlv: VariableLoweredValue
                    if vlv.id == "scalus.ledger.api.v1.Credential$.given_Eq_Credential252" =>
                    lctx.debug = true
                    lctx.log(
                      "applying function with id scalus.ledger.api.v1.Credential$.given_Eq_Credential252"
                    )
                    // println(s"resTp = ${resTp.map(_.show)}, resRepresentation = $resRepr")
                    // resTp match {
                    //    case Some(SIRType.Fun(resIn, resOut)) =>
                    //        if resIn.show == "scalus.ledger.api.v1.Credential$.PubKeyCredential" && resOut == SIRType.Boolean
                    //        then throw new RuntimeException("QQQQ")
                    //    case _ =>
                    // }
                case _ =>

            def argType(tp: SIRType): SIRType = tp match {
                case SIRType.Fun(argTp, _) => argTp
                case SIRType.TypeLambda(params, body) =>
                    argType(body)
                case SIRType.TypeProxy(ref) =>
                    if ref != null then argType(ref.asInstanceOf[SIRType])
                    else
                        throw LoweringException(
                          s"Empty type proxy in function application",
                          inPos
                        )
                case SIRType.FreeUnificator =>
                    SIRType.FreeUnificator
                case _ =>
                    throw LoweringException(
                      s"Exprected function type, but have: ${tp.show}",
                      inPos
                    )
            }

            val (targetArgType, targetArgRepresentation) = argType(f.sirType) match {
                case tv @ SIRType.TypeVar(name, optId, isBuiltin) =>
                    val resolvedFArgType = lctx.resolveTypeVarIfNeeded(tv)
                    if isBuiltin then
                        (
                          resolvedFArgType,
                          lctx.typeGenerator(resolvedFArgType)
                              .defaultRepresentation(resolvedFArgType)
                        )
                    else
                        (
                          resolvedFArgType,
                          lctx.typeGenerator(resolvedFArgType)
                              .defaultTypeVarReperesentation(resolvedFArgType)
                        )
                case SIRType.FreeUnificator =>
                    (
                      arg.sirType,
                      lctx.typeGenerator(arg.sirType).defaultTypeVarReperesentation(arg.sirType)
                    )
                case other =>
                    f.representation match {
                        case LambdaRepresentation(inRepr, outRepr) =>
                            (other, inRepr)
                        case _ =>
                            (other, lctx.typeGenerator(other).defaultRepresentation(other))
                    }

            }
            if lctx.debug then {
                lctx.log(
                  s"lvApply: argType(f.sirType) = ${argType(f.sirType).show}, f.sirType = ${f.sirType.show}, arg.sirType = ${arg.sirType.show}"
                )
                lctx.log(
                  s"lvApply: f = ${f.pretty.render(100)}"
                )
                lctx.log(
                  s"lvApply: f.representation = ${f.representation.doc.render(100)}, arg.representation = ${arg.representation.doc.render(100)}"
                )
                // println("f.createdAt:")
                // f.createdEx.printStackTrace()
                lctx.log(
                  s"lvApply: targetArgType = ${targetArgType.show}, targetArgRepresentation = $targetArgRepresentation"
                )
                lctx.log(
                  s"lvApply: resRepresentation = ${resRepr.getOrElse("None")}"
                )
                lctx.log(
                  s"lvApply: arg = ${arg.pretty.render(80)}"
                )

            }

            // TODO: add to to
            @tailrec
            def isSumCaseClass(tp: SIRType): Boolean = {
                tp match {
                    case x: SIRType.SumCaseClass          => true
                    case SIRType.TypeLambda(params, body) => isSumCaseClass(body)
                    case tv: SIRType.TypeVar =>
                        lctx.typeUnifyEnv.filledTypes.get(tv) match {
                            case Some(filledType) => isSumCaseClass(filledType)
                            case None             => false
                        }
                    case SIRType.TypeProxy(ref) =>
                        if ref != null then isSumCaseClass(ref.asInstanceOf[SIRType])
                        else false
                    case _ => false
                }
            }

            val (argTypevarResolved, typeAligned) = arg.sirType match {
                case tv @ SIRType.TypeVar(name, optId, isBuiltin) =>
                    val resolvedArgType = lctx.resolveTypeVarIfNeeded(arg.sirType)
                    resolvedArgType match
                        case tv1 @ SIRType.TypeVar(name, optId, isBuiltin) =>
                            val targetArgGen = lctx.typeGenerator(targetArgType)
                            val targetArgRepr =
                                if isBuiltin then targetArgGen.defaultRepresentation(targetArgType)
                                else targetArgGen.defaultTypeVarReperesentation(targetArgType)
                            val argTyped = new ProxyLoweredValue(arg) {
                                override def sirType: SIRType = targetArgType
                                override def representation: LoweredValueRepresentation =
                                    targetArgRepr
                                override def pos: SIRPosition = inPos
                                override def termInternal(gctx: TermGenerationContext): Term =
                                    arg.termInternal(gctx)
                            }
                            (argTyped, true)
                        case other =>
                            val gen = lctx.typeGenerator(other)
                            val targetArgRepr =
                                if isBuiltin then gen.defaultRepresentation(other)
                                else gen.defaultTypeVarReperesentation(other)
                            val argTyped = new ProxyLoweredValue(arg) {
                                override def sirType: SIRType = other
                                override def representation: LoweredValueRepresentation =
                                    targetArgRepr
                                override def pos: SIRPosition = inPos
                                override def termInternal(gctx: TermGenerationContext): Term =
                                    arg.termInternal(gctx)
                            }
                            (argTyped, false)
                case _ => (arg, false)
            }

            val argInTargetRepresentation =
                if !typeAligned && isSumCaseClass(targetArgType) then
                    arg
                        .maybeUpcast(targetArgType, inPos)
                        .toRepresentation(
                          targetArgRepresentation,
                          inPos
                        )
                else if SIRType.isPolyFunOrFun(arg.sirType) then {
                    // if we have a function, we need check, are we need to upcast their arguments
                    //  because it can be a polymorphic function with covariant argumets.
                    //  like Eq[Credential],
                    alignTypeArgumentsAndRepresentations(
                      arg,
                      targetArgType,
                      targetArgRepresentation,
                      inPos
                    )
                } else {
                    argTypevarResolved.toRepresentation(targetArgRepresentation, inPos)
                }
            // prin tln(s"argInTargetRepresentation = ${argInTargetRepresentation}")

            val calculatedResType = SIRType.calculateApplyType(
              f.sirType,
              argInTargetRepresentation.sirType,
              Map.empty
            )

            val resType = resTp.getOrElse(calculatedResType)

            val calculatedResRepr = f.representation match
                case LambdaRepresentation(inRepr, outRepr) =>
                    outRepr
                case _ =>
                    throw LoweringException("Expected that f have function representation", inPos)

            if lctx.debug then {
                lctx.log(
                  s"lvApply: resType = ${resType.show}, calculatedResType=${calculatedResType.show}"
                )
                // resTp match {
                //    case Some(SIRType.Fun(resIn, resOut)) =>
                //        if resIn.show == "scalus.ledger.api.v1.Credential$.PubKeyCredential" && resOut == SIRType.Boolean
                //        then throw new RuntimeException("QQQQ")
                //    case _ =>
                // }
            }

            // if (SIRType.isPolyFunOrFun(resType)) then {
            //    val alignTypeArgumentsAndRepresentations(
            //        arg,
            //        targetArgType,
            //        targetArgRepresentation,
            //        inPos
            //    )
            // }

            lctx.debug = prevDebug

            val applied = ApplyLoweredValue(
              f,
              argInTargetRepresentation,
              calculatedResType,
              calculatedResRepr,
              inPos
            )

            val retval =
                if SIRType.isPolyFunOrFun(resType) then
                    if lctx.debug then {
                        lctx.log(
                          s"lvApply: aligning type arguments and representations for resType = ${resType.show}"
                        )
                    }
                    val resRepresentation = resRepr.getOrElse(
                      lctx.typeGenerator(resType).defaultRepresentation(resType)
                    )
                    val retval = alignTypeArgumentsAndRepresentations(
                      applied,
                      resType,
                      resRepresentation,
                      inPos
                    )
                    if lctx.debug then {
                        lctx.log(
                          s"lvApply: retval type after aligning = ${retval.sirType.show}"
                        )
                    }
                    retval
                else if SIRType.isSum(resType) then
                    val upcasted = applied.maybeUpcast(resType, inPos)
                    resRepr match
                        case Some(repr) =>
                            upcasted.toRepresentation(repr, inPos)
                        case None => upcasted
                else
                    resRepr match
                        case Some(repr) =>
                            applied.toRepresentation(repr, inPos)
                        case None =>
                            applied

            if lctx.debug then {
                lctx.log(
                  s"lvApply: retval = ${retval.pretty.render(100)}"
                )
            }

            retval

        }

        def lvEqualsInteger(x: LoweredValue, y: LoweredValue, inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            val xc = x.toRepresentation(PrimitiveRepresentation.Constant, inPos)
            val yc = y.toRepresentation(PrimitiveRepresentation.Constant, inPos)
            BuilinApply2LoweredVale(
              SIRBuiltins.equalsInteger,
              SIRType.Boolean,
              PrimitiveRepresentation.Constant,
              inPos,
              xc,
              yc
            )
        }

        def lvLamAbs(
            name: String,
            tp: SIRType,
            inputRepresentation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            lvLamAbs(
              SIR.Var(name, tp, AnnotationsDecl(inPos)),
              inputRepresentation,
              f,
              inPos
            )
        }

        def lvLamAbs(
            sirVar: SIR.Var,
            inputRepresentation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            val newVar = new VariableLoweredValue(
              id = lctx.uniqueVarName(sirVar.name),
              name = sirVar.name,
              sir = sirVar,
              representation = inputRepresentation
            )
            val prevScope = lctx.scope
            lctx.scope = lctx.scope.add(newVar)
            val body = f(newVar)(using lctx)
            lctx.scope = prevScope
            LambdaLoweredValue(newVar, body, inPos)
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
            BuilinApply1LoweredVale(fun, tp, lvr, inPos, arg)
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
            BuilinApply2LoweredVale(
              fun,
              tp,
              lvr,
              inPos,
              arg1,
              arg2
            )
        }

        def lvCast(expr: LoweredValue, targetType: SIRType, inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            if lctx.debug then {
                lctx.log(
                  s"lvCast: expr.sirType = ${expr.sirType.show}, targetType = ${targetType.show}"
                )
            }

            def castedValue(
                value: LoweredValue = expr,
                changeRepresentation: Boolean = true,
                printWarning: Boolean = true
            ): LoweredValue = {
                if printWarning then {
                    lctx.warn(
                      s"casting unrelated types ${value.sirType.show} and ${targetType.show}",
                      inPos
                    )
                }
                val valueGen = lctx.typeGenerator(value.sirType)
                val tvRepr =
                    if changeRepresentation then
                        value.toRepresentation(
                          valueGen.defaultTypeVarReperesentation(value.sirType),
                          inPos
                        )
                    else value
                new RepresentationProxyLoweredValue(tvRepr, tvRepr.representation, inPos) {
                    override def sirType: SIRType = targetType

                    override def docDef(style: PrettyPrinter.Style): Doc =
                        tvRepr
                            .docRef(style)
                            .bracketBy(
                              Doc.text("cast") + Doc.text("("),
                              Doc.text(":") + Doc.text(targetType.show) + Doc.text(")")
                            )
                }
            }

            SIRUnify.unifyType(
              expr.sirType,
              targetType,
              SIRUnify.Env.empty.withoutUpcasting
            ) match {
                case SIRUnify.UnificationSuccess(_, tp) =>
                    if lctx.debug then println("cast successful without upcasting")
                    // if we can unify types, we can return the expression as is
                    castedValue(changeRepresentation = false, printWarning = false)
                case SIRUnify.UnificationFailure(_, _, _) =>
                    if SIRType.isSum(targetType) then
                        val parentsSeq =
                            SIRUnify.subtypeSeq(expr.sirType, targetType, SIRUnify.Env.empty)
                        if parentsSeq.isEmpty then castedValue(printWarning = true)
                        else {
                            val upcasted = parentsSeq.tail.foldLeft(expr) { (s, e) =>
                                s.upcastOne(e, inPos)
                            }
                            castedValue(
                              upcasted,
                              changeRepresentation = false,
                              printWarning = false
                            )
                        }
                    else
                        val printWarning =
                            targetType != SIRType.FreeUnificator && expr.sirType != SIRType.TypeNothing
                        castedValue(printWarning = printWarning)
            }

        }

    }

    private def alignTypeArgumentsAndRepresentations(
        arg: LoweredValue,
        targetType: SIRType,
        targetRepresentation: LoweredValueRepresentation,
        inPos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // arg

        if lctx.debug then {
            println(
              s"alignTypeArgumentsAndRepresentations: arg.sirType = ${arg.sirType.show}, targetType = ${targetType.show}, arg.representation = ${arg.representation}  targetRepresentation = $targetRepresentation"
            )
        }

        def alignWithChange(
            arg: LoweredValue,
            targetType: SIRType,
            targetRepresentation: LoweredValueRepresentation
        ): (LoweredValue, Boolean) = {

            SIRType.collectPolyOrFun(arg.sirType) match {
                case Some((argParams, argIn, argOut)) =>
                    SIRType.collectPolyOrFun(targetType) match {
                        case Some((targetParams, targetIn, targetOut)) =>
                            val (targetInRepr, targetOutRepr) = targetRepresentation match {
                                case LambdaRepresentation(inRepr, outRepr) => (inRepr, outRepr)
                                case _ =>
                                    throw LoweringException(
                                      s"Expected lambda representation, but got $targetRepresentation",
                                      inPos
                                    )
                            }
                            val (argInRepr, argOutRepr) = arg.representation match {
                                case LambdaRepresentation(inRepr, outRepr) => (inRepr, outRepr)
                                case _ =>
                                    throw LoweringException(
                                      s"Expected lambda representation, but got ${arg.representation} for arg\n${arg.show}",
                                      inPos
                                    )
                            }
                            // if both are functions, we need to align their type arguments
                            val runUpcast = SIRType.isSum(argIn) && SIRUnify
                                .unifyType(
                                  argIn,
                                  targetIn,
                                  lctx.typeUnifyEnv.withoutUpcasting
                                )
                                .isFailure
                            val changeRepresentation = !argInRepr.isCompatible(targetInRepr)
                            val xId = lctx.uniqueVarName("xIn")
                            val xIn = VariableLoweredValue(
                              xId,
                              xId,
                              SIR.Var(xId, targetIn, AnnotationsDecl(inPos)),
                              targetInRepr
                            )
                            var changed = false
                            val prevScope = lctx.scope
                            lctx.scope = lctx.scope.add(xIn)
                            val xInUpcased =
                                if runUpcast then xIn.maybeUpcast(argIn, inPos) else xIn
                            if !(xInUpcased eq xIn) then changed = true
                            val xInAligned =
                                if SIRType.isPolyFunOrFun(targetIn) then
                                    // arg = \\ lambda xIn: targetIn => (xIn'): argIn
                                    val (xInAligned, changed) =
                                        alignWithChange(xInUpcased, argIn, argInRepr)
                                    xInAligned
                                else xInUpcased
                            if !(xInAligned eq xInUpcased) then changed = true
                            val yIn = xInAligned.toRepresentation(argInRepr, inPos)
                            if !(yIn eq xInAligned) then changed = true
                            val xOut = ApplyLoweredValue(arg, yIn, argOut, argOutRepr, inPos)
                            val (resOut, outChanged) =
                                alignWithChange(xOut, targetOut, targetOutRepr)
                            if outChanged then changed = true
                            lctx.scope = prevScope
                            if !changed then (arg, false)
                            else {
                                /*
                                val nextArg = new ComplexLoweredValue(Set(xIn), resOut) {
                                    override def sirType: SIRType = targetType
                                    override def pos: SIRPosition = inPos
                                    override def termInternal(gctx: TermGenerationContext): Term = {
                                        val ngctx = gctx.addGeneratedVar(xIn.id)
                                        Term.LamAbs(xIn.id, resOut.termWithNeededVars(ngctx))
                                    }

                                    override def representation: LoweredValueRepresentation =
                                        targetRepresentation
                                    override def print: String = {
                                        s"Lam(${resOut.print},${xIn.id})"
                                    }
                                }
                                
                                 */
                                val nextArg = LambdaLoweredValue(xIn, resOut, inPos)
                                (nextArg, true)
                            }
                        case None =>
                            // arg is function but targetis npt. Passing function in type params is not supported yet
                            throw LoweringException(
                              s"Cannot apply function with type ${arg.sirType.show} to type ${targetType.show} at $inPos",
                              inPos
                            )
                    }
                case None =>

                    val retval = arg
                        .maybeUpcast(targetType, arg.pos)
                        .toRepresentation(targetRepresentation, arg.pos)
                    val chanded = retval != arg
                    (retval, chanded)
            }

        }

        alignWithChange(arg, targetType, targetRepresentation) match {
            case (alignedArg, changed) =>
                if lctx.debug then {
                    println(
                      s"alignTypeArgumentsAndRepresentations: alignedArg.type = ${alignedArg.sirType.show}, changed = $changed"
                    )
                }
                if changed then alignedArg
                else arg
        }

    }

}
