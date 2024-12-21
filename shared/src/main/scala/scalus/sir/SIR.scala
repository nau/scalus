package scalus.sir

import scalus.sir.SIRType.{checkAllProxiesFilled, FreeUnificator}
import scalus.uplc.Constant
import scalus.uplc.DefaultFun

case class Module(version: (Int, Int), defs: List[Binding])

case class Binding(name: String, value: SIR) {

    override def toString: String = s"Binding(\"$name\" : $value)"

}

case class TypeBinding(name: String, tp: SIRType) {
    override def toString: String = s"TypeBinding(\"$name\" : ${tp.show})"
}

enum Recursivity:
    case NonRec, Rec

case class ConstrDecl(
    name: String,
    storageType: SIRVarStorage,

    /** Parameters of the constructor.
      */
    params: List[TypeBinding],

    /** Type parameters of this type.
      */
    typeParams: List[SIRType.TypeVar]

    /// **
    // * Type of the constructor.
    // (moved to DataDecl,  since when we have a hierarchy with more than one level,
    //  then we map this to few DataDecl, and each DataDecl has its own constructor type).
    // */
    // parentTypeArgs: List[SIRType]

) {

    if (name.contains(" ") || name.contains("\u0021")) {
        throw new RuntimeException("Invalud name in constructor: " + name)
    }

    private var _tp: SIRType = null

    def tp: SIRType =
        if (_tp == null) then
            _tp = typeParams match
                case Nil => SIRType.CaseClass(this, typeParams)
                case tp :: tail =>
                    SIRType.TypeLambda(typeParams, SIRType.CaseClass(this, typeParams))
        _tp

}

//  Data~ Lost(Const)

//  A -> B -> C1, C2
//    -> B1 -> D

//f:  B => Int   B = Data(C1,C2):     \lambda (c1,c2) => c2(x)
//f:  A => Int   A = Data(C1, C2, D)

case class DataDecl(
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar]
    /*constructorTypeMapping: Map[String, List[SIRType]]*/
) {

    private var _tp: SIRType = null

    def tp: SIRType =
        if _tp == null then
            _tp = typeParams match
                case Nil => SIRType.SumCaseClass(this, Nil)
                case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))
        _tp

}

//case class ExternalDataDecl(module: String, name: String)

sealed trait SIR {

    def tp: SIRType

    def ~=~(that: SIR): Boolean =
        SIRUnify.unifySIR(this, that, SIRUnify.Env.empty) match {
            case SIRUnify.UnificationSuccess(_, _)    => true
            case SIRUnify.UnificationFailure(_, _, _) => false
        }

}

object SIR:

    //  Module A:
    //   case class AClass(x:Int, y: String)
    //  Module b:
    //    val x = A.AClass(1, "hello")
    //    val y = A.AClass(2, "world")

    // Mole b SIR:  (var1)
    //  Var(x,  SumType("AClass", List(("x", Int), ("y", String))))
    //  Var(y,  SumType("AClass", List(("x", Int), ("y", String))))

    // Module b SIR:
    //  TypeAlias(1, SumType("AClass", List(("x", Int), ("y", String))))
    //  Var(x,  TypeRef(1))

    case class Var(name: String, tp: SIRType) extends SIR {
        override def toString: String = s"Var($name, ${tp.show})"
    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType) extends SIR {

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    case class Let(recursivity: Recursivity, bindings: List[Binding], body: SIR) extends SIR {
        override def tp: SIRType = body.tp
    }

    case class LamAbs(param: Var, term: SIR) extends SIR {

        override def tp: SIRType =
            term.tp match
                case SIRType.TypeLambda(tvars, tpexpr) =>
                    SIRType.TypeLambda(tvars, SIRType.Fun(param.tp, tpexpr))
                case _ =>
                    SIRType.Fun(param.tp, term.tp)

    }

    case class Apply(f: SIR, arg: SIR, tp: SIRType) extends SIR {

        // TODO: makr tp computable, not stored.  (implement subst at first).
        /*
        def recheckTp: SIRType = f.tp match {
            case SIRType.Fun(in, out) => out
            case SIRType.TypeLambda(tp1, tpexpr) =>
                val sftp = tpexpr.subst(tp1,arg.tp)
                sftp match {
                    case SIRType.Fun(in, out) => out
                    case _ => throw new Exception("Apply: f is not a function")
                }
            case _ => throw new Exception("Apply: f is not a function")
        }
         */

    }

    case class Const(uplcConst: Constant, tp: SIRType) extends SIR

    case class And(a: SIR, b: SIR) extends SIR {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }

    case class Or(a: SIR, b: SIR) extends SIR {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }

    case class Not(a: SIR) extends SIR {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }

    case class IfThenElse(cond: SIR, t: SIR, f: SIR, tp: SIRType) extends SIR

    case class Builtin(bn: DefaultFun, tp: SIRType) extends SIR

    case class Error(msg: String, cause: Throwable | Null = null) extends SIR {
        override def tp: SIRType = SIRType.TypeError(msg, cause)
    }

    // TODO: unify data-decl.
    case class Constr(name: String, data: DataDecl, args: List[SIR]) extends SIR {
        override def tp: SIRType = data.tp
    }

    case class Case(
        constr: ConstrDecl,
        bindings: List[String],
        typeBindings: List[SIRType],
        body: SIR
    )

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: SIR, cases: List[Case], tp: SIRType) extends SIR

    case class Hole(name: String, override val tp: SIRType = FreeUnificator) extends SIR

    case class Decl(data: DataDecl, term: SIR) extends SIR {

        override def tp: SIRType = term.tp

    }

case class Program(version: (Int, Int, Int), term: SIR)

object SIRChecker {

    case class CheckException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    def checkAndThrow(SIR: SIR, throwOnFirst: Boolean = true): Unit = {
        val errors = checkSIR(SIR, throwOnFirst)
        if errors.nonEmpty then throw new CheckException(errors.mkString("\n"))
    }

    def checkSIR(sir: SIR, throwOnFirst: Boolean): Seq[String] = {
        checkExpr(sir, throwOnFirst)
    }

    def checkData(data: DataDecl, throwOnFirst: Boolean): Seq[String] = {
        val checkConstrs = data.constructors.flatMap(c => checkConstr(c, throwOnFirst))
        val checkTp = checkType(data.tp, throwOnFirst)
        checkConstrs ++ checkTp
    }

    def checkConstr(constr: ConstrDecl, throwOnFirst: Boolean): Seq[String] = {
        val checkParams = constr.params.flatMap(b => checkTypeBinding(b, throwOnFirst))
        val checkTp = checkType(constr.tp, throwOnFirst)
        checkParams ++ checkTp
    }

    def checkTypeBinding(tpb: TypeBinding, throwOnFirst: Boolean): Seq[String] =
        checkType(tpb.tp, throwOnFirst)

    def checkExpr(expr: SIR, throwOnFirst: Boolean): Seq[String] = {
        val checkTp = checkType(expr.tp, throwOnFirst)
        expr match {
            case SIR.Var(_, tp)            => checkTp
            case SIR.ExternalVar(_, _, tp) => checkTp
            case SIR.Let(_, bindings, body) =>
                val checkBindings = bindings.flatMap(b => checkExpr(b.value, throwOnFirst))
                checkBindings ++ checkSIR(body, throwOnFirst) ++ checkTp
            case SIR.LamAbs(param, term) =>
                checkType(param.tp, throwOnFirst) ++ checkSIR(term, throwOnFirst) ++ checkTp
            case SIR.Apply(f, arg, tp) =>
                checkSIR(f, throwOnFirst) ++ checkSIR(arg, throwOnFirst) ++ checkTp
            case SIR.Const(_, tp) => checkTp
            case SIR.And(a, b) => checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Or(a, b)  => checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Not(a)    => checkSIR(a, throwOnFirst) ++ checkTp
            case SIR.IfThenElse(cond, t, f, tp) =>
                checkSIR(cond, throwOnFirst) ++ checkSIR(t, throwOnFirst) ++ checkSIR(
                  f,
                  throwOnFirst
                ) ++ checkTp
            case SIR.Builtin(_, tp) => checkTp
            case SIR.Error(_, _)    => checkTp
            case SIR.Constr(_, data, args) =>
                val checkArgs = args.flatMap(a => checkSIR(a, throwOnFirst))
                checkData(data, throwOnFirst) ++ checkArgs ++ checkTp
            case SIR.Match(scrutinee, cases, tp) =>
                val checkCases = cases.flatMap(c => checkCase(c, throwOnFirst))
                checkSIR(scrutinee, throwOnFirst) ++ checkCases ++ checkTp
            case SIR.Hole(_, _) => checkTp
            case SIR.Decl(data, term) =>
                checkData(data, throwOnFirst) ++ checkSIR(term, throwOnFirst)
        }
    }

    def checkCase(c: SIR.Case, throwOnFirst: Boolean): Seq[String] = {
        c.typeBindings.flatMap(x => checkType(x, throwOnFirst)) ++
            checkConstr(c.constr, throwOnFirst) ++ checkSIR(c.body, throwOnFirst)
    }

    def checkType(tp: SIRType, throwOnFirst: Boolean): Seq[String] =
        if (SIRType.checkAllProxiesFilled(tp)) then Nil
        else
            val msg = s"Type has unfilled proxies. (tp=${tp})"
            if throwOnFirst then throw CheckException(msg)
            else Seq(msg)

}
