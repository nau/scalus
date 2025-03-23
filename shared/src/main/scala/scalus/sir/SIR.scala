package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun

case class Module(version: (Int, Int), defs: List[Binding])

case class Binding(name: String, value: SIR) {

    if (name == "pkh" && value.tp == SIRType.FreeUnificator) {
        throw new RuntimeException("Binding with name pkh and value FreeUnitifactor")
    }

    override def toString: String = s"Binding(\"$name\" : $value)"

}

case class TypeBinding(name: String, tp: SIRType) {
    override def toString: String = s"TypeBinding(\"$name\" : ${tp.show})"
}

enum Recursivity:
    case NonRec, Rec

/** SIR Position - position in Scala source code.
  * @param file
  *   -- fiel of position. Empty file ("") means that position is unknown.
  * @param startLine
  * @param startColumn
  * @param endLine
  * @param endColumn
  */
case class SIRPosition(
    file: String,
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int
)

object SIRPosition {

    val empty: SIRPosition = SIRPosition("", 0, 0, 0, 0)

}

case class AnnotationsDecl(
    pos: SIRPosition,
    comment: Option[String] = None,
    data: Map[String, SIR] = Map.empty
)

case object AnnotationsDecl {
    import scala.quoted.*

    inline def empty: AnnotationsDecl = ${ emptyImpl }

    private def emptyImpl(using qctx: Quotes): Expr[AnnotationsDecl] = {
        val scalaPosition = qctx.reflect.Position.ofMacroExpansion
        '{
            AnnotationsDecl(
              SIRPosition(
                file = ${ Expr(scalaPosition.sourceFile.path) },
                startLine = ${ Expr(scalaPosition.startLine) },
                startColumn = ${ Expr(scalaPosition.startColumn) },
                endLine = ${ Expr(scalaPosition.endLine) },
                endColumn = ${ Expr(scalaPosition.endColumn) }
              ),
              comment = None,
              data = Map.empty
            )
        }
    }

}

case class ConstrDecl(
    name: String,
    storageType: SIRVarStorage,

    /** Parameters of the constructor.
      */
    params: List[TypeBinding],

    /** Type parameters of this type.
      */
    typeParams: List[SIRType.TypeVar],

    /** Type parameters of the parent constructor.
      */
    parentTypeArgs: List[SIRType],

    /** Annotations of the constructor.
      */
    annotations: AnnotationsDecl
) {

    if name.contains(" ") || name.contains("\u0021") then {
        throw new RuntimeException("Invalid name in constructor: " + name)
    }

    if (name == "scalus.prelude.These.This") {
        println("Constr with scalus.prelude.These.This")
        throw new RuntimeException("These.This")
    }

}

//  Data~ Lost(Const)

//  A -> B -> C1, C2
//    -> B1 -> D

//f:  B => Int   B = Data(C1,C2):     \lambda (c1,c2) => c2(x)
//f:  A => Int   A = Data(C1, C2, D)

case class DataDecl(
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar],
    annotations: AnnotationsDecl
) {

    private var _tp: SIRType = null
    private var _constrs: Map[String, SIRType] = null

    def tp: SIRType =
        if _tp == null then
            _tp = typeParams match
                case Nil => SIRType.SumCaseClass(this, Nil)
                case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))
        _tp

    def constrType(constrName: String): SIRType =
        if _constrs == null then
            _constrs = constructors
                .map { c =>
                    val parent = typeParams match
                        case Nil => tp
                        case typeParams =>
                            val tpEnv = typeParams.zip(c.parentTypeArgs).toMap
                            SIRType.substitute(tp, tpEnv, Map.empty)
                    val sirType = c.typeParams match
                        case Nil => SIRType.CaseClass(c, Nil, Some(parent))
                        case targs =>
                            SIRType.TypeLambda(
                              c.typeParams,
                              SIRType.CaseClass(c, c.typeParams, Some(parent))
                            )
                    c.name -> sirType
                }
                .toMap[String, SIRType]
        _constrs.getOrElse(
          constrName,
          throw new IllegalArgumentException(
            s"Constructor for $constrName not found in $name"
          )
        )

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

    case class Var(name: String, tp: SIRType, anns: AnnotationsDecl) extends SIR {
        override def toString: String = s"Var($name, ${tp.show})"
    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType, anns: AnnotationsDecl)
        extends SIR {

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    case class Let(
        recursivity: Recursivity,
        bindings: List[Binding],
        body: SIR,
        anns: AnnotationsDecl
    ) extends SIR {
        override def tp: SIRType = body.tp
    }

    case class LamAbs(param: Var, term: SIR, anns: AnnotationsDecl) extends SIR {

        override def tp: SIRType =
            term.tp match
                case SIRType.TypeLambda(tvars, tpexpr) =>
                    SIRType.TypeLambda(tvars, SIRType.Fun(param.tp, tpexpr))
                case _ =>
                    SIRType.Fun(param.tp, term.tp)

    }

    case class Apply(f: SIR, arg: SIR, tp: SIRType, anns: AnnotationsDecl) extends SIR {

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

    case class Select(scrutinee: SIR, field: String, tp: SIRType, anns: AnnotationsDecl) extends SIR

    case class Const(uplcConst: Constant, tp: SIRType, anns: AnnotationsDecl) extends SIR

    case class And(a: SIR, b: SIR, anns: AnnotationsDecl) extends SIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Or(a: SIR, b: SIR, anns: AnnotationsDecl) extends SIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Not(a: SIR, anns: AnnotationsDecl) extends SIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class IfThenElse(cond: SIR, t: SIR, f: SIR, tp: SIRType, anns: AnnotationsDecl) extends SIR

    case class Builtin(bn: DefaultFun, tp: SIRType, anns: AnnotationsDecl) extends SIR

    case class Error(msg: String, anns: AnnotationsDecl, cause: Throwable | Null = null)
        extends SIR {
        override def tp: SIRType = SIRType.TypeNothing
    }

    // TODO: unify data-decl.
    case class Constr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ) extends SIR

    enum Pattern:
        case Constr(constr: ConstrDecl, bindings: List[String], typeBindings: List[SIRType])
        case Wildcard

    case class Case(
        pattern: Pattern,
        body: SIR
    )

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: SIR, cases: List[Case], tp: SIRType, anns: AnnotationsDecl)
        extends SIR

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
        checkParams
    }

    def checkTypeBinding(tpb: TypeBinding, throwOnFirst: Boolean): Seq[String] =
        checkType(tpb.tp, throwOnFirst)

    def checkAnnotations(decl: AnnotationsDecl, throwOnFirst: Boolean): Seq[String] = {
        (decl.data.flatMap { case (k, v) => checkExpr(v, throwOnFirst) }).toSeq
    }

    def checkExpr(expr: SIR, throwOnFirst: Boolean): Seq[String] = {
        val checkTp = checkType(expr.tp, throwOnFirst)
        expr match {
            case SIR.Var(_, tp, anns)            => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.ExternalVar(_, _, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Let(_, bindings, body, anns) =>
                val checkBindings = bindings.flatMap(b => checkExpr(b.value, throwOnFirst))
                checkBindings ++ checkSIR(body, throwOnFirst) ++ checkTp ++ checkAnnotations(
                  anns,
                  throwOnFirst
                )
            case SIR.LamAbs(param, term, anns) =>
                checkType(param.tp, throwOnFirst) ++ checkSIR(
                  term,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Apply(f, arg, tp, anns) =>
                checkSIR(f, throwOnFirst) ++ checkSIR(
                  arg,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Select(scrutinee, _, _, anns) =>
                checkSIR(scrutinee, throwOnFirst) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Const(_, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.And(a, b, anns) =>
                checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Or(a, b, anns) =>
                checkSIR(a, throwOnFirst) ++ checkSIR(b, throwOnFirst) ++ checkTp
            case SIR.Not(a, anns) => checkSIR(a, throwOnFirst) ++ checkTp
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                checkSIR(cond, throwOnFirst) ++ checkSIR(t, throwOnFirst) ++ checkSIR(
                  f,
                  throwOnFirst
                ) ++ checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Builtin(_, tp, anns) => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Error(_, anns, _)    => checkTp ++ checkAnnotations(anns, throwOnFirst)
            case SIR.Constr(_, data, args, tp1, anns) =>
                val checkArgs = args.flatMap(a => checkSIR(a, throwOnFirst))
                val checkTp1 = checkType(tp1, throwOnFirst)
                checkData(data, throwOnFirst) ++ checkArgs ++ checkTp ++ checkTp1 ++
                    checkAnnotations(anns, throwOnFirst)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                val checkCases = cases.flatMap(c => checkCase(c, throwOnFirst))
                checkSIR(scrutinee, throwOnFirst) ++ checkCases ++ checkTp ++ checkAnnotations(
                  anns,
                  throwOnFirst
                )
            case SIR.Decl(data, term) =>
                checkData(data, throwOnFirst) ++ checkSIR(term, throwOnFirst)
        }
    }

    def checkCase(c: SIR.Case, throwOnFirst: Boolean): Seq[String] = {
        c.pattern match
            case SIR.Pattern.Constr(constr, bindings, typeBindings) =>
                typeBindings.flatMap(x => checkType(x, throwOnFirst)) ++
                    checkConstr(constr, throwOnFirst) ++ checkSIR(c.body, throwOnFirst)
            case SIR.Pattern.Wildcard => checkSIR(c.body, throwOnFirst)
    }

    def checkType(tp: SIRType, throwOnFirst: Boolean): Seq[String] =
        if (SIRType.checkAllProxiesFilled(tp)) then Nil
        else
            val msg = s"Type has unfilled proxies. (tp=${tp})"
            if throwOnFirst then throw CheckException(msg)
            else Seq(msg)

}
