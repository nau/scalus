package scalus.sir

import scalus.uplc.Constant.Integer
import scalus.uplc.{Constant, DefaultFun, DefaultUni}

val SIRVersion: (Int, Int) = (3, 0)

case class Module(version: (Int, Int), name: String, defs: List[Binding])

case class Binding(name: String, tp: SIRType, value: SIR) {

    override def toString: String = s"Binding(\"$name\" [${tp.show}] : $value)"

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
    endColumn: Int,
    
) {

    def show: String = {
        s"$file:${startLine + 1}:${startColumn} - ${endLine + 1}:${endColumn}"
    }

    def isEmpty: Boolean = file.isEmpty && startLine == 0 && startColumn == 0 &&
        endLine == 0 && endColumn == 0

}

object SIRPosition {

    val empty: SIRPosition = SIRPosition("", 0, 0, 0, 0)

}

case class AnnotationsDecl(
    pos: SIRPosition,
    comment: Option[String] = None,
    data: Map[String, SIR] = Map.empty
) {

    def ++(data: Map[String, SIR]): AnnotationsDecl = {
        AnnotationsDecl(
          pos = pos,
          comment = comment,
          data = this.data ++ data
        )
    }

}

case object AnnotationsDecl {
    import scala.quoted.*

    inline def empty: AnnotationsDecl = ${ SIRMacro.emptyAnnotationsDeclImpl }

}

case class ConstrDecl(
    /** Name (usually - full name of symbol, i.s. scalus.prelude.List$.Nit )
      */
    name: String,

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

    if name == "scalus.prelude.List$.Nil" then
        if parentTypeArgs.isEmpty then
            throw new RuntimeException(
              "Nil constructor must have parent type arguments. (name=" + name + ")"
            )
        else if parentTypeArgs.size != 1 then
            throw new RuntimeException(
              "Nil constructor must have exactly one parent type argument. (name=" + name + ")"
            )

    if name == "scalus.prelude.Option$.Some" then
        if parentTypeArgs.isEmpty then
            throw new RuntimeException(
              "Some constructor must have parent type arguments. (name=" + name + ")"
            )
        else if parentTypeArgs.size != 1 then
            throw new RuntimeException(
              "Some constructor must have exactly one parent type argument. (name=" + name + ")"
            )

    if name == "scala.Tuple3" then
        if params.head.name == "t1" then
            throw new RuntimeException(
              "Tuple3 constructor must have parameters _1, _2, _3. (name=" + name + ")"
            )
        if parentTypeArgs.nonEmpty then
            throw new RuntimeException(
              "Tuple3 constructor must not have parent type arguments. (name=" + name + ")"
            )

}

//  Data~ Lost(Const)

//  A -> B -> C1, C2
//    -> B1 -> D

//f:  B => Int   B = Data(C1,C2):     \lambda (c1,c2) => c2(x)
//f:  A => Int   A = Data(C1, C2, D)

case class DataDecl(
    /** Name of the data type. (full name of the symbol)
      */
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar],
    annotations: AnnotationsDecl
) {

    if constructors.isEmpty then
        throw new RuntimeException(
          s"Data declaration must have at least one constructor. (name=$name)"
        )
    // if (name == "")

    private var _tp: SIRType = null
    private var _constrs: Map[String, SIRType] = null

    def tp: SIRType =
        if _tp == null then
            _tp = typeParams match
                case Nil => SIRType.SumCaseClass(this, Nil)
                case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))
        _tp

    def constrType(constrName: String): SIRType = {
        if _constrs == null then {
            val nConstrs = constructors.length
            _constrs = constructors
                .map { c =>
                    val parent = typeParams match
                        case Nil => tp
                        case typeParams =>
                            val tpEnv = typeParams.zip(c.parentTypeArgs).toMap
                            SIRType.substitute(tp, tpEnv, Map.empty)
                    val optParent = if nConstrs == 1 then None else Some(parent)
                    val sirType = c.typeParams match
                        case Nil => SIRType.CaseClass(c, Nil, optParent)
                        case targs =>
                            SIRType.TypeLambda(
                              c.typeParams,
                              SIRType.CaseClass(c, c.typeParams, optParent)
                            )
                    c.name -> sirType
                }
                .toMap[String, SIRType]
        }
        _constrs.getOrElse(
          constrName,
          throw new IllegalArgumentException(
            s"Constructor for $constrName not found in $name"
          )
        )

    }
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

sealed trait AnnotatedSIR extends SIR {

    def anns: AnnotationsDecl

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

    case class Var(name: String, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR {

        override def toString: String = s"Var($name, ${tp.show})"
    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR {

        if moduleName == "scalus.prelude" then
            throw new RuntimeException(
              s"ExternalVar: scalus.prelude, moduleName ${moduleName}, name=${name} at " + anns.pos.show
            )

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    case class Let(
        recursivity: Recursivity,
        bindings: List[Binding],
        body: SIR,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR {

        override def tp: SIRType = body.tp
    }

    case class LamAbs(
        param: Var,
        term: SIR,
        typeParams: List[SIRType.TypeVar],
        anns: AnnotationsDecl
    ) extends AnnotatedSIR {

        override def tp: SIRType = {
            if typeParams.isEmpty then SIRType.Fun(param.tp, term.tp)
            else SIRType.TypeLambda(typeParams, SIRType.Fun(param.tp, term.tp))
        }

    }

    case class Apply(f: AnnotatedSIR, arg: AnnotatedSIR, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR {

        if f.tp == SIRType.Unit then
            throw new RuntimeException(
              s"Apply: f is Unit, cannot apply to Unit at ${anns.pos.show}.\n" +
                  s"f: $f"
            )

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

    case class Select(scrutinee: SIR, field: String, tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR

    case class Const(uplcConst: Constant, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR

    object Const {
        def boolean(value: Boolean, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Bool(value), SIRType.Boolean, anns)
        }

        def bool(value: Boolean, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            boolean(value, anns)
        }

        def integer(value: BigInt, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Integer(value), SIRType.Integer, anns)
        }

        def string(value: String, anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.String(value), SIRType.String, anns)
        }

        def unit(anns: AnnotationsDecl = AnnotationsDecl.empty): Const = {
            Const(Constant.Unit, SIRType.Unit, anns)
        }
    }

    case class And(a: AnnotatedSIR, b: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Or(a: AnnotatedSIR, b: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class Not(a: AnnotatedSIR, anns: AnnotationsDecl) extends AnnotatedSIR {
        override def tp: SIRType = SIRType.Boolean
    }

    case class IfThenElse(
        cond: AnnotatedSIR,
        t: AnnotatedSIR,
        f: AnnotatedSIR,
        tp: SIRType,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR

    case class Builtin(bn: DefaultFun, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR

    case class Error(msg: String, anns: AnnotationsDecl, cause: Throwable | Null = null)
        extends AnnotatedSIR {
        override def tp: SIRType = SIRType.TypeNothing
    }

    // TODO: unify data-decl.
    case class Constr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ) extends AnnotatedSIR

    sealed trait Pattern {
        def show: String = this match {
            case Pattern.Constr(constr, bindings, typeParamsBindings) =>
                s"${constr.name}, ${bindings.mkString(", ")}"
            case Pattern.Wildcard => "_"
        }
    }

    object Pattern:
        case class Constr(
            constr: ConstrDecl,
            bindings: List[String], // TODO: add wildcard as a special case.
            typeParamsBindings: List[SIRType]
        ) extends Pattern
        case object Wildcard extends Pattern

    case class Case(
        pattern: Pattern,
        body: SIR,
        anns: AnnotationsDecl
    )

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: AnnotatedSIR, cases: List[Case], tp: SIRType, anns: AnnotationsDecl)
        extends AnnotatedSIR

    case class Cast(term: AnnotatedSIR, tp: SIRType, anns: AnnotationsDecl) extends AnnotatedSIR {

        if anns.pos.file.contains("GeneratingUnscopedVarsTest") then
            tp match
                case SIRType.Fun(SIRType.Integer, tuple2Constr) =>
                    tuple2Constr match
                        case SIRType.CaseClass(constr, List(SIRType.Integer, SIRType.Integer), _)
                            if constr.name == "scala.Tuple2" =>
                            throw RuntimeException("QQQ")
                        case _ =>
                case _ =>

    }

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
        decl.data.flatMap { case (k, v) => checkExpr(v, throwOnFirst) }.toSeq
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
            case SIR.LamAbs(param, term, typeParams, anns) =>
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
            case SIR.Cast(term, tp, anns) =>
                checkSIR(term, throwOnFirst) ++ checkType(tp, throwOnFirst) ++
                    checkAnnotations(anns, throwOnFirst)
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
        if SIRType.checkAllProxiesFilled(tp) then Nil
        else
            val msg = s"Type has unfilled proxies. (tp=${tp})"
            if throwOnFirst then throw CheckException(msg)
            else Seq(msg)

}
