package scalus.sir

import scalus.sir.SIRType.FreeUnificator
import scalus.uplc.Constant
import scalus.uplc.DefaultFun

case class Module(version: (Int, Int), defs: List[Binding])

case class Binding(name: String, value: SIRExpr) {

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

    val tp: SIRType = typeParams match
                case Nil => SIRType.CaseClass(this, typeParams)
                case tp :: tail => SIRType.TypeLambda(typeParams, SIRType.CaseClass(this, typeParams))
    

}

case class DataDecl(
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar]
    /*constructorTypeMapping: Map[String, List[SIRType]]*/
) {
    
    val tp: SIRType = typeParams match
                    case Nil => SIRType.SumCaseClass(this, Nil)
                    case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))

}

//case class ExternalDataDecl(module: String, name: String)

sealed trait SIR {

    def ~=~(that: SIR): Boolean =
        SIRUnify.unifySIR(this, that, SIRUnify.Env.empty) match {
            case SIRUnify.UnificationSuccess(_, _) => true
            case SIRUnify.UnificationFailure(_,_,_)  => false
        }

}

sealed trait SIRExpr extends SIR {
    def tp: SIRType
}

sealed trait SIRDef extends SIR

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

    case class Var(name: String, tp: SIRType) extends SIRExpr {
        override def toString: String = s"Var($name, ${tp.show})"
    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType) extends SIRExpr {

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    case class Let(recursivity: Recursivity, bindings: List[Binding], body: SIRExpr)
        extends SIRExpr {
        override def tp: SIRType = body.tp
    }

    case class LamAbs(param: Var, term: SIRExpr) extends SIRExpr {

        override def tp: SIRType =
            term.tp match
                case SIRType.TypeLambda(tvars, tpexpr) =>
                    SIRType.TypeLambda(tvars, SIRType.Fun(param.tp, tpexpr))
                case _ =>
                    SIRType.Fun(param.tp, term.tp)

    }


    case class Apply(f: SIRExpr, arg: SIRExpr, tp: SIRType) extends SIRExpr {

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

    case class Const(uplcConst: Constant, tp: SIRType) extends SIRExpr


    case class And(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }


    case class Or(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }

    case class Not(a: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive
    }

    case class IfThenElse(cond: SIRExpr, t: SIRExpr, f: SIRExpr, tp: SIRType) extends SIRExpr


    case class Builtin(bn: DefaultFun, tp: SIRType) extends SIRExpr


    case class Error(msg: String, cause: Throwable | Null = null) extends SIRExpr {
        override def tp: SIRType = SIRType.TypeError(msg, cause)
    }


    // TODO: unify data-decl.
    case class Constr(name: String, data: DataDecl, args: List[SIRExpr]) extends SIRExpr {
        override def tp: SIRType = data.tp
    }

    case class Case(
        constr: ConstrDecl,
        bindings: List[String],
        typeBindings: List[SIRType],
        body: SIRExpr
    )

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: SIRExpr, cases: List[Case], tp: SIRType) extends SIRExpr

    case class Hole(name: String, override val tp: SIRType = FreeUnificator) extends SIRExpr

    case class Decl(data: DataDecl, term: SIR) extends SIRDef


case class Program(version: (Int, Int, Int), term: SIR)
