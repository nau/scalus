package scalus.sir

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

    def tp: SIRType =
        typeParams match
            case Nil        => SIRType.CaseClass(this, typeParams)
            case tp :: tail => SIRType.TypeLambda(typeParams, SIRType.CaseClass(this, typeParams))

}

case class DataDecl(
    name: String,
    constructors: List[ConstrDecl],
    typeParams: List[SIRType.TypeVar]
    /*constructorTypeMapping: Map[String, List[SIRType]]*/
) {

    def tp: SIRType =
        typeParams match
            case Nil => SIRType.SumCaseClass(this, Nil)
            case _   => SIRType.TypeLambda(typeParams, SIRType.SumCaseClass(this, typeParams))

}

case class ExternalDataDecl(module: String, name: String)

sealed trait SIR {

    def unifyEq(that: SIR): Boolean

    def ~=~(that: SIR): Boolean = unifyEq(that)

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

        override def unifyEq(that: SIR): Boolean = that match {
            case Var(name2, tp2) => name == name2 && tp ~=~ tp2
            case _ => false
        }

    }

    case class ExternalVar(moduleName: String, name: String, tp: SIRType) extends SIRExpr {

        override def unifyEq(that: SIR): Boolean = that match {
            case ExternalVar(moduleName2, name2, tp2) => moduleName == moduleName2 && name == name2 && tp ~=~ tp2
            case _ => false
        }

        override def toString: String = s"ExternalVar($moduleName, $name, ${tp.show})"

    }

    case class Let(recursivity: Recursivity, bindings: List[Binding], body: SIRExpr)
        extends SIRExpr {
        override def tp: SIRType = body.tp
        override def unifyEq(that: SIR): Boolean = that match {
            case Let(recursivity2, bindings2, body2) =>
                recursivity == recursivity2 && bindings == bindings2 && body ~=~ body2
            case _ => false
        }
    }

    case class LamAbs(param: Var, term: SIRExpr) extends SIRExpr {

        override def tp: SIRType =
            term.tp match
                case SIRType.TypeLambda(tvars, tpexpr) =>
                    SIRType.TypeLambda(tvars, SIRType.Fun(param.tp, tpexpr))
                case _ =>
                    SIRType.Fun(param.tp, term.tp)

        override def unifyEq(that: SIR): Boolean = that match {
            case LamAbs(param2, term2) => param ~=~ param2 && term ~=~ term2
            case _ => false
        }

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

        override def unifyEq(that: SIR): Boolean = that match {
            case Apply(f2, arg2, tp2) => f ~=~ f2 && arg ~=~ arg2 && tp ~=~ tp2
            case _ => false
        }

    }

    case class Const(uplcConst: Constant, tp: SIRType) extends SIRExpr {

        override def unifyEq(that: SIR): Boolean = that match {
            case Const(uplcConst2, tp2) => uplcConst == uplcConst2 && tp ~=~ tp2
            case _ => false
        }

    }


    case class And(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive

        override def unifyEq(that: SIR): Boolean = that match {
            case And(a2, b2) => a ~=~ a2 && b ~=~ b2
            case _ => false
        }

    }


    case class Or(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive

        override def unifyEq(that: SIR): Boolean = that match {
            case Or(a2, b2) => a ~=~ a2 && b ~=~ b2
            case _ => false
        }

    }

    case class Not(a: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BooleanPrimitive

        override def unifyEq(that: SIR): Boolean = that match {
            case Not(a2) => a ~=~ a2
            case _ => false
        }
    }

    case class IfThenElse(cond: SIRExpr, t: SIRExpr, f: SIRExpr, tp: SIRType) extends SIRExpr {

        override def unifyEq(that: SIR): Boolean = that match {
            case IfThenElse(cond2, t2, f2, tp2) => cond ~=~ cond2 && t ~=~ t2 && f ~=~ f2 && tp ~=~ tp2
            case _ => false
        }

    }


    case class Builtin(bn: DefaultFun, tp: SIRType) extends SIRExpr {

        override def unifyEq(that: SIR): Boolean = that match {
            case Builtin(bn2, tp2) => bn == bn2 && tp ~=~ tp2
            case _ => false
        }

    }


    case class Error(msg: String, cause: Throwable | Null = null) extends SIRExpr {
        override def tp: SIRType = SIRType.TypeError(msg, cause)

        override def unifyEq(that: SIR): Boolean = that match {
            case Error(msg2, cause2) => msg == msg2
            case _ => false
        }

    }



    case class Constr(name: String, data: DataDecl, args: List[SIRExpr]) extends SIRExpr {

        override def tp: SIRType = data.tp

        override def unifyEq(that: SIR): Boolean = that match {
            case Constr(name2, data2, args2) => name == name2 && data == data2 && args == args2
            case _ => false
        }

    }

    case class Case(
        constr: ConstrDecl,
        bindings: List[String],
        typeBindings: List[SIRType],
        body: SIRExpr
    ) {

        def unifyEq(that: Case): Boolean = that match {
            case Case(constr2, bindings2, typeBindings2, body2) =>
                constr == constr2 && bindings == bindings2 &&
                    typeBindings.zip(typeBindings2).forall((t,t2)=>t ~=~ t2) &&
                    typeBindings.length == typeBindings2.length &&
                    body ~=~ body2
            case _ => false
        }

        def ~=~(that: Case): Boolean = unifyEq(that)

    }

    /** Match expression.
      * @param scrutinee
      * @param cases
      * @param tp
      *   \- resulting type of Match expression, can be calculated as max(tp of all cases)
      */
    case class Match(scrutinee: SIRExpr, cases: List[Case], tp: SIRType) extends SIRExpr {

        override def unifyEq(that: SIR): Boolean = that match {
            case Match(scrutinee2, cases2, tp2) =>
                scrutinee ~=~ scrutinee2 && cases.zip(cases2).forall((c1,c2)=>c1 ~=~ c2) && tp ~=~ tp2
            case _ => false
        }

    }

    case class Decl(data: DataDecl, term: SIR) extends SIRDef {

        override def unifyEq(that: SIR): Boolean = that match {
            case Decl(data2, term2) => data == data2 && term ~=~ term2
            case _ => false
        }

    }

case class Program(version: (Int, Int, Int), term: SIR)
