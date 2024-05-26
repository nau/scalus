package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun

case class Module(version: (Int, Int), defs: List[Binding])

case class Binding(name: String, value: SIRExpr) {

    override def toString: String = s"Binding(\"$name\" : ${tp.show},  $value)"
}

case class TypeBinding(name: String, tp: SIRType) {

    override def toString: String = s"TypeBinding(\"$name\" : ${value.show})"
}

enum Recursivity:
    case NonRec, Rec


case class ConstrDecl(name: String, storageType: SIRVarStorage, params: List[TypeBinding])
case class DataDecl(name: String, storageType: SIRVarStorage, constructors: List[ConstrDecl])

sealed trait SIR

sealed trait SIRExpr extends SIR {
    def tp: SIRType
}

sealed trait SIRDef extends SIR

object SIR:

    case class Var(name: String, tp: SIRType) extends SIRExpr  //TODO: add sieStorage parameter.
    case class ExternalVar(moduleName: String, name: String, tp: SIRType) extends SIRExpr
    case class Let(recursivity: Recursivity, bindings: List[Binding], body: SIRExpr) extends SIRExpr
    case class LamAbs(param: Var, term: SIRExpr) extends SIRExpr
    case class Apply(f: SIRExpr, arg: SIRExpr) extends SIRExpr
    case class Const(uplcConst: Constant, tp: SIRType) extends SIRExpr
    case class And(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BoolPrimitive
    }
    case class Or(a: SIRExpr, b: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BoolPrimitive
    }
    case class Not(a: SIRExpr) extends SIRExpr {
        override def tp: SIRType = SIRType.BoolPrimitive
    }

    case class IfThenElse(cond: SIRExpr, t: SIRExpr, f: SIRExpr, tp: SIRType) extends SIRExpr
    case class Builtin(bn: DefaultFun, tp: SIRType) extends SIRExpr
    case class Error(msg: String) extends SIRExpr
    case class Constr(name: String, data: DataDecl, args: List[SIRExpr]) extends SIRExpr {
        override def tp: SIRType = data.tp
    }

    case class Case(constr: ConstrDecl, bindings: List[String], body: SIRExpr)

    /**
     * Match expression.  
     * @param scrutinee
     * @param cases
     * @param tp - resulting type of Match expression, can be calculated as max(tp of all cases)
     */
    case class Match(scrutinee: SIRExpr, cases: List[Case], tp: SIRType[?])

    case class Decl(data: DataDecl, term: SIRExpr) extends SIRDef


case class Program(version: (Int, Int, Int), term: SIR)
