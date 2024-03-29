package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun

case class Module(version: (Int, Int), defs: List[Binding])
case class Binding(name: String, value: SIR) {
    override def toString: String = s"Binding(\"$name\", $value)"
}

enum Recursivity:
    case NonRec, Rec

case class ConstrDecl(name: String, params: List[String])
case class DataDecl(name: String, constructors: List[ConstrDecl])
case class Case(constr: ConstrDecl, bindings: List[String], body: SIR)

enum SIR:
    case Var(name: String) extends SIR
    case ExternalVar(moduleName: String, name: String) extends SIR
    case Let(recursivity: Recursivity, bindings: List[Binding], body: SIR) extends SIR
    case LamAbs(name: String, term: SIR) extends SIR
    case Apply(f: SIR, arg: SIR) extends SIR
    case Const(const: Constant) extends SIR
    case And(a: SIR, b: SIR) extends SIR
    case Or(a: SIR, b: SIR) extends SIR
    case Not(a: SIR) extends SIR
    case IfThenElse(cond: SIR, t: SIR, f: SIR) extends SIR
    case Builtin(bn: DefaultFun) extends SIR
    case Error(msg: String) extends SIR
    case Decl(data: DataDecl, term: SIR) extends SIR
    case Constr(name: String, data: DataDecl, args: List[SIR]) extends SIR
    case Match(scrutinee: SIR, cases: List[Case]) extends SIR

case class Program(version: (Int, Int, Int), term: SIR)
