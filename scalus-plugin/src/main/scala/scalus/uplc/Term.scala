package scalus.uplc

import scalus.sir.SIR
import scalus.uplc.Data.*

import java.util
import scala.collection.immutable

case class NamedDeBruijn(name: String, index: Int = 0):
  assert(index >= 0)
  override def toString: String =
    if index == 0 then s"NamedDeBruijn(\"$name\")"
    else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
  case Var(name: NamedDeBruijn) extends Term
  case LamAbs(name: String, term: Term) extends Term
  case Apply(f: Term, arg: Term) extends Term
  case Force(term: Term) extends Term
  case Delay(term: Term) extends Term
  case Const(const: Constant) extends Term
  case Builtin(bn: DefaultFun) extends Term
  case Error(msg: String) extends Term
