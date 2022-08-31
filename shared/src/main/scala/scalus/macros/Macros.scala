package scalus.macros

import scalus.uplc.ExprBuilder.vr
import scalus.uplc.{Expr as Exp, Term as Trm}

import scala.quoted.*
object Macros {
  def lamMacro[A: Type, B: Type](f: Expr[Exp[A] => Exp[B]])(using Quotes): Expr[Exp[A => B]] =
    import quotes.reflect.*
    val name = f.asTerm match
      // lam(x => body)
      case Inlined(_, _, Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _)) =>
        Expr(name)
      // lam { x => body }
      case Inlined(
            _,
            _,
            Block(List(), Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _))
          ) =>
        Expr(name)
      case x => report.errorAndAbort(x.toString)
    '{
      Exp(Trm.LamAbs($name, $f(vr($name)).term))
    }
}
