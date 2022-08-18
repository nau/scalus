package scalus.uplc

import scalus.uplc.Constant.LiftValue

import scala.annotation.targetName

trait Delay[+A]
case class Expr[+A](term: Term)

object ExprBuilder:
  import TermDSL.*

  given liftableToExpr[A: LiftValue]: Conversion[A, Expr[A]] = const

  def const[A: LiftValue](a: A): Expr[A] = Expr(Term.Const(summon[LiftValue[A]].lift(a)))
  def vr[A](name: String): Expr[A] = Expr(Term.Var(name))
  def app[A, B](f: Expr[A => B], x: Expr[B]): Expr[A] = Expr(Term.Apply(f.term, x.term))
  def lam[A](name: String): [B] => (Expr[A] => Expr[B]) => Expr[A => B] = [B] =>
    (f: Expr[A] => Expr[B]) => Expr(Term.LamAbs(name, f(vr(name)).term))
  def delay[A](x: Expr[A]): Expr[Delay[A]] = Expr(~x.term)
  def force[A](x: Expr[Delay[A]]): Expr[A] = Expr(!x.term)
  def error: Expr[Delay[Nothing]] = Expr(~Term.Error)
  def ifThenElse[A](cond: Expr[Boolean], t: Expr[Delay[A]], f: Expr[Delay[A]]): Expr[Delay[A]] =
    Expr(!Term.Builtin(DefaultFun.IfThenElse) $ cond.term $ t.term $ f.term)
  val unConstrData: Expr[Data => (BigInt, List[Data])] = Expr(Term.Builtin(DefaultFun.UnConstrData))
  val unListData: Expr[Data => List[Data]] = Expr(Term.Builtin(DefaultFun.UnListData))

  def fstPair[A, B](x: Expr[(A, B)]): Expr[A] = Expr(
    Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.FstPair))), x.term)
  )
  def sndPair[A, B](x: Expr[(A, B)]): Expr[B] = Expr(
    Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.SndPair))), x.term)
  )

  val headList: Expr[List[Data] => Data] = Expr(
    Term.Force(Term.Builtin(DefaultFun.HeadList))
  )

  val tailList: Expr[List[Data] => List[Data]] = Expr(
    Term.Force(Term.Builtin(DefaultFun.TailList))
  )

  val nullList: Expr[List[Data] => Boolean] = Expr(
    Term.Force(Term.Builtin(DefaultFun.NullList))
  )

  def addInteger(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = Expr(
    Term.Builtin(DefaultFun.AddInteger) $ x.term $ y.term
  )

  extension (lhs: Expr[BigInt])
    @targetName("plus")
    def |+|(rhs: Expr[BigInt]): Expr[BigInt] = addInteger(lhs, rhs)

  extension [A, B](lhs: Expr[A => B]) def apply(rhs: Expr[A]): Expr[B] = app(lhs, rhs)

object Example:
  import Constant.given
  import ExprBuilder.{*, given}
  // simple validator that checks that the spending transaction has no outputs
  // it's a gift to the validators community
  val validator: Expr[Unit => Unit => Data => Unit] = lam[Unit]("redeemer") { _ =>
    lam[Unit]("datum") { _ =>
      lam[Data]("ctx") { ctx =>
        // ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
        val scriptContext = unConstrData(ctx)
        // ScriptContext args
        val ctxArgs = sndPair(scriptContext)
        // second in the list
        val txInfo = unConstrData(headList(ctxArgs))
        val txInfoArgs = sndPair(txInfo)
        val txInfoOutputs = headList(tailList(tailList(txInfoArgs)))
        val isTxInfoOutputsEmpty = nullList(unListData(txInfoOutputs))
        val result = ifThenElse(isTxInfoOutputsEmpty, delay(const(())), error)
        force(result)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(validator.term.pretty.render(80))
  }
