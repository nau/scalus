package scalus.uplc

import scalus.uplc.Constant.LiftValue

trait Delay[+A]
case class Expr[+A](term: Term)

object ExprBuilder:
  import TermDSL.*
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
  def unConstrData(x: Expr[Data]): Expr[(BigInt, List[Data])] = Expr(
    Term.Builtin(DefaultFun.UnConstrData) $ x.term
  )
  def unListData(x: Expr[Data]): Expr[List[Data]] = Expr(
    Term.Builtin(DefaultFun.UnListData) $ x.term
  )

  def fstPair[A, B](x: Expr[(A, B)]): Expr[A] = Expr(
    Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.FstPair))), x.term)
  )
  def sndPair[A, B](x: Expr[(A, B)]): Expr[B] = Expr(
    Term.Apply(Term.Force(Term.Force(Term.Builtin(DefaultFun.SndPair))), x.term)
  )
  def headList(x: Expr[List[Data]]): Expr[Data] = Expr(
    Term.Apply(Term.Force(Term.Builtin(DefaultFun.HeadList)), x.term)
  )

  def tailList(x: Expr[List[Data]]): Expr[List[Data]] = Expr(
    Term.Apply(Term.Force(Term.Builtin(DefaultFun.TailList)), x.term)
  )

  def nullList(x: Expr[List[Data]]): Expr[Boolean] = Expr(
    Term.Apply(Term.Force(Term.Builtin(DefaultFun.NullList)), x.term)
  )

object Example:
  import ExprBuilder.*
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
