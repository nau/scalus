package scalus.uplc

import scalus.ledger.api.v1.PubKeyHash
import scalus.macros.Macros
import scalus.uplc.Constant.LiftValue
import scalus.utils.Utils.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.annotation.targetName

trait Delayed[+A]
case class Expr[+A](term: Term)

object ExprBuilder:
  import TermDSL.*

  given liftableToExpr[A: LiftValue]: Conversion[A, Expr[A]] = const

  def const[A: LiftValue](a: A): Expr[A] = Expr(Term.Const(summon[LiftValue[A]].lift(a)))
  def vr[A](name: String): Expr[A] = Expr(Term.Var(name))
  def app[A, B](f: Expr[A => B], x: Expr[B]): Expr[A] = Expr(Term.Apply(f.term, x.term))
  def lam[A](name: String): [B] => (Expr[A] => Expr[B]) => Expr[A => B] = [B] =>
    (f: Expr[A] => Expr[B]) => Expr(Term.LamAbs(name, f(vr(name)).term))
  inline def lam[A, B](inline f: Expr[A] => Expr[B]): Expr[A => B] = ${ Macros.lamMacro('f) }
  def delay[A](x: Expr[A]): Expr[Delayed[A]] = Expr(Term.Delay(x.term))
  def force[A](x: Expr[Delayed[A]]): Expr[A] = Expr(Term.Force(x.term))
  def error: Expr[Delayed[Nothing]] = Expr(Term.Delay(Term.Error))

  // Z Combinator
  def z[A, B](f: Expr[(A => B) => A => B]): Expr[A => B] =
    // (lam ff [(lam xx [ff (lam vv [xx xx vv])]) (lam xx [ff (lam vv [xx xx vv])])])
    val Z: Term = λ("ff") {
      val zz = λ("xx")(Term.Var("ff") $ λ("vv")(Term.Var("xx") $ Term.Var("xx") $ Term.Var("vv")))
      zz $ zz
    }

    // app(ZCombinator, lam(funcName, r))
    Expr(Z $ f.term)

  def rec[A, B](f: Expr[A => B] => Expr[A => B]): Expr[A => B] =
    z(lam[A => B]("self")(self => f.apply(self)))

  def ifThenElse[A](cond: Expr[Boolean])(t: Expr[Delayed[A]])(f: Expr[Delayed[A]]): Expr[Delayed[A]] =
    Expr(Term.Force(Term.Builtin(DefaultFun.IfThenElse)) $ cond.term $ t.term $ f.term)
  val unConstrData: Expr[Data => (BigInt, List[Data])] = Expr(Term.Builtin(DefaultFun.UnConstrData))
  val unListData: Expr[Data => List[Data]] = Expr(Term.Builtin(DefaultFun.UnListData))
  val unBData: Expr[Data => Array[Byte]] = Expr(Term.Builtin(DefaultFun.UnBData))

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

  def chooseList[A, B](ls: Expr[List[A]])(e: Expr[B])(ne: Expr[B]): Expr[B] = Expr(
    Term.Force(Term.Force(Term.Builtin(DefaultFun.ChooseList))) $ ls.term $ e.term $ ne.term
  )

  def addInteger(x: Expr[BigInt], y: Expr[BigInt]): Expr[BigInt] = Expr(
    Term.Builtin(DefaultFun.AddInteger) $ x.term $ y.term
  )

  val lessThanEqualsInteger: Expr[BigInt => BigInt => Boolean] = Expr(
    Term.Builtin(DefaultFun.LessThanEqualsInteger)
  )

  val equalsByteString: Expr[Array[Byte] => Array[Byte] => Boolean] = Expr(
    Term.Builtin(DefaultFun.EqualsByteString)
  )

  extension (lhs: Expr[BigInt])
    @targetName("plus")
    def |+|(rhs: Expr[BigInt]): Expr[BigInt] = addInteger(lhs, rhs)
    def <=(rhs: Expr[BigInt]): Expr[Boolean] = lessThanEqualsInteger(lhs)(rhs)

  extension (lhs: Expr[Array[Byte]])
    def ===(rhs: Expr[Array[Byte]]): Expr[Boolean] = equalsByteString(lhs)(rhs)

  extension [A, B](lhs: Expr[A => B]) def apply(rhs: Expr[A]): Expr[B] = app(lhs, rhs)
  extension [A](lhs: Expr[A]) def unary_~ : Expr[Delayed[A]] = delay(lhs)
  extension [A](lhs: Expr[Delayed[A]]) def unary_! : Expr[A] = force(lhs)

  def uplcToFlat(program: String): Array[Byte] =
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc convert --of flat"
    val outStream = new ByteArrayOutputStream()
    cmd.#<(new ByteArrayInputStream(program.getBytes("UTF-8"))).#>(outStream).!
    outStream.toByteArray

object Example:
  import Constant.given
  import ExprBuilder.{*, given}
  // simple validator that checks that the spending transaction has no outputs
  // it's a gift to the validators community
  val giftValidator: Expr[Unit => Unit => Data => Unit] = lam { redeemer =>
    lam { datum =>
      lam { ctx =>
        // ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
        val scriptContext = unConstrData(ctx)
        // ScriptContext args
        val ctxArgs = sndPair(scriptContext)
        // second in the list
        val txInfo = unConstrData(headList(ctxArgs))
        val txInfoArgs = sndPair(txInfo)
        val txInfoOutputs = headList(tailList(tailList(txInfoArgs)))
        val isTxInfoOutputsEmpty = nullList(unListData(txInfoOutputs))
        val result = ifThenElse(isTxInfoOutputsEmpty)(~())(error)
        !result
      }
    }
  }

  /// PubKey style validator. Checks whether the transaction has a specific signature
  def pubKeyValidator(pkh: PubKeyHash): Expr[Unit => Unit => Data => Unit] =
    lam { redeemer =>
      lam { datum =>
        lam { ctx =>
          // ctx.scriptContextTxInfo.txInfo
          val txInfoArgs: Expr[List[Data]] =
            sndPair(unConstrData(headList(sndPair(unConstrData(ctx)))))
          val txInfoSignatories: Expr[List[Data]] = unListData(
            headList(
              tailList(tailList(tailList(tailList(tailList(tailList(tailList(txInfoArgs)))))))
            )
          )

          val search = rec[List[Data], Unit] { self =>
            lam { signatories =>
              // signatories.head.pubKeyHash
              val headPubKeyHash = unBData(headList(sndPair(unConstrData(headList(signatories)))))
              !(!chooseList(signatories)(error) {
                ~ifThenElse(headPubKeyHash === pkh.hash)(~()) { ~self(tailList(signatories)) }
              })
            }
          }
          search(txInfoSignatories)
        }
      }
    }

  def main(args: Array[String]): Unit = {
//    println(giftValidator.term.pretty.render(80))
    val pubKeyProgram =
      Program((1, 0, 0), pubKeyValidator(PubKeyHash(hex"deadbeef")).term).pretty.render(80)
    println(pubKeyProgram)
    val flat = uplcToFlat(pubKeyProgram)
    println(s"${flat.length} ${bytesToHex(flat)}")

    /*val asdf = rec[BigInt, BigInt](self =>
      lam[BigInt]("x") { x =>
        !ifThenElse(x <= BigInt(0), ~self(x |+| const(BigInt(1))), ~const(BigInt(123)))
      }
    )

    println(Cek.evalUPLC(asdf(BigInt(-3)).term).pretty.render(80))*/
  }

  @main def testLam2() =
    println(
      lam[BigInt, BigInt](x => x |+| BigInt(1))(BigInt(2)).term.pretty.render(80)
    )

    println(
      lam((x: Expr[BigInt]) => x |+| BigInt(1))(BigInt(2)).term.pretty.render(80)
    )

    println(
      lam(x => BigInt(1))(BigInt(2)).term.pretty.render(80)
    )
