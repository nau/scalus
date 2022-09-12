package scalus.uplc

import scalus.ledger.api.v1.*
import scalus.macros.Macros
import scalus.uplc.Constant.LiftValue
import scalus.utils.Utils.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import scala.annotation.targetName

trait Delayed[+A]
case class Expr[+A](term: Term)

object ExprBuilder:
  import TermDSL.*

  trait Unlift[A]:
    def unlift: Expr[Data => A]

  given Unlift[BigInt] with {
    def unlift = unIData
  }

  given Unlift[Array[Byte]] with {
    def unlift = unBData
  }

  given liftableToExpr[A: LiftValue]: Conversion[A, Expr[A]] = const

  def const[A: LiftValue](a: A): Expr[A] = Expr(Term.Const(summon[LiftValue[A]].lift(a)))
  def vr[A](name: String): Expr[A] = Expr(Term.Var(name))
  def app[A, B](f: Expr[A => B], x: Expr[A]): Expr[B] = Expr(Term.Apply(f.term, x.term))
  def lam[A](name: String): [B] => (Expr[A] => Expr[B]) => Expr[A => B] = [B] =>
    (f: Expr[A] => Expr[B]) => Expr(Term.LamAbs(name, f(vr(name)).term))
  inline def lam[A, B](inline f: Expr[A] => Expr[B]): Expr[A => B] = ${ Macros.lamMacro('f) }
  inline def asExpr[A](inline e: A): Expr[A] = ${ Macros.asExprMacro('e) }
  def delay[A](x: Expr[A]): Expr[Delayed[A]] = Expr(Term.Delay(x.term))
  def force[A](x: Expr[Delayed[A]]): Expr[A] = Expr(Term.Force(x.term))
  def error: Expr[Delayed[Nothing]] = Expr(Term.Delay(Term.Error))
  def err: Expr[Nothing] = Expr(Term.Error)
  def let[A, B](expr: Expr[A])(f: Expr[A] => Expr[B]): Expr[B] = lam[A]("let")[B](f)(expr)

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

  def ifThenElse[A](cond: Expr[Boolean])(t: Expr[Delayed[A]])(
      f: Expr[Delayed[A]]
  ): Expr[Delayed[A]] =
    Expr(Term.Force(Term.Builtin(DefaultFun.IfThenElse)) $ cond.term $ t.term $ f.term)

  def ifThenElse2[A](cond: Expr[Boolean])(t: Expr[A])(
      f: Expr[A]
  ): Expr[A] = !ifThenElse(cond)(delay(t))(delay(f))
  val unConstrData: Expr[Data => (BigInt, List[Data])] = Expr(Term.Builtin(DefaultFun.UnConstrData))
  val unListData: Expr[Data => List[Data]] = Expr(Term.Builtin(DefaultFun.UnListData))
  val unBData: Expr[Data => Array[Byte]] = Expr(Term.Builtin(DefaultFun.UnBData))
  val unIData: Expr[Data => BigInt] = Expr(Term.Builtin(DefaultFun.UnIData))

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

  val addInteger: Expr[BigInt => BigInt => BigInt] = Expr(
    Term.Builtin(DefaultFun.AddInteger)
  )

  val substractInteger: Expr[BigInt => BigInt => BigInt] = Expr(
    Term.Builtin(DefaultFun.SubtractInteger)
  )

  val lessThanEqualsInteger: Expr[BigInt => BigInt => Boolean] = Expr(
    Term.Builtin(DefaultFun.LessThanEqualsInteger)
  )

  val lessThanInteger: Expr[BigInt => BigInt => Boolean] = Expr(
    Term.Builtin(DefaultFun.LessThanInteger)
  )

  val equalsInteger: Expr[BigInt => BigInt => Boolean] = Expr(
    Term.Builtin(DefaultFun.EqualsInteger)
  )

  def equalsByteString(lhs: Expr[Array[Byte]])(rhs: Expr[Array[Byte]]): Expr[Boolean] = Expr(
    Term.Builtin(DefaultFun.EqualsByteString) $ lhs.term $ rhs.term
  )

  inline def field[A: Data.ToData](inline expr: A => Any): Expr[Data] => Expr[Data] = ${
    Macros.fieldMacro('expr)
  }

  transparent inline def field2[A: Data.ToData](inline expr: A => Any): Any = ${
    Macros.fieldMacro2('expr)
  }

  extension (lhs: Expr[BigInt])
    @targetName("plus")
    def |+|(rhs: Expr[BigInt]): Expr[BigInt] = addInteger(lhs)(rhs)
    def |-|(rhs: Expr[BigInt]): Expr[BigInt] = substractInteger(lhs)(rhs)
    def ===(rhs: Expr[BigInt]): Expr[Boolean] = equalsInteger(lhs)(rhs)
    def <=(rhs: Expr[BigInt]): Expr[Boolean] = lessThanEqualsInteger(lhs)(rhs)
    def <(rhs: Expr[BigInt]): Expr[Boolean] = lessThanInteger(lhs)(rhs)

  extension (lhs: Expr[Array[Byte]])
    infix def =*=(rhs: Expr[Array[Byte]]): Expr[Boolean] = equalsByteString(lhs)(rhs)

  extension [A, B](lhs: Expr[A => B]) def apply(rhs: Expr[A]): Expr[B] = app(lhs, rhs)
  extension [B, C](lhs: Expr[B => C])
    def compose[A](rhs: Expr[A => B]): Expr[A => C] = lam(a => app(lhs, app(rhs, a)))
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
  import scalus.ledger.api.v1.Instances.given
  // simple validator that checks that the spending transaction has no outputs
  // it's a gift to the validators community

  val giftValidator: Expr[Unit => Unit => Data => Unit] = lam { redeemer =>
    lam { datum =>
      lam { ctx =>
        val txInfoOutputs =
          field[ScriptContext](_.scriptContextTxInfo.txInfoOutputs).apply(ctx)
        val isTxInfoOutputsEmpty = nullList(unListData(txInfoOutputs))
        ifThenElse2(isTxInfoOutputsEmpty)(())(err)
      }
    }
  }

  /// PubKey style validator. Checks whether the transaction has a specific signature
  def pubKeyValidator(pkh: PubKeyHash): Expr[Unit => Unit => Data => Unit] =
    lam { redeemer =>
      lam { datum =>
        lam { ctx =>
          val txInfoSignatories: Expr[List[Data]] = unListData(
            field[ScriptContext](_.scriptContextTxInfo.txInfoSignatories).apply(ctx)
          )

          val search = rec[List[Data], Unit] { self =>
            lam { signatories =>
              // signatories.head.pubKeyHash
              val head = headList.apply(signatories)
              val headPubKeyHash = unBData(head)
              !chooseList(signatories)(error) {
                ~ifThenElse2(equalsByteString(headPubKeyHash)(pkh.hash))(()) {
                  self(tailList(signatories))
                }
              }
            }
          }
          search(txInfoSignatories)
        }
      }
    }

  def mintingPolicyScript(txOutRef: TxOutRef, tokenName: TokenName): Expr[Unit => Data => Unit] =
    lam { redeemer =>
      lam { ctx =>
        val txInfo: Expr[Data] = field[ScriptContext](_.scriptContextTxInfo).apply(ctx)
        val purpose: Expr[Data] =
          field[ScriptContext](_.scriptContextPurpose).apply(ctx)
        val ownCurrencySymbol =
          unBData.apply(field[ScriptPurpose.Minting](_.curSymbol).apply(purpose))
        val minted: Expr[List[Data]] = unListData(
          field[TxInfo](_.txInfoMint).apply(txInfo)
        )
        val ref: Expr[Data] = headList(unListData(field[TxInfo](_.txInfoInputs).apply(txInfo)))
        val txId = unBData(field[TxInInfo](_.txInInfoOutRef.txOutRefId).apply(ref))
        val idx = unIData.apply(field[TxInInfo](_.txInInfoOutRef.txOutRefIdx).apply(ref))

        val checkMintedTrue: Expr[List[Data] => Unit] = rec[List[Data], Unit] { self =>
          lam(minted => ())
        }
        val checkMinted: Expr[List[Data] => Unit] = rec[List[Data], Unit] { self =>
          lam { minted =>
            // Value: List[(CurrencySymbol, List[(TokenName, Amount)])]
            // head: (CurrencySymbol, List[(TokenName, Amount)])
            val head = headList.apply(minted)
            // List(CurrencySymbol, List[(TokenName, Amount)])
            val curSymTokenPair: Expr[List[Data]] = sndPair(unConstrData.apply(head))
            val curSym = unBData.apply(headList(curSymTokenPair))
            // List[(TokenName, Amount): Data]
            val tokens = unListData.apply(headList(tailList(curSymTokenPair)))

            val checkTokens: Expr[List[Data] => Unit] = rec[List[Data], Unit] { self =>
              lam { tokens =>
                val head = headList.apply(tokens)
                val tokenAmountPair: Expr[List[Data]] = sndPair(unConstrData.apply(head))
                val token = unBData.apply(headList(tokenAmountPair))
                val amount = unIData.apply(headList(tailList(tokenAmountPair)))
                !ifThenElse(token =*= tokenName)(
                  ifThenElse(amount === BigInt(1))(~())(error)
                ) {
                  ~self(tailList(tokens))
                }
              }
            }

            ifThenElse2(curSym =*= ownCurrencySymbol) {
              checkTokens(tokens)
            } {
              self(tailList(minted))
            }
          }
        }

        !ifThenElse(txId =*= txOutRef.txOutRefId.id)(
          ifThenElse(idx === txOutRef.txOutRefIdx)(
            ~checkMinted(minted)
          )(error)
        )(error)
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

    val letTerm = let(BigInt(123))(x => x |+| BigInt(1)).term
    println(letTerm.pretty.render(80))
    println(Cek.evalUPLC(letTerm).pretty.render(80))

    def foo = 4

    val expr = asExpr {
      val a = 5
      foo
    }
    println(expr.term.pretty.render(80))
