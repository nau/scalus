package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.sir.Recursivity.NonRec
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.{ArbitraryInstances, Constant, DefaultFun, NamedDeBruijn, Term}
import scalus.builtins.ByteString.StringInterpolators

class SimpleSirToUplcLoweringSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:
  extension (s: SIR)
    infix def lowersTo(r: Term): Unit =
      val l = SimpleSirToUplcLowering()
      assert(l.lower(s) == r)

  test("pretty") {
    val data =
      DataDecl("List", List(ConstrDecl("Nil", List()), ConstrDecl("Cons", List("head", "tail"))))
    val sir = SIR.Decl(
      data,
      SIR.Match(
        SIR.Constr("Cons", data, List(SIR.Var(NamedDeBruijn("h")), SIR.Var(NamedDeBruijn("tl")))),
        List(
          Case(ConstrDecl("Nil", List()), List(), SIR.Const(Constant.Integer(1))),
          Case(ConstrDecl("Cons", List()), List("h", "tl"), SIR.Const(Constant.Integer(2)))
        )
      )
    )
    println(sir.pretty.render(10))
    println(sir.pretty.render(100))
  }

  test("lower constant") {
    forAll { (c: Constant) =>
      SIR.Const(c) lowersTo Term.Const(c)
    }
  }

  test("lower") {
    SIR.Error("error") lowersTo Term.Error("error")
    SIR.Var(NamedDeBruijn("x", 1)) lowersTo Term.Var(NamedDeBruijn("x", 1))
    SIR.Apply(
      SIR.LamAbs("x", SIR.Var(NamedDeBruijn("x", 0))),
      SIR.Const(Constant.Unit)
    ) lowersTo Term.Apply(
      Term.LamAbs("x", Term.Var(NamedDeBruijn("x", 0))),
      Term.Const(Constant.Unit)
    )
  }

  test("lower builtins") {
    SIR.Builtin(AddInteger) lowersTo Term.Builtin(AddInteger)
    SIR.Builtin(HeadList) lowersTo Term.Force(Term.Builtin(HeadList))
    SIR.Builtin(FstPair) lowersTo Term.Force(Term.Force(Term.Builtin(FstPair)))
  }

  test("lower let") {
    import scalus.uplc.TermDSL.{*, given}
    /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
     */
    SIR.Let(
      NonRec,
      Binding("x", SIR.Const(asConstant(1))) :: Binding("y", SIR.Const(asConstant(2))) :: Nil,
      SIR.Apply(
        SIR.Apply(SIR.Builtin(AddInteger), SIR.Var(NamedDeBruijn("x"))),
        SIR.Var(NamedDeBruijn("y"))
      )
    ) lowersTo (lam("x")(
      lam("y")(AddInteger $ Term.Var(NamedDeBruijn("x")) $ Term.Var(NamedDeBruijn("y"))) $ 2
    ) $ 1)
  }

  test("lower Constr") {
    import scalus.uplc.TermDSL.{*, given}
    /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
     */
    val listData =
      DataDecl("List", List(ConstrDecl("Nil", List()), ConstrDecl("Cons", List("head", "tail"))))
    val txIdData = DataDecl("TxId", List(ConstrDecl("TxId", List("hash"))))
    def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
    withDecls(SIR.Constr("Nil", listData, List())) lowersTo (lam("Nil", "Cons")(
      !(Term.Var(NamedDeBruijn("Nil")))
    ))
    withDecls(
      SIR.Constr("TxId", txIdData, List(SIR.Const(asConstant(hex"DEADBEEF"))))
    ) lowersTo (lam(
      "hash",
      "TxId"
    )(Term.Var(NamedDeBruijn("TxId")) $ Term.Var(NamedDeBruijn("hash"))) $ Term.Const(
      asConstant(hex"DEADBEEF")
    ))

  }

  test("lower Match") {
    import scalus.uplc.TermDSL.{*, given}
    /* Nil match
        case Nil -> 1
        case Cons(h, tl) -> 2

       lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
     */
    val nilConstr = ConstrDecl("Nil", List())
    val consConstr = ConstrDecl("Cons", List("head", "tail"))
    val listData =
      DataDecl("List", List(nilConstr, consConstr))
    val txIdData = DataDecl("TxId", List(ConstrDecl("TxId", List("hash"))))
    def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
    withDecls(
      SIR.Match(
        SIR.Constr("Nil", listData, List()),
        List(
          Case(nilConstr, Nil, SIR.Const(Constant.Integer(1))),
          Case(consConstr, List("h", "tl"), SIR.Const(Constant.Integer(2)))
        )
      )
    ) lowersTo (lam("Nil", "Cons")(!(Term.Var(NamedDeBruijn("Nil")))) $ ~Term.Const(
      asConstant(1)
    ) $ lam("h", "tl")(Term.Const(asConstant(2))))
  }
  test("eta-reduction") {
    import scalus.uplc.TermDSL.{*, given}
    // (\x -> f x) reduces to f
    // (\x y -> f x y) reduces to f

    val l = SimpleSirToUplcLowering()
    assert(
      l.etaReduce(
        lam("x")(Term.Var(NamedDeBruijn("f")) $ Term.Var(NamedDeBruijn("x")))
      ) === Term.Var(NamedDeBruijn("f"))
    )
    assert(
      l.etaReduce(
        lam("x", "y")(
          Term.Var(NamedDeBruijn("f")) $ Term.Var(NamedDeBruijn("x")) $ Term.Var(NamedDeBruijn("y"))
        )
      ) === Term.Var(NamedDeBruijn("f"))
    )
  }
