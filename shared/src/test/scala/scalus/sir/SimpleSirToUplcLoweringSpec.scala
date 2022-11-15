package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.sir.Recursivity.NonRec
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.{ArbitraryInstances, Constant, DefaultFun, NamedDeBruijn, Term}

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
        SIR.Constr("Cons", List(SIR.Var(NamedDeBruijn("h")), SIR.Var(NamedDeBruijn("tl")))),
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
