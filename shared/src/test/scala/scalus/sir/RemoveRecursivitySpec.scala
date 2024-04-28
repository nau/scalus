package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import scalus.*

class RemoveRecursivitySpec extends AnyFunSuite:
    test("remove recursivity") {
        val compiled = Compiler.compile {
            def nonRecursive(x: BigInt): BigInt =
                def recursive(x: BigInt): BigInt =
                    if x == BigInt(0) then 0 else recursive(x - 1)
                recursive(x)
        }
        val optimized = RemoveRecursivity(compiled)
        import sir.SIR.*, Recursivity.*
        import scalus.uplc.Constant.{Unit, Integer}
        import scalus.uplc.DefaultFun.{IfThenElse as _, *}
        assert(
          optimized == Let(
            NonRec,
            List(
              Binding(
                "nonRecursive",
                LamAbs(
                  "x",
                  Let(
                    Rec,
                    List(
                      Binding(
                        "recursive",
                        LamAbs(
                          "x",
                          IfThenElse(
                            Apply(Apply(Builtin(EqualsInteger), Var("x")), Const(Integer(0))),
                            Const(Integer(0)),
                            Apply(
                              Var("recursive"),
                              Apply(Apply(Builtin(SubtractInteger), Var("x")), Const(Integer(1)))
                            )
                          )
                        )
                      )
                    ),
                    Apply(Var("recursive"), Var("x"))
                  )
                )
              )
            ),
            Const(Unit)
          )
        )
    }
