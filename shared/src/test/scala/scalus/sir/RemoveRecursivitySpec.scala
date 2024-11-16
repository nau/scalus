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
        import sir.SIRType.{Fun, IntegerPrimitive, BooleanPrimitive, VoidPrimitive}
        import scalus.uplc.Constant.{Unit, Integer}
        import scalus.uplc.DefaultFun.{IfThenElse as _, *}

        val xVar = Var("x", SIRType.IntegerPrimitive)
        val nonRecursiveVar = Var("nonRecursive", Fun(IntegerPrimitive, IntegerPrimitive))
        val recursiveVar = Var("recursive", Fun(IntegerPrimitive, IntegerPrimitive))

        assert(
          optimized == Let(
            NonRec,
            List(
              Binding(
                "nonRecursive",
                LamAbs(
                  xVar,
                  Let(
                    Rec,
                    List(
                      Binding(
                        "recursive",
                        LamAbs(
                          xVar,
                          IfThenElse(
                            Apply(
                                Apply(SIRBuiltins.equalsInteger, xVar, Fun(IntegerPrimitive,BooleanPrimitive) ),
                                Const(Integer(0), IntegerPrimitive),
                                BooleanPrimitive
                            ),
                            Const(Integer(0), IntegerPrimitive),
                            Apply(
                              recursiveVar,
                              Apply(
                                  Apply(SIRBuiltins.subtractInteger, xVar, Fun(IntegerPrimitive,IntegerPrimitive)),
                                  Const(Integer(1), IntegerPrimitive),
                                  IntegerPrimitive),
                              IntegerPrimitive
                            ),
                            IntegerPrimitive
                          )
                        )
                      )
                    ),
                    Apply(recursiveVar, xVar, IntegerPrimitive)
                  )
                )
              )
            ),
            Const(Unit, VoidPrimitive)
          )
        )
    }
