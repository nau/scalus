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
        import sir.SIRType.{Fun, Integer, BooleanPrimitive, VoidPrimitive}
        import scalus.uplc.Constant
        import scalus.uplc.DefaultFun.{IfThenElse as _, *}

        val xVar = Var("x", SIRType.Integer)
        val nonRecursiveVar = Var("nonRecursive", Fun(Integer, Integer))
        val recursiveVar = Var("recursive", Fun(Integer, Integer))

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
                              Apply(
                                SIRBuiltins.equalsInteger,
                                xVar,
                                Fun(Integer, BooleanPrimitive)
                              ),
                              Const(Constant.Integer(0), Integer),
                              BooleanPrimitive
                            ),
                            Const(Constant.Integer(0), Integer),
                            Apply(
                              recursiveVar,
                              Apply(
                                Apply(
                                  SIRBuiltins.subtractInteger,
                                  xVar,
                                  Fun(Integer, Integer)
                                ),
                                Const(Constant.Integer(1), Integer),
                                Integer
                              ),
                              Integer
                            ),
                            Integer
                          )
                        )
                      )
                    ),
                    Apply(recursiveVar, xVar, Integer)
                  )
                )
              )
            ),
            Const(Constant.Unit, VoidPrimitive)
          )
        )
    }
