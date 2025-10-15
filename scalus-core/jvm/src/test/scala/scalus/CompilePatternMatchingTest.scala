package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.ByteString
import scalus.ledger.api.v1.*
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class CompilePatternMatchingTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    // Test nested pattern matching with enums
    test("nested pattern matching on List[Option[BigInt]]") {
        import scalus.prelude.*
        val compiled = compile { (x: List[Option[BigInt]]) =>
            x match
                case List.Cons(Option.Some(v), _) => v
                case List.Cons(Option.None, _)    => BigInt(0)
                case List.Nil                     => BigInt(-1)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            List.Cons(Option.Some(BigInt(42)), List.Nil)
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile {
            List.Cons(Option.None, List.Nil)
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(0)))

        val arg3 = compile {
            List.Nil: List[Option[BigInt]]
        }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(-1)))
    }

    // Test nested pattern matching with multiple levels
    test("deeply nested pattern matching on List[Option[These[BigInt, Boolean]]]") {
        import scalus.prelude.*
        val compiled = compile { (x: List[Option[These[BigInt, Boolean]]]) =>
            x match
                case List.Cons(Option.Some(These.This(n)), _) => n
                case List.Cons(Option.Some(These.That(b)), _) => if b then BigInt(1) else BigInt(0)
                case List.Cons(Option.Some(These.These(n, _)), _) => n + BigInt(100)
                case List.Cons(Option.None, _)                    => BigInt(-1)
                case List.Nil                                     => BigInt(-2)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            List.Cons(Option.Some(These.This(BigInt(42))), List.Nil)
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile {
            List.Cons(Option.Some(These.That(true)), List.Nil)
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(1)))

        val arg3 = compile {
            List.Cons(Option.Some(These.These(BigInt(5), false)), List.Nil)
        }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(105)))

        val arg4 = compile {
            List.Cons(Option.None: Option[These[BigInt, Boolean]], List.Nil)
        }.toUplc()
        assert((uplc $ arg4).evaluate == Term.Const(Constant.Integer(-1)))

        val arg5 = compile {
            List.Nil: List[Option[These[BigInt, Boolean]]]
        }.toUplc()
        assert((uplc $ arg5).evaluate == Term.Const(Constant.Integer(-2)))
    }

    // Test pattern matching with case classes
    test("pattern matching on case class PubKeyHash") {
        val compiled = compile { (pkh: PubKeyHash) =>
            pkh match
                case PubKeyHash(hash) => hash
        }

        val uplc = compiled.toUplc()
        val arg = compile { PubKeyHash(hex"deadbeef") }.toUplc()
        assert((uplc $ arg).evaluate == Term.Const(Constant.ByteString(hex"deadbeef")))
    }

    // Test nested case class pattern matching
    test("nested pattern matching on case classes") {
        case class Inner(value: BigInt)
        case class Outer(inner: Inner, flag: Boolean)

        val compiled = compile { (x: Outer) =>
            x match
                case Outer(Inner(v), true)  => v
                case Outer(Inner(v), false) => v + BigInt(100)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile { Outer(Inner(BigInt(42)), true) }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile { Outer(Inner(BigInt(42)), false) }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(142)))
    }

    // Test wildcard patterns
    test("pattern matching with wildcards") {
        import scalus.prelude.*
        val compiled = compile { (x: Option[BigInt]) =>
            x match
                case Option.Some(v) => v
                case _              => BigInt(0)
        }

        val uplc = compiled.toUplc()

        val arg1 = compile { Option.Some(BigInt(42)) }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile { Option.None: Option[BigInt] }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(0)))
    }

    // Test wildcard in nested patterns
    test("pattern matching with wildcards in nested positions") {
        import scalus.prelude.*
        val compiled = compile { (x: List[Option[BigInt]]) =>
            x match
                case List.Cons(Option.Some(v), _) => v
                case List.Cons(_, tail)           => tail.length
                case List.Nil                     => BigInt(-1)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            List.Cons(Option.Some(BigInt(42)), List.Nil)
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile {
            List.Cons(Option.None: Option[BigInt], List.Cons(Option.Some(BigInt(1)), List.Nil))
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(1)))
    }

    // Test constant patterns with integers
    // TODO: Add special handling for BigInt() constructor in PatternMatchingCompiler
    /*
    test("pattern matching on integer constants") {
        val compiled = compile { (x: BigInt) =>
            x match
                case BigInt(0) => "zero"
                case BigInt(1) => "one"
                case BigInt(2) => "two"
                case _         => "other"
        }

        val uplc = compiled.toUplc()

        val arg0 = compile { BigInt(0) }.toUplc()
        assert((uplc $ arg0).evaluate == Term.Const(Constant.String("zero")))

        val arg1 = compile { BigInt(1) }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.String("one")))

        val arg2 = compile { BigInt(2) }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.String("two")))

        val arg3 = compile { BigInt(42) }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.String("other")))
    }
     */

    // Test constant patterns with booleans
    test("pattern matching on boolean constants") {
        val compiled = compile { (x: Boolean) =>
            x match
                case true  => BigInt(1)
                case false => BigInt(0)
        }

        val uplc = compiled.toUplc()

        val argTrue = compile { true }.toUplc()
        assert((uplc $ argTrue).evaluate == Term.Const(Constant.Integer(1)))

        val argFalse = compile { false }.toUplc()
        assert((uplc $ argFalse).evaluate == Term.Const(Constant.Integer(0)))
    }

    // Test constant patterns with strings
    test("pattern matching on string constants") {
        val compiled = compile { (x: String) =>
            x match
                case "hello" => BigInt(1)
                case "world" => BigInt(2)
                case _       => BigInt(0)
        }

        val uplc = compiled.toUplc()

        val arg1 = compile { "hello" }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(1)))

        val arg2 = compile { "world" }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(2)))

        val arg3 = compile { "other" }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(0)))
    }

    // Test constant patterns with ByteString
    // TODO: Currently no onchain bytestring literals support in pattern matching
    /*
    test("pattern matching on ByteString constants") {
        val compiled = compile { (x: ByteString) =>
            x match
                case hex"dead" => BigInt(1)
                case hex"beef" => BigInt(2)
                case _         => BigInt(0)
        }

        val uplc = compiled.toUplc()

        val arg1 = compile { hex"dead" }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(1)))

        val arg2 = compile { hex"beef" }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(2)))

        val arg3 = compile { hex"cafe" }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(0)))
    }
     */

    // Test mixed patterns: constructors, constants, and wildcards
    // TODO: Add special handling for BigInt() constructor in PatternMatchingCompiler
    /*
    test("pattern matching with mixed patterns") {
        import scalus.prelude.*
        val compiled = compile { (x: Option[BigInt]) =>
            x match
                case Option.Some(BigInt(0)) => "zero"
                case Option.Some(BigInt(1)) => "one"
                case Option.Some(_)         => "some"
                case Option.None            => "none"
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg0 = compile { Option.Some(BigInt(0)) }.toUplc()
        assert((uplc $ arg0).evaluate == Term.Const(Constant.String("zero")))

        val arg1 = compile { Option.Some(BigInt(1)) }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.String("one")))

        val arg42 = compile { Option.Some(BigInt(42)) }.toUplc()
        assert((uplc $ arg42).evaluate == Term.Const(Constant.String("some")))

        val argNone = compile { Option.None: Option[BigInt] }.toUplc()
        assert((uplc $ argNone).evaluate == Term.Const(Constant.String("none")))
    }
     */

    // Test guards with if expressions
    test("pattern matching with if guards") {
        import scalus.prelude.*
        val compiled = compile { (x: Option[BigInt]) =>
            x match
                case Option.Some(v) if v > BigInt(0) => "positive"
                case Option.Some(v) if v < BigInt(0) => "negative"
                case Option.Some(_)                  => "zero"
                case Option.None                     => "none"
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val argPos = compile { Option.Some(BigInt(42)) }.toUplc()
        assert((uplc $ argPos).evaluate == Term.Const(Constant.String("positive")))

        val argNeg = compile { Option.Some(BigInt(-42)) }.toUplc()
        assert((uplc $ argNeg).evaluate == Term.Const(Constant.String("negative")))

        val argZero = compile { Option.Some(BigInt(0)) }.toUplc()
        assert((uplc $ argZero).evaluate == Term.Const(Constant.String("zero")))

        val argNone = compile { Option.None: Option[BigInt] }.toUplc()
        assert((uplc $ argNone).evaluate == Term.Const(Constant.String("none")))
    }

    // Test guards with complex conditions
    test("pattern matching with complex guards") {
        import scalus.prelude.*
        val compiled = compile { (x: List[BigInt]) =>
            x match
                case List.Cons(h, t) if h > BigInt(0) && t.length > 0 => "positive head with tail"
                case List.Cons(h, _) if h > BigInt(0)                 => "positive head no tail"
                case List.Cons(_, t) if t.length > 0                  => "has tail"
                case _                                                => "other"
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            List.Cons(BigInt(1), List.Cons(BigInt(2), List.Nil))
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.String("positive head with tail")))

        val arg2 = compile {
            List.Cons(BigInt(1), List.Nil)
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.String("positive head no tail")))

        val arg3 = compile {
            List.Cons(BigInt(-1), List.Cons(BigInt(2), List.Nil))
        }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.String("has tail")))

        val arg4 = compile {
            List.Nil: List[BigInt]
        }.toUplc()
        assert((uplc $ arg4).evaluate == Term.Const(Constant.String("other")))
    }

    // Test tuple pattern matching
    // TODO: Add special handling for BigInt() constructor in PatternMatchingCompiler
    /*
    test("pattern matching on tuples") {
        val compiled = compile { (x: (BigInt, Boolean, String)) =>
            x match
                case (BigInt(0), true, s) => s
                case (n, false, _)        => n.toString
                case (n, true, _)         => (n + BigInt(100)).toString
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile { (BigInt(0), true, "zero") }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.String("zero")))

        val arg2 = compile { (BigInt(42), false, "ignored") }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.String("42")))

        val arg3 = compile { (BigInt(42), true, "ignored") }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.String("142")))
    }
     */

    // Test nested tuple pattern matching
    // TODO: Add special handling for BigInt() constructor in PatternMatchingCompiler
    /*
    test("pattern matching on nested tuples") {
        val compiled = compile { (x: ((BigInt, Boolean), String)) =>
            x match
                case ((BigInt(0), true), s) => s
                case ((n, false), _)        => n.toString
                case ((n, _), _)            => (n + BigInt(100)).toString
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile { ((BigInt(0), true), "zero") }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.String("zero")))

        val arg2 = compile { ((BigInt(42), false), "ignored") }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.String("42")))

        val arg3 = compile { ((BigInt(42), true), "ignored") }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.String("142")))
    }
     */

    // Test pattern matching with variable binding at different levels
    test("pattern matching with variable bindings") {
        import scalus.prelude.*
        val compiled = compile { (x: List[Option[BigInt]]) =>
            x match
                case List.Cons(some @ Option.Some(v), _) if v > BigInt(0) => v
                case List.Cons(opt @ Option.None, tail)                   => tail.length
                case list @ List.Nil                                      => BigInt(-1)
                case _                                                    => BigInt(-2)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            List.Cons(Option.Some(BigInt(42)), List.Nil)
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile {
            List.Cons(Option.None: Option[BigInt], List.Cons(Option.Some(BigInt(1)), List.Nil))
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(1)))

        val arg3 = compile {
            List.Nil: List[Option[BigInt]]
        }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(-1)))
    }

    // Test exhaustiveness checking with @unchecked
    test("non-exhaustive pattern matching with @unchecked") {
        import scalus.prelude.*
        val compiled = compile { (x: Option[BigInt]) =>
            (x: @unchecked) match
                case Option.Some(v) => v
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val argSome = compile { Option.Some(BigInt(42)) }.toUplc()
        assert((uplc $ argSome).evaluate == Term.Const(Constant.Integer(42)))

        // This should fail with an error
        val argNone = compile { Option.None: Option[BigInt] }.toUplc()
        val result = (uplc $ argNone).evaluateDebug
        assert(result.isFailure)
    }

    // Test pattern matching in nested contexts
    // TODO: Fix issue with sequential match expressions
    /*
    test("pattern matching within pattern matching") {
        import scalus.prelude.*
        val compiled = compile { (x: Option[List[BigInt]]) =>
            x match
                case Option.Some(list) =>
                    list match
                        case List.Cons(h, _) => h
                        case List.Nil        => BigInt(0)
                case Option.None => BigInt(-1)
        }

        val uplc = compiled.toUplc(generateErrorTraces = true)

        val arg1 = compile {
            Option.Some(List.Cons(BigInt(42), List.Nil))
        }.toUplc()
        assert((uplc $ arg1).evaluate == Term.Const(Constant.Integer(42)))

        val arg2 = compile {
            Option.Some(List.Nil: List[BigInt])
        }.toUplc()
        assert((uplc $ arg2).evaluate == Term.Const(Constant.Integer(0)))

        val arg3 = compile {
            Option.None: Option[List[BigInt]]
        }.toUplc()
        assert((uplc $ arg3).evaluate == Term.Const(Constant.Integer(-1)))
    }
     */

}
