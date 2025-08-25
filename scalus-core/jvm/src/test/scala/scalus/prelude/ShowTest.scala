package scalus.prelude

import scalus.Compiler
import scalus.Compiler.Options
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.Data.toData

class ShowTest extends StdlibTestKit {

    test("Show[Unit] is ()") {
        assertEvalEq(().show, "()")
    }

    test("Show[Boolean] is correct") {
        assertEvalEq(true.show, "True")
        assertEvalEq(false.show, "False")
    }

    test("Show[ByteString] is a hex string") {
        assertEvalEq(hex"00112233".show, "\"00112233\"")
    }

    test("Show[String] is correct") {
        assertEvalEq("Hello, World!".show, "\"Hello, World!\"")
        assertEvalEq("".show, "\"\"")
    }

    test("Show[BigInt] is correct") {
        assertEvalEq(BigInt(0).show, "0")
        assertEvalEq(BigInt(123456789).show, "123456789")
        assertEvalEq(BigInt(-123456789).show, "-123456789")
    }

    test("Show[Data] is correct") {
        assertEvalEq(BigInt(0).toData.show, "0")
        assertEvalEq(hex"0011".toData.show, "\"0011\"")
        assertEvalEq(
          List(BigInt(0).toData, hex"0011".toData).toData.show,
          "[0, \"0011\"]"
        )
        // FIXME: enable this test when AssocMap is fixed
//        assertEvalEq(
//          AssocMap(List((BigInt(0).toData, hex"0011".toData))).toData.show,
//          "{0: \"0011\"}"
//        )
        assertEvalEq(
          Rational(1, 2).toData.show,
          "<0, [1, 2]>" // Rational is represented as a pair of BigInts
        )
    }

}
