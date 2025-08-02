package scalus.prelude

import scalus.builtin.ByteString.hex
import scalus.prelude.Show.*

class ShowTest extends StdlibTestKit {

    test("Show[Unit] is ()") {
        assertEvalEq(().show, "()")
    }

    test("Show[Boolean] is correct") {
        assertEvalEq(true.show, "True")
        assertEvalEq(false.show, "False")
    }

    test("Show[ByteString] is a hex string") {
        assertEvalEq(hex"00112233".show, "00112233")
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
        assertEvalEq(BigInt(0).toData.show, "I(0)")
    }

}
