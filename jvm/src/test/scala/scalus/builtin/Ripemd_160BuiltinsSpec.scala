package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.{fromString, given}

import scala.language.implicitConversions

// TODO: move to shared when BLS12-381 builtins on JS are implemented
class Ripemd_160BuiltinsSpec extends AnyFunSuite {
    test("empty input") {
        assertResult(hex"9c1185a5c5e9fc54612808977ee8f548b2258d31")(ripemd_160(hex""))
    }

    test("well known phrases as input") {
        assertResult(hex"37f332f68db77bd9d7edd4969571ad671cf9dd3b")(
          ripemd_160(fromString("The quick brown fox jumps over the lazy dog"))
        )

        assertResult(hex"132072df690933835eb8b6ad0b77e7b6f14acad7")(
          ripemd_160(fromString("The quick brown fox jumps over the lazy cog"))
        )
    }
}
