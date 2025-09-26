package scalus.uplc

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.cbor.Cbor
import scalus.*

class DeBruijnedProgramTest extends AnyFunSuite {
    private val flatEncoded = Compiler.compile(true).toUplc().plutusV3.flatEncoded

    test("fromCborWithRemainingBytes") {
        val suffix = Arbitrary.arbitrary[Array[Byte]].sample.get
        val cbor = Cbor.encode(flatEncoded ++ suffix)
        val result = DeBruijnedProgram.fromCborWithRemainingBytes(cbor)

        assert(result._2 sameElements suffix)
    }
}
