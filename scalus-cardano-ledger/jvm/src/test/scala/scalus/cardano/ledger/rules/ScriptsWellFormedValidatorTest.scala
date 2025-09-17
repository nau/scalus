package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.DeBruijnedProgram

class ScriptsWellFormedValidatorTest extends AnyFunSuite {

    private val program = Compiler.compile(true).toUplc().plutusV3
    test("testValidate") {
        pending
        // make arbitrary array
        val suffix = Arbitrary.arbitrary[Array[Byte]].sample.get
        val cbor = Cbor.encode(program.flatEncoded ++ suffix)
        DeBruijnedProgram.fromCbor(cbor)
    }
}
