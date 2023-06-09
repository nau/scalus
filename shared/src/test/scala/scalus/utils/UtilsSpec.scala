package scalus.utils

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.ArbitraryInstances
import scalus.uplc.DeBruijn
import scalus.uplc.Program
import scalus.uplc.Term
import scalus.uplc.TermDSL.*

import java.nio.file.Files

class UtilsSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with scalus.ledger.api.v1.ArbitraryInstances {

  test("programToPlutusFileContent/readPlutusFileContent work") {
    val term = Arbitrary.arbitrary[Term].sample.get
    val program = Program((2, 0, 0), term)
    val debruijnedProgram = DeBruijn.deBruijnProgram(program)
    val undebuijnedProgram = DeBruijn.fromDeBruijnProgram(debruijnedProgram)
    val f = Files.createTempFile("test", ".plutus").toFile()
    val path = f.getAbsolutePath()
    f.deleteOnExit()
    Utils.writePlutusFile(path, program)
    val program2 = Utils.readPlutusFile(path)
    assert(undebuijnedProgram == program2)
  }

  test("readPlutusFile always-fails.plutus work") {
    val alwaysFails = """{"type":"PlutusScriptV2","description":"","cborHex":"4746020000222601"}"""
    val program = Utils.readPlutusFileContent(alwaysFails.getBytes("UTF-8"))
    assert(program == Program((2, 0, 0), lam("i0", "i1", "i2")(Term.Error(""))))
    val serialized = new String(Utils.programToPlutusFileContent(program), "UTF-8")
    assert(serialized == alwaysFails)
  }
}
