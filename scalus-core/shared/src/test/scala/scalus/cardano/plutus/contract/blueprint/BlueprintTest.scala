package scalus.cardano.plutus.contract.blueprint
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.Language.{languageId, PlutusV2, PlutusV3}
import scalus.cardano.plutus.contract.blueprint
import scalus.cardano.plutus.contract.blueprint.ValidBlueprints.ForFakeValidator.compiledFakeValidatorFn
import scalus.utils.Hex

class BlueprintTest extends AnyFunSuite {

    test("should hash the script using the language tag") {
        val bp = blueprint.mkBlueprint("title", "description", PlutusV3, compiledFakeValidatorFn)
        val validator = bp.validators.head

        val hash = validator.hash.get
        val compiled = validator.compiledCode.get

        val unCboredCompiledScript = Cbor.decode(Hex.hexToBytes(compiled)).to[Array[Byte]].value
        val assumedPreimage = languageId(PlutusV3).toByte +: unCboredCompiledScript
        val expectedHash = platform.blake2b_224(ByteString.fromArray(assumedPreimage))

        assert(expectedHash.toHex == hash)

        val incorrectPreimage = languageId(PlutusV2).toByte +: unCboredCompiledScript
        val v2Hash = platform.blake2b_224(ByteString.fromArray(incorrectPreimage))

        assert(expectedHash != v2Hash)
    }

    test("should produce a correct JSON value") {
        val title = "title"
        val descriptions = "description"
        val bp = blueprint.mkBlueprint(title, descriptions, PlutusV3, compiledFakeValidatorFn)
        val expected = ValidBlueprints.ForFakeValidator.compiledAndHashed(title, descriptions)

        assert(bp.show == expected)
    }
}
