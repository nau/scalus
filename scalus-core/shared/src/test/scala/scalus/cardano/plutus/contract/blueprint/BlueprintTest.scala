package scalus.cardano.plutus.contract.blueprint
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.plutus.contract.blueprint.model.PlutusVersion.{v2, v3}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.{Validator, *}
import scalus.utils.Hex
import scalus.cardano.plutus.contract.blueprint

class BlueprintTest extends AnyFunSuite {

    test("should hash the script using the language tag") {
        val bp = blueprint.mkBlueprint("title", "description", v3, FakeValidator)
        val validator = bp.validators.head

        val hash = validator.hash.get
        val compiled = validator.compiledCode.get

        val unCboredCompiledScript = Cbor.decode(Hex.hexToBytes(compiled)).to[Array[Byte]].value
        val assumedPreimage = v3.langTag.toByte +: unCboredCompiledScript
        val expectedHash = platform.blake2b_224(ByteString.fromArray(assumedPreimage))

        assert(expectedHash.toHex == hash)

        val incorrectPreimage = v2.langTag.toByte +: unCboredCompiledScript
        val v2Hash = platform.blake2b_224(ByteString.fromArray(incorrectPreimage))

        assert(expectedHash != v2Hash)
    }

    object FakeValidator extends Validator:
        override def spend(
            datum: Option[Data],
            redeemer: Data,
            tx: TxInfo,
            ownRef: TxOutRef
        ): Unit = ()
}
