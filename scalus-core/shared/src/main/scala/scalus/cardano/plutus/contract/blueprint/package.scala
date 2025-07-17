package scalus.cardano.plutus.contract
import io.bullet.borer.Cbor
import scalus.buildinfo.BuildInfo
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.Language
import scalus.cardano.ledger.Language.languageId
import scalus.utils.Hex.toHex

package object blueprint {

    /** Returns a CIP-57 compliant [[Blueprint]] based on the provided [[validator]].
      *
      * The returned `Blueprint` always contains only 1 validator.
      *
      * @param contractTitle
      *   the title of the "blueprintee" contract
      * @param description
      *   the description of the "blueprintee" contact
      * @param version
      *   the plutus version used in the contract
      * @param validatorFnCbored
      *   CBORed UPLC of the validator
      */
    def mkBlueprint(
        contractTitle: String,
        description: String,
        version: Language,
        validatorFnCbored: Array[Byte]
    ): Blueprint = {
        val preamble = mkPreamble(contractTitle, description, version)
        val blueprintValidator = mkValidator(version, validatorFnCbored)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    private def mkPreamble(title: String, description: String, version: Language) =
        Preamble(
          title = title,
          description = Some(description),
          compiler = Some(CompilerInfo("scalus", Some(BuildInfo.scalusVersion))),
          plutusVersion = Some(version)
        )

    private inline def mkValidator(version: Language, cboredValidatorFn: Array[Byte]) = {
        val decodedFn =
            Cbor.decode(cboredValidatorFn).to[Array[Byte]].value // assumed to be flattened
        val preimage = languageId(version).toByte +: decodedFn
        val hash = platform.blake2b_224(ByteString.fromArray(preimage)).toHex

        Validator(
          "validator",
          compiledCode = Some(cboredValidatorFn.toHex),
          hash = Some(hash)
        )
    }
}
