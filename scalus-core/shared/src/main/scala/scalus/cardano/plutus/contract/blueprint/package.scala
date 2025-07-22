package scalus.cardano.plutus.contract
import scalus.buildinfo.BuildInfo
import scalus.cardano.ledger.Script.{PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint.Blueprint.Preamble
import scalus.utils.Hex.toHex

/** Implementation of Plutus Contract Blueprints.
  *
  * @see
  *   https://cips.cardano.org/cip/CIP-57
  */
package object blueprint {

    /** Returns a CIP-57 compliant [[Blueprint]] based on the provided [[validator]].
      *
      * The returned `Blueprint` always contains only 1 validator.
      *
      * To specify the `redeemer` and `datum` schemas, use [[Blueprint.newBuilder()]].
      *
      * @param contractTitle
      *   the title of the "blueprintee" contract
      * @param description
      *   the description of the "blueprintee" contact
      * @param validatorScript
      *   the script of the validator
      */
    def mkBlueprint(
        contractTitle: String,
        description: String,
        validatorScript: PlutusScript
    ): Blueprint = {
        val preamble = mkPreamble(contractTitle, description, validatorScript.language)
        val blueprintValidator = mkValidator(validatorScript)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    def mkPreamble(title: String, description: String, version: Language): Preamble =
        Preamble(
          title = title,
          description = Some(description),
          compiler = Some(CompilerInfo("scalus", Some(BuildInfo.version))),
          plutusVersion = Some(version)
        )

    private def mkValidator(validatorScript: Script) = {
        val cbor = validatorScript match {
            case PlutusV1(script)      => script.toHex
            case PlutusV2(script)      => script.toHex
            case PlutusV3(script)      => script.toHex
            case Script.Native(script) => script.toCbor.toHex
        }
        Validator(
          "validator",
          compiledCode = Some(cbor),
          hash = Some(validatorScript.scriptHash.toHex)
        )
    }
}
