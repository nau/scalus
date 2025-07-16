package scalus.cardano.plutus.contract
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import scalus.Compiler
import scalus.buildinfo.BuildInfo
import scalus.builtin.{platform, ByteString, Data}
import scalus.toUplcOptimized
import scalus.cardano.plutus.contract.blueprint.model.*
import scalus.prelude.Validator as ScalusValidator
import scalus.utils.Hex.toHex
import scalus.{plutusV1, plutusV2, plutusV3}

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
      * @param validator
      *   the validator that represents the contract logic
      */
    def mkBlueprint(
        contractTitle: String,
        description: String,
        version: PlutusVersion,
        validator: ScalusValidator
    ): Blueprint = {
        val preamble = mkPreamble(contractTitle, description, version)
        val blueprintValidator = mkValidator(version, validator)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    def blueprintString(
        contractTitle: String,
        description: String,
        version: PlutusVersion,
        validator: ScalusValidator
    ): String = writeToString(mkBlueprint(contractTitle, description, version, validator))

    private def mkPreamble(title: String, description: String, version: PlutusVersion) =
        Preamble(
          title = title,
          description = Some(description),
          compiler = Some(CompilerInfo("scalus", Some(BuildInfo.scalusVersion))),
          plutusVersion = Some(version)
        )

    private inline def mkValidator(version: PlutusVersion, validator: ScalusValidator) = {
        val title = validator.getClass.getSimpleName
        // todo unsure if it makes a difference
        val uplc = {
            version match {
                case PlutusVersion.v1 => validatorUplc(validator.validate).plutusV1
                case PlutusVersion.v2 => validatorUplc(validator.validate).plutusV2
                case PlutusVersion.v3 => validatorUplc(validator.validate).plutusV3
            }
        }
        val cboredFn = uplc.cborEncoded.toHex
        val preimage = version.langTag.toByte +: uplc.flatEncoded
        val hash = platform.blake2b_224(ByteString.fromArray(preimage)).toHex

        Validator(
          title,
          compiledCode = Some(cboredFn),
          hash = Some(hash)
        )
    }

    private type ValidatorFn = Data => Unit

    private inline def validatorUplc(inline fn: ValidatorFn) =
        Compiler.compileInline(fn).toUplcOptimized()
}
