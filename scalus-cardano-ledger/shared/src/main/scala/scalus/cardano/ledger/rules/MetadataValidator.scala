package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.PlutusScript
import scalus.builtin.{platform, ByteString}

import java.nio.charset.StandardCharsets

// It's part of Shelley.validateMetadata in cardano-ledger
object MetadataValidator extends STS.Validator {
    override final type Error = TransactionException.MetadataException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val body = event.body.value

        val auxiliaryDataOption = event.auxiliaryData
        val auxiliaryDataHashOption = body.auxiliaryDataHash
        val majorProtocolVersion = MajorProtocolVersion(context.env.params.protocolVersion.major)

        (auxiliaryDataHashOption, auxiliaryDataOption) match
            case (None, None) => success

            case (Some(auxiliaryDataHash), None) =>
                failure(
                  TransactionException.MetadataException.MissingAuxiliaryDataException(
                    transactionId,
                    auxiliaryDataHash
                  )
                )

            case (None, Some(auxiliaryData)) =>
                failure(
                  TransactionException.MetadataException.MissingAuxiliaryDataHashException(
                    transactionId,
                    auxiliaryData.value
                  )
                )

            case (Some(expectedAuxiliaryDataHash), Some(auxiliaryData)) =>
                val actualAuxiliaryDataHash = AuxiliaryDataHash.fromByteString(
                  platform.blake2b_256(ByteString.unsafeFromArray(auxiliaryData.raw))
                )

                if actualAuxiliaryDataHash != expectedAuxiliaryDataHash then
                    return failure(
                      TransactionException.MetadataException.InvalidAuxiliaryDataHashException(
                        transactionId,
                        actualAuxiliaryDataHash,
                        expectedAuxiliaryDataHash
                      )
                    )

                if majorProtocolVersion <= MajorProtocolVersion.shelleyPV then return success

                if isValidAuxiliaryData(auxiliaryData.value, majorProtocolVersion) then success
                else
                    failure(
                      TransactionException.MetadataException.InvalidAuxiliaryDataException(
                        transactionId,
                        auxiliaryData.value
                      )
                    )
    }

    private def isValidAuxiliaryData(
        auxiliaryData: AuxiliaryData,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        isValidAuxiliaryMetadata(auxiliaryData) && isValidAuxiliaryPlutusScripts(
          auxiliaryData,
          majorProtocolVersion
        )
    }

    private def isValidAuxiliaryMetadata(auxiliaryData: AuxiliaryData): Boolean = {
        auxiliaryData.getMetadata.view.values.forall(isValidTransactionMetadatum)
    }

    private def isValidTransactionMetadatum(metadatum: TransactionMetadatum): Boolean = {
        metadatum match {
            case TransactionMetadatum.Int(_)       => true
            case TransactionMetadatum.Bytes(bytes) => bytes.length <= MaxSize
            case TransactionMetadatum.Text(str) =>
                str.getBytes(StandardCharsets.UTF_8).length <= MaxSize
            case TransactionMetadatum.List(items) => items.forall(isValidTransactionMetadatum)
            case TransactionMetadatum.Map(entries) =>
                entries.forall { (key, value) =>
                    isValidTransactionMetadatum(key) && isValidTransactionMetadatum(value)
                }
        }
    }

    private def isValidAuxiliaryPlutusScripts(
        auxiliaryData: AuxiliaryData,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        auxiliaryData.getPlutusV1Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV1, majorProtocolVersion)
        } && auxiliaryData.getPlutusV2Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV2, majorProtocolVersion)
        } && auxiliaryData.getPlutusV3Scripts.forall {
            PlutusScript.isWellFormed(_, Language.PlutusV3, majorProtocolVersion)
        }
    }

    private val MaxSize = 64
}
