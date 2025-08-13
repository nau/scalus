package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data, FromData}
import scalus.ledger.api.v3.*
import scalus.patterns.TransactionLevelMinterValidator
import scalus.prelude.*

@Compile
object TransactionLevelMinterValidatorExample extends Validator {
    case class SampleSpendRedeemer(
        ownIndex: BigInt,
        burn: Boolean
    )

    case class SampleMintRedeemer(maxUtxosToSpend: BigInt)

    given FromData[SampleSpendRedeemer] = FromData.derived
    given FromData[SampleMintRedeemer] = FromData.derived

    // Sample spend logic on how to use the provided interface. Here we are
    // passing script's own hash as the expected minting policy.
    override def spend(
        datum: Option[Data],
        redeemer: Redeemer,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val sampleSpendRedeemer = redeemer.to[SampleSpendRedeemer]
        // Grabbing spending UTxO based on the provided index.
        val input = tx.inputs.get(sampleSpendRedeemer.ownIndex).getOrFail("Undefined ownIndex")
        val ownAddress = input.resolved.address
        val outRef = input.outRef

        // Validating that the found UTxO is in fact the spending UTxO.
        require(ownRef === outRef)

        // Getting the validator's script hash.
        val ownHash = ownAddress.credential match
            case Credential.ScriptCredential(validatorHash) => validatorHash
            case _ => fail("Own address must be ScriptCredential")

        // Utilizing the design pattern, where the underlying logic expects a single
        // "BEACON" token to be either burnt or minted.
        TransactionLevelMinterValidator.spend(
          minterScriptHash = ownHash,
          minterRedeemerValidator = _.to[SampleMintRedeemer].maxUtxosToSpend > 0,
          minterTokensValidator = tnQtyDict => {
              val (tokenName, mintQuantity) = tnQtyDict.toList.head
              require(tokenName === ByteString.fromString("BEACON"))
              if sampleSpendRedeemer.burn then mintQuantity === BigInt(-1)
              else mintQuantity === BigInt(1)
          },
          txInfo = tx
        )
    }

    // Sample mint logic that benefits from this design pattern. This example
    // expects a specific number of inputs to be spent in each transaction.
    override def mint(redeemer: Redeemer, currencySymbol: CurrencySymbol, tx: TxInfo): Unit = {
        val sampleMintRedeemer = redeemer.to[SampleMintRedeemer]
        val scriptInputsCount = tx.inputs.foldRight(BigInt(0)) { (input, acc) =>
            input.resolved.address.credential match
                case Credential.ScriptCredential(validatorHash) =>
                    if validatorHash === currencySymbol then acc + 1 else acc
                case _ => acc
        }

        require(scriptInputsCount === sampleMintRedeemer.maxUtxosToSpend)
    }
}
