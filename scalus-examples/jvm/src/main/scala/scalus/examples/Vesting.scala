package scalus.examples

import scalus.*
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.ledger.api.v1.IntervalBoundType.*
import scalus.ledger.api.v2.OutputDatum
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.Compiler.compile

case class VestingDatum(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    initialAmount: Lovelace
) derives FromData,
      ToData

@Compile
object VestingDatum {
    given Eq[VestingDatum] = (x, y) =>
        x.beneficiary === y.beneficiary && x.startTimestamp == y.startTimestamp && x.duration == y.duration && x.initialAmount == y.initialAmount
}

case class VestingRedeemer(amount: Lovelace) derives FromData, ToData

@Compile
object VestingRedeemer

@Compile
object VestingUtils {
    def getOwnInput(inputs: List[TxInInfo], ownRef: TxOutRef): TxInInfo = {
        inputs.find(input => input.outRef === ownRef).get
    }

    def linearVesting(vestingDatum: VestingDatum, timestamp: BigInt): BigInt = {
        val min = vestingDatum.startTimestamp
        val max = vestingDatum.startTimestamp + vestingDatum.duration
        if timestamp < min then 0
        else if timestamp >= max then vestingDatum.initialAmount
        else
            vestingDatum.initialAmount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
    }
}

@Compile
object Vesting extends Validator:
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val Some(receivedData) = datum: @unchecked
        val vestingDatum: VestingDatum = receivedData.to[VestingDatum]
        val VestingRedeemer(requestedAmount) = redeemer.to[VestingRedeemer]

        require(requestedAmount > 0, "Withdrawal amount must be greater than 0")

        val ownInput = VestingUtils.getOwnInput(txInfo.inputs, txOutRef).resolved
        val contractAddress = ownInput.address
        val contractAmount = ownInput.value.getLovelace

        val contractOutputs = txInfo.outputs.filter(txOut => txOut.address === contractAddress)

        val txEarliestTime = txInfo.validRange.from.boundType match
            case Finite(t) => t
            case _         => BigInt(0)

        val released = vestingDatum.initialAmount - contractAmount

        val availableAmount = VestingUtils.linearVesting(vestingDatum, txEarliestTime) - released

        require(
          txInfo.signatories.contains(vestingDatum.beneficiary),
          "No signature from beneficiary"
        )
        require(
          requestedAmount <= availableAmount,
          "Declared amount does not match calculated amount"
        )

        val beneficiaryInputs = txInfo.inputs.filter(txInInfo =>
            txInInfo.resolved.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )
        val beneficiaryOutputs = txInfo.outputs.filter(txOut =>
            txOut.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )

        val adaInInputs = beneficiaryInputs
            .map(txInInfo => txInInfo.resolved.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        val adaInOutputs = beneficiaryOutputs
            .map(txOut => txOut.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)

        val expectedOutput =
            requestedAmount + adaInInputs - txInfo.fee

        require(
          adaInOutputs == expectedOutput,
          "Beneficiary output mismatch"
        )

        if requestedAmount == contractAmount then ()
        else require(contractOutputs.length == BigInt(1), "Expected exactly one contract output")

        val contractOutput = contractOutputs.head
        contractOutput.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData == receivedData,
                  "VestingDatum mismatch"
                )
            case _ => fail("Expected inline datum")
    }

object VestingScript {
    val compiled = compile(Vesting.validate)
}
