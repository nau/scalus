package scalus.cardano.ledger.txbuilder

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network}
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, *}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.api.Timelock.Signature
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

import scala.collection.immutable.SortedMap

/** This suite contains exemplary use cases of the transaction builder.
  *
  * Invariants are checked more thoroughly in the [[TxBuilderTest]].
  *
  * The goal is to have the following examples: ADA or/and CNT transfer Multiple recipients Datum
  * hash/inline Mint/Burn Native Script Spending Validator with 1/N params
  */
class TxBuilderComprehensiveTest extends AnyFunSuite with ArbAddresses with ArbLedger {

    val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    private val costModels = CostModels.fromProtocolParams(params)
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.enormous,
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = costModels
    )

    private def context(utxo: UTxO): BuilderContext =
        BuilderContext(params, evaluator, Network.Mainnet, UtxoProvider.from(utxo))

    test("Simple ADA Transfer") {
        val sender = arbitrary[Address].sample.get
        val recipient = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(sender, Value.lovelace(10_000_000L))
        )

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .payTo(recipient, Value.lovelace(5_000_000L))
            .build

        assert(tx.body.value.outputs.exists(_.value.address == recipient))
        assert(
          tx.body.value.outputs.exists(o =>
              o.value.address == recipient && o.value.value.coin.value == 5_000_000L
          )
        )
    }

    test("Multiple Recipients") {
        val sender = arbitrary[Address].sample.get
        val alice = arbitrary[Address].sample.get
        val bob = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(sender, Value.lovelace(20_000_000L))
        )

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .payTo(alice, Value.lovelace(5_000_000L))
            .payTo(bob, Value.lovelace(5_000_000L))
            .build

        val outputs = tx.body.value.outputs
        assert(
          outputs.exists(o => o.value.address == alice && o.value.value.coin.value == 5_000_000L)
        )
        assert(
          outputs.exists(o => o.value.address == bob && o.value.value.coin.value == 5_000_000L)
        )
    }

    test("Mint Native Tokens") {
        val minter = arbitrary[Address].sample.get
        val recipient = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val policyId = arbitrary[ScriptHash].sample.get
        val assetName = AssetName(ByteString.fromString("60065"))
        val mintValue = Value(
          Coin.zero,
          MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 100L)))
        )

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(minter, Value.lovelace(10_000_000L))
        )

        val mintingScript = Script.Native(Signature(arbitrary[AddrKeyHash].sample.get))

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .mint(recipient, mintValue, mintingScript)
            .build

        assert(tx.body.value.mint.isDefined)
        val mint = tx.body.value.mint.get
        assert(mint.assets(policyId)(assetName) == 100L)

        // Verify recipient gets the tokens
        assert(
          tx.body.value.outputs.exists(o =>
              o.value.address == recipient &&
                  o.value.value.assets.assets.get(policyId).exists(_.get(assetName).contains(100L))
          )
        )
    }

    test("Spend with Script") {
        pending
        val scriptAddress = arbitrary[Address].sample.get
        val beneficiary = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val datum = Data.unit
        val redeemer = Data.unit

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            scriptAddress,
            Value.lovelace(10_000_000L),
            Some(DatumOption.Inline(datum))
          )
        )

        val plutusScript = arbitrary[Script.PlutusV2].sample.get

        val tx = context(utxo).buildNewTx
            .withInputs(Set(TransactionInput(hash, 0)))
            .payTo(beneficiary, Value.lovelace(8_000_000L))
            .attachSpendingScript(plutusScript, datum, redeemer, 0)
            .build

        assert(tx.witnessSet.plutusV2Scripts.contains(plutusScript))
        assert(tx.witnessSet.redeemers.isDefined)
        assert(tx.body.value.outputs.exists(_.value.address == beneficiary))
    }

    test("Register and Delegate Stake") {
        val user = arbitrary[Address].sample.get
        val stakeCredential = arbitrary[Credential].sample.get
        val poolKeyHash = arbitrary[PoolKeyHash].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(user, Value.lovelace(10_000_000L))
        )

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .registerAndDelegateStake(stakeCredential, poolKeyHash)
            .build

        assert(tx.body.value.certificates.toIndexedSeq.size == 1)
        tx.body.value.certificates.toIndexedSeq.head match {
            case Certificate.StakeRegDelegCert(cred, pool, deposit) =>
                assert(cred == stakeCredential)
                assert(pool == poolKeyHash)
                assert(deposit.value == params.stakeAddressDeposit)
            case cert => fail(s"Expected StakeRegDelegCert, got $cert")
        }
    }

    test("Withdraw Rewards") {
        val user = arbitrary[Address].sample.get
        val rewardAccount = arbitrary[RewardAccount].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(user, Value.lovelace(5_000_000L))
        )

        val rewardAmount = Coin(500_000L)

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .withdrawRewards(rewardAccount, rewardAmount)
            .build

        assert(tx.body.value.withdrawals.isDefined)
        val withdrawals = tx.body.value.withdrawals.get
        assert(withdrawals.withdrawals(rewardAccount) == rewardAmount)
    }

    test("Vote Delegation") {
        val voter = arbitrary[Address].sample.get
        val stakeCredential = arbitrary[Credential].sample.get
        val drep = arbitrary[DRep].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(voter, Value.lovelace(5_000_000L))
        )

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .delegateToVote(stakeCredential, drep)
            .build

        assert(tx.body.value.certificates.toIndexedSeq.size == 1)
        tx.body.value.certificates.toIndexedSeq.head match {
            case Certificate.VoteDelegCert(cred, delegatedDrep) =>
                assert(cred == stakeCredential)
                assert(delegatedDrep == drep)
            case cert => fail(s"Expected VoteDelegCert, got $cert")
        }
    }

    test("Combined Transaction - Payment, Minting, and Staking") {
        pending
        val user = arbitrary[Address].sample.get
        val recipient = arbitrary[Address].sample.get
        val stakeCredential = arbitrary[Credential].sample.get
        val poolKeyHash = arbitrary[PoolKeyHash].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val policyId = arbitrary[ScriptHash].sample.get
        val assetName = AssetName(ByteString.fromHex("546f6b656e"))
        val mintValue =
            Value(Coin.zero, MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 50L))))

        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(user, Value.lovelace(50_000_000L))
        )

        val mintingScript = Script.Native(Signature(arbitrary[AddrKeyHash].sample.get))

        val tx = context(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .payTo(recipient, Value.lovelace(10_000_000L))
            .mint(recipient, mintValue, mintingScript)
            .registerAndDelegateStake(stakeCredential, poolKeyHash)
            .build

        // Verify payment
        assert(
          tx.body.value.outputs.exists(o =>
              o.value.address == recipient && o.value.value.coin.value >= 10_000_000L
          )
        )

        // Verify minting
        assert(tx.body.value.mint.isDefined)
        assert(tx.body.value.mint.get.assets(policyId)(assetName) == 50L)

        // Verify staking
        assert(tx.body.value.certificates.toIndexedSeq.size == 1)
        assert(tx.body.value.certificates.toIndexedSeq.isInstanceOf[Certificate.StakeRegDelegCert])

        // Verify script attachment
        assert(tx.witnessSet.nativeScripts.contains(mintingScript))
    }
}
