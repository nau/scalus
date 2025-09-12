package scalus.cardano.ledger.txbuilder
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.TxSigner as CCLSigner
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.txbuilder.TxBuilder.{dummyVkey, modifyBody, modifyWs}
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

case class TxBuilder(
    context: BuilderContext,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer,
    tx: Transaction = TxBuilder.emptyTx,
    backendService: BackendService = null,
    var payer: Address = null,
    signer: CCLSigner = null
) {

    def withBackendService(backendService: BackendService): TxBuilder =
        copy(backendService = backendService)
    def withSigner(signer: CCLSigner): TxBuilder = copy(signer = signer)
    def withPayer(address: Address): TxBuilder = copy(payer = address)

    /** Configures the behaviour that is invoked when the payment transaction produces more ada than
      * it consumes.
      */
    def onSurplus(onSurplus: OnSurplus): TxBuilder = copy(onSurplus = onSurplus)

    /** Specifies the minting information for the transaction, setting a mint field and adding a
      * respective 0 ada output that contains the minted token. The token is sent to the specified
      * address.
      */
    def mint(address: Address, value: Value, script: Script): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx =
            modifyBody(tx, b => b.copy(mint = Some(Mint(value.assets)), outputs = b.outputs :+ out))
        )
    }

    def payTo(address: Address, value: Value) = new PaymentTransactionBuilder(
      context,
      payer,
      address,
      value
    )

    /** Adds a new output which contains the specified value to be sent to the specified address.
      * The output contains the specified data stored as inline datum.
      */
    def payToScript(address: Address, value: Value, datum: Data): TxBuilder = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Inline(datum))))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    /** Adds a new output which contains the specified value to be sent to the specified address.
      * The output contains the specified data hash.
      */
    def payToScript(address: Address, value: Value, datumHash: DataHash): TxBuilder = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Hash(datumHash))))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    /** Configures the slot number. The result transaction is considered valid starting from the
      * specified slot.
      */
    def validFrom(slot: Slot): TxBuilder = {
        copy(tx = modifyBody(tx, b => b.copy(validityStartSlot = Some(slot.slot))))
    }

    /** Appends the specified address key hashes as required signers for this transaction. */
    def withRequiredSigners(signers: Set[AddrKeyHash]): TxBuilder = {
        copy(tx =
            modifyBody(
              tx,
              b =>
                  b.copy(requiredSigners =
                      TaggedOrderedSet(b.requiredSigners.toSortedSet ++ signers)
                  )
            )
        )
    }

    /** Attaches a Plutus script for spending UTXOs from script addresses.
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachSpendingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Spend)

    /** Attaches a Plutus script for minting/burning tokens.
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachMintingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Mint)

    /** Attaches a Plutus script for certificate operations (staking, delegation, etc.).
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachCertScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Cert)

    /** Attaches a Plutus script for reward withdrawal operations.
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachRewardScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Reward)

    /** Attaches a Plutus script for governance voting operations.
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachVotingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Voting)

    /** Attaches a Plutus script for governance proposal operations.
      *
      * @see
      *   [[attachPlutusScript]]
      */
    def attachProposingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Proposing)

    /** Attaches a Plutus script with the specified redeemer tag.
      *
      * This is the general method for attaching Plutus scripts. Use the specific methods like
      * [[attachSpendingScript]], [[attachMintingScript]], etc. for better type safety.
      *
      * @param script
      *   The Plutus script (V1, V2, or V3)
      * @param datum
      *   The datum data for the script
      * @param redeemer
      *   The redeemer data for script execution
      * @param index
      *   The index for the redeemer (context-dependent based on tag)
      * @param tag
      *   The redeemer tag indicating the script's purpose
      */
    def attachPlutusScript(
        script: PlutusScript,
        datum: Data,
        redeemer: Data,
        index: Int,
        tag: RedeemerTag
    ) = {
        val plutusData = TaggedSet.from(
          tx.witnessSet.plutusData.value.toIndexedSeq :+ KeepRaw(datum) :+ KeepRaw(redeemer)
        )
        val updatedWitnessSet = script match {
            case plutusV1: Script.PlutusV1 =>
                tx.witnessSet.copy(
                  plutusV1Scripts = tx.witnessSet.plutusV1Scripts + plutusV1,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
            case plutusV2: Script.PlutusV2 =>
                tx.witnessSet.copy(
                  plutusV2Scripts = tx.witnessSet.plutusV2Scripts + plutusV2,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
            case plutusV3: Script.PlutusV3 =>
                tx.witnessSet.copy(
                  plutusV3Scripts = tx.witnessSet.plutusV3Scripts + plutusV3,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
        }
        copy(tx = tx.copy(witnessSet = updatedWitnessSet))
    }

    /** Attaches a native script (timelock/multisig) to the transaction.
      *
      * Native scripts don't require redeemers or datum, only signatures and timelock validation.
      */
    def attachNativeScript(script: Script.Native, index: Int): TxBuilder = {
        val updatedWitnessSet = tx.witnessSet.copy(
          nativeScripts = tx.witnessSet.nativeScripts + script
        )
        copy(tx = tx.copy(witnessSet = updatedWitnessSet))
    }

    private def addRedeemer(index: Int, redeemerData: Data, tag: RedeemerTag): Redeemers = {
        val newRedeemer = Redeemer(
          tag = tag,
          index = index,
          data = redeemerData,
          exUnits = TxBuilder.dummyExUnits
        )

        tx.witnessSet.redeemers match {
            case Some(existingRedeemers) =>
                val existing = existingRedeemers.value.toIndexedSeq
                val updated =
                    existing.filterNot(r => r.tag == tag && r.index == index) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }

    /** Specifies the exact set of UTXOs to use as transaction inputs.
      *
      * @param inputs
      *   The transaction inputs to use
      */
    def withInputs(inputs: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(inputs = TaggedOrderedSet.from(inputs))))

    /** Selects transaction inputs using the provided selection strategy.
      *
      * @param selectInputs
      *   Strategy for selecting inputs from available UTXOs
      * @see
      *   [[SelectInputs.all]], [[SelectInputs.particular]]
      */
    def selectInputs(selectInputs: SelectInputs): TxBuilder = withInputs(selectInputs(context.utxo))

    /** Specifies collateral inputs for Plutus script transactions.
      *
      * Collateral is required when spending UTXOs locked by Plutus scripts. If script execution
      * fails, collateral is consumed to pay for fees.
      *
      * @param collateralIn
      *   The UTXOs to use as collateral
      */
    def withCollateral(collateralIn: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(collateralInputs = TaggedOrderedSet.from(collateralIn))))

    /** Registers a stake credential, enabling staking operations.
      *
      * Requires a deposit as specified in protocol parameters.
      *
      * @param credential
      *   The stake credential to register
      */
    def registerStake(credential: Credential): TxBuilder = {
        val stakeDeposit = Coin(context.protocolParams.stakeAddressDeposit)
        addCertificate(Certificate.RegCert(credential, Some(stakeDeposit)))
    }

    /** Delegates stake from a registered credential to a stake pool.
      *
      * @param credential
      *   The registered stake credential
      * @param poolKeyHash
      *   The target stake pool's key hash
      */
    def delegateStake(credential: Credential, poolKeyHash: PoolKeyHash): TxBuilder =
        addCertificate(Certificate.StakeDelegation(credential, poolKeyHash))

    /** Registers a stake credential and delegates to a pool in a single certificate.
      *
      * More efficient than separate registration and delegation transactions.
      *
      * @param credential
      *   The stake credential to register and delegate
      * @param poolKeyHash
      *   The target stake pool's key hash
      */
    def registerAndDelegateStake(credential: Credential, poolKeyHash: PoolKeyHash): TxBuilder = {
        val stakeDeposit = Coin(context.protocolParams.stakeAddressDeposit)
        addCertificate(Certificate.StakeRegDelegCert(credential, poolKeyHash, stakeDeposit))
    }

    /** Deregisters a stake credential and returns the deposit.
      *
      * @param credential
      *   The stake credential to deregister
      */
    def deregisterStake(credential: Credential): TxBuilder = {
        val stakeDeposit = Coin(context.protocolParams.stakeAddressDeposit)
        addCertificate(Certificate.UnregCert(credential, Some(stakeDeposit)))
    }

    /** Delegates voting power to a DRep (Delegated Representative) for governance.
      *
      * @param credential
      *   The stake credential delegating voting power
      * @param drep
      *   The target DRep to delegate to
      */
    def delegateToVote(credential: Credential, drep: DRep): TxBuilder =
        addCertificate(Certificate.VoteDelegCert(credential, drep))

    /** Delegates both stake (to a pool) and voting power (to a DRep) in a single certificate.
      *
      * @param credential
      *   The registered stake credential
      * @param poolKeyHash
      *   The target stake pool's key hash
      * @param drep
      *   The target DRep for voting delegation
      */
    def delegateStakeAndVote(
        credential: Credential,
        poolKeyHash: PoolKeyHash,
        drep: DRep
    ): TxBuilder =
        addCertificate(Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep))

    /** Registers a stake credential and delegates both stake and voting power in a single
      * certificate.
      *
      * Most efficient way to perform all three operations (register, delegate stake, delegate
      * vote).
      *
      * @param credential
      *   The stake credential to register and delegate
      * @param poolKeyHash
      *   The target stake pool's key hash
      * @param drep
      *   The target DRep for voting delegation
      */
    def registerAndDelegateStakeAndVote(
        credential: Credential,
        poolKeyHash: PoolKeyHash,
        drep: DRep
    ): TxBuilder = {
        val stakeDeposit = Coin(context.protocolParams.stakeAddressDeposit)
        addCertificate(
          Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, stakeDeposit)
        )
    }

    /** Withdraws staking rewards from multiple reward accounts.
      *
      * @param withdrawals
      *   Map of reward accounts to amounts to withdraw
      */
    def withdrawRewards(withdrawals: Withdrawals): TxBuilder =
        copy(tx = modifyBody(tx, b => b.copy(withdrawals = Some(withdrawals))))

    /** Withdraws staking rewards from a single reward account.
      *
      * @param rewardAccount
      *   The reward account to withdraw from
      * @param amount
      *   The amount to withdraw
      */
    def withdrawRewards(rewardAccount: RewardAccount, amount: Coin): TxBuilder =
        withdrawRewards(Withdrawals(SortedMap(rewardAccount -> amount)))

    private def addCertificate(certificate: Certificate): TxBuilder = {
        val updatedCertificates =
            TaggedSet.from(tx.body.value.certificates.toIndexedSeq :+ certificate)
        copy(tx = modifyBody(tx, b => b.copy(certificates = updatedCertificates)))
    }

    private def addDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses + dummyVkey))

    private def removeDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses - dummyVkey))

    /** Builds the final transaction with automatic balancing, fee calculation, and signing.
      *
      * This method:
      *   1. Adds temporary dummy witnesses for fee calculation
      *   2. Balances inputs/outputs and calculates fees
      *   3. Evaluates Plutus scripts if present and updates execution units
      *   4. Signs the transaction with provided signing keys
      *   5. Validates the final transaction against configured validators
      *
      * @return
      *   The complete, balanced, and signed transaction ready for submission
      * @throws TransactionException
      *   if the transaction cannot be balanced or validated
      */
    def build: Transaction = {
        val withDummyVkey = addDummyVkey(tx)
        val balanced = TxBalance.doBalance(withDummyVkey)(
          context.utxoProvider.utxo,
          context.protocolParams,
          onSurplus
        )
        context.validate(balanced).toTry.get
    }


    private def isPlutusScriptTx(transaction: Transaction): Boolean =
        transaction.witnessSet.plutusV1Scripts.size +
            transaction.witnessSet.plutusV2Scripts.size +
            transaction.witnessSet.plutusV3Scripts.size > 0
}

object TxBuilder {
    // will be updated during balancing
    private val dummyExUnits = ExUnits(memory = 0L, steps = 0L)

    // needed during fee calculation and prior to signing
    private val dummyVkey = VKeyWitness(
      ByteString.fromString("0".repeat(32)),
      ByteString.fromString("0".repeat(64))
    )

    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody) = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }

    def modifyWs(
        tx: Transaction,
        f: TransactionWitnessSet => TransactionWitnessSet
    ): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }

    val emptyTx: Transaction = Transaction(
      TransactionBody(TaggedOrderedSet.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    def withInputsFromUtxos(utxo: UTxO) = Transaction(
      TransactionBody(TaggedOrderedSet.from(utxo.keySet), IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )
}
