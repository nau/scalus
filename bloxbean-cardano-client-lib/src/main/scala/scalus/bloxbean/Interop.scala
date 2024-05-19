package scalus.bloxbean

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.address.AddressType
import com.bloxbean.cardano.client.address.Credential
import com.bloxbean.cardano.client.address.CredentialType
import com.bloxbean.cardano.client.crypto.Blake2bUtil.blake2bHash224
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import scalus.builtin.ByteString
import scalus.builtin.given
import scalus.builtin.Data
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.api.v1.DCert
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v1.StakingCredential
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.v1
import scalus.ledger.api.v2
import scalus.ledger.babbage.PlutusV1Params
import scalus.ledger.babbage.PlutusV2Params
import scalus.prelude
import scalus.prelude.AssocMap
import scalus.uplc.eval.*

import java.math.BigInteger
import java.util
import java.util.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.BigInt

given Ordering[TransactionInput] with
    def compare(x: TransactionInput, y: TransactionInput): Int =
        x.getTransactionId.compareTo(y.getTransactionId) match
            case 0 => x.getIndex.compareTo(y.getIndex)
            case c => c

given Ordering[v1.StakingCredential.StakingHash] = Ordering.by { cred =>
    cred.cred match
        case v1.Credential.PubKeyCredential(pkh)  => pkh.hash
        case v1.Credential.ScriptCredential(hash) => hash
}

given Ordering[Redeemer] with
    def compare(x: Redeemer, y: Redeemer): Int =
        x.getTag.value.compareTo(y.getTag.value) match
            case 0 => x.getIndex.compareTo(y.getIndex)
            case c => c

enum ExecutionPurpose:
    def scriptHash: ByteString
    case WithDatum(
        scriptVersion: PlutusLedgerLanguage,
        scriptHash: ByteString,
        script: VM.ScriptForEvaluation,
        datum: Data
    )

    case NoDatum(
        scriptVersion: PlutusLedgerLanguage,
        scriptHash: ByteString,
        script: VM.ScriptForEvaluation
    )

/** Interoperability between Cardano Client Lib and Scalus */
object Interop {
    /// Helper for null check
    extension [A](inline a: A) inline infix def ??(b: A): A = if a != null then a else b

    /** Converts Cardano Client Lib's [[PlutusData]] to Scalus' [[Data]] */
    def toScalusData(datum: PlutusData): Data = {
        datum match
            case c: ConstrPlutusData =>
                val constr = c.getAlternative
                val args = c.getData.getPlutusDataList.asScala.map(toScalusData).toList
                Data.Constr(c.getAlternative, args)
            case m: MapPlutusData =>
                val values = m.getMap.asScala.map { case (k, v) =>
                    (toScalusData(k), toScalusData(v))
                }.toList
                Data.Map(values)
            case l: ListPlutusData =>
                val values = l.getPlutusDataList.asScala.map(toScalusData).toList
                Data.List(values)
            case i: BigIntPlutusData =>
                Data.I(i.getValue)
            case b: BytesPlutusData =>
                Data.B(ByteString.fromArray(b.getValue))
    }

    /** Converts Scalus' [[Data]] to Cardano Client Lib's [[PlutusData]] */
    def toPlutusData(data: Data): PlutusData = {
        data match
            case Data.Constr(tag, args) =>
                val convertedArgs = ListPlutusData
                    .builder()
                    .plutusDataList(args.map(toPlutusData).asJava)
                    .build()
                ConstrPlutusData
                    .builder()
                    .alternative(tag)
                    .data(convertedArgs)
                    .build()
            case Data.Map(items) =>
                MapPlutusData
                    .builder()
                    .map(
                      items
                          .map { case (k, v) =>
                              (toPlutusData(k), toPlutusData(v))
                          }
                          .toMap
                          .asJava
                    )
                    .build()
            case Data.List(items) =>
                ListPlutusData
                    .builder()
                    .plutusDataList(items.map(toPlutusData).asJava)
                    .build()
            case Data.I(i) =>
                BigIntPlutusData.of(i.bigInteger)
            case Data.B(b) =>
                BytesPlutusData.of(b.bytes)
    }

    /** Creates [[MachineParams]] from a [[CostMdls]] and a [[PlutusLedgerLanguage]] */
    def translateMachineParamsFromCostMdls(
        costMdls: CostMdls,
        plutus: PlutusLedgerLanguage
    ): MachineParams = {
        import upickle.default.*
        val paramsMap = plutus match
            case PlutusLedgerLanguage.PlutusV1 =>
                val costs = costMdls.get(Language.PLUTUS_V1)
                val params = PlutusV1Params.fromSeq(costs.getCosts.map(_.toInt).toSeq)
                writeJs(params).obj.map { case (k, v) => (k, v.num.toInt) }.toMap
            case PlutusLedgerLanguage.PlutusV2 =>
                val costs = costMdls.get(Language.PLUTUS_V2)
                val params = PlutusV2Params.fromSeq(costs.getCosts.map(_.toInt).toSeq)
                writeJs(params).obj.map { case (k, v) => (k, v.num.toInt) }.toMap
            case PlutusLedgerLanguage.PlutusV3 =>
                throw new NotImplementedError("PlutusV3 not supported yet")

        val builtinCostModel = BuiltinCostModel.fromCostModelParams(paramsMap)
        val machineCosts = CekMachineCosts.fromMap(paramsMap)
        MachineParams(machineCosts = machineCosts, builtinCostModel = builtinCostModel)
    }

    def getCredential(cred: Credential): v1.Credential = {
        cred.getType match
            case CredentialType.Key =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getBytes)))
            case CredentialType.Script =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getBytes))
    }

    def getStakingCredential(cred: StakeCredential): v1.StakingCredential = {
        cred.getType match
            case StakeCredType.ADDR_KEYHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getHash)))
                )
            case StakeCredType.SCRIPTHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.ScriptCredential(ByteString.fromArray(cred.getHash))
                )
    }

    def getStakingCredential(cred: Credential): v1.StakingCredential = {
        v1.StakingCredential.StakingHash(getCredential(cred))
    }

    def getAddress(address: Address): v1.Address = {
        val cred = address.getPaymentCredential.map(getCredential).get
        val staking = address.getDelegationCredential
            .map(cred => prelude.Maybe.Just(getStakingCredential(cred)))
            .orElse(prelude.Maybe.Nothing)
        v1.Address(cred, staking)
    }

    def getTxInInfoV1(
        input: TransactionInput,
        utxos: util.Set[ResolvedInput]
    ): v1.TxInInfo = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.input.getTransactionId == input.getTransactionId && utxo.input.getIndex == input.getIndex
            )
            .map(_.output)
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        val maybeDatumHash =
            if out.getDatumHash != null then
                prelude.Maybe.Just(ByteString.fromArray(out.getDatumHash))
            else prelude.Maybe.Nothing
        v1.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v1.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            maybeDatumHash
          )
        )
    }

    def getTxInInfoV2(
        input: TransactionInput,
        utxos: util.Set[ResolvedInput]
    ): v2.TxInInfo = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.input.getTransactionId == input.getTransactionId && utxo.input.getIndex == input.getIndex
            )
            .map(_.output)
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        v2.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v2.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            getOutputDatum(out),
            if out.getScriptRef != null
            then prelude.Maybe.Just(ByteString.fromArray(blake2bHash224(out.getScriptRef)))
            else prelude.Maybe.Nothing
          )
        )
    }

    def getValue(value: Value): v1.Value = {
        val ma = getValue(value.getMultiAssets)
        if value.getCoin != null then
            val lovelace = v1.Value.lovelace(value.getCoin)
            prelude.AssocMap(
              prelude.List.Cons(
                (ByteString.empty, AssocMap.singleton(ByteString.empty, BigInt(value.getCoin))),
                ma.inner
              )
            )
        else ma
    }

    def getValue(value: util.List[MultiAsset]): v1.Value = {
        // get sorted multi assets
        val multi = mutable.TreeMap.empty[ByteString, mutable.TreeMap[ByteString, BigInt]]
        for m <- value.asScala do
            val assets = mutable.TreeMap.empty[ByteString, BigInt]
            for asset <- m.getAssets.asScala do
                assets.put(ByteString.fromArray(asset.getNameAsBytes), asset.getValue)
            multi.put(ByteString.fromHex(m.getPolicyId), assets)
        // convert to AssocMap
        val am =
            for (policyId, assets) <- multi.iterator
            yield policyId -> AssocMap(prelude.List.from(assets))

        prelude.AssocMap(prelude.List.from(am))
    }

    def getTxOutV1(out: TransactionOutput): v1.TxOut = {
        val addr = Address(out.getAddress)
        val maybeDatumHash =
            if out.getDatumHash != null then
                prelude.Maybe.Just(ByteString.fromArray(out.getDatumHash))
            else prelude.Maybe.Nothing
        v1.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          maybeDatumHash
        )
    }

    def getTxOutV2(out: TransactionOutput): v2.TxOut = {
        val addr = Address(out.getAddress)
        v2.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          getOutputDatum(out),
          if out.getScriptRef != null then
              prelude.Maybe.Just(ByteString.fromArray(out.getScriptRef))
          else prelude.Maybe.Nothing
        )
    }

    def getOutputDatum(out: TransactionOutput): v2.OutputDatum = {
        if out.getDatumHash != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromArray(out.getDatumHash))
        else if out.getInlineDatum != null then
            v2.OutputDatum.OutputDatum(toScalusData(out.getInlineDatum))
        else v2.OutputDatum.NoOutputDatum
    }

    def slotToBeginPosixTime(slot: Long, sc: SlotConfig): Long = {
        val msAfterBegin = (slot - sc.zero_slot) * sc.slot_length
        sc.zero_time + msAfterBegin
    }

    def getInterval(tx: Transaction, slotConfig: SlotConfig): v1.Interval = {
        val validFrom = tx.getBody.getValidityStartInterval
        val lower =
            if validFrom == 0 then v1.Interval.negInf
            else v1.Interval.finite(BigInt(slotToBeginPosixTime(validFrom, slotConfig)))
        val validTo = tx.getBody.getTtl
        val upper =
            if validTo == 0 then v1.Interval.posInf
            else
                v1.IntervalBound(
                  v1.IntervalBoundType.Finite(BigInt(slotToBeginPosixTime(validTo, slotConfig))),
                  false // Closure is false here, this is how Cardano Ledger does it for upper bound
                )
        v1.Interval(lower, upper)
    }

    def getWithdrawals(
        withdrawals: util.List[Withdrawal]
    ): prelude.List[(v1.StakingCredential, BigInt)] = {
        // get sorted withdrawals
        val wdwls = mutable.TreeMap.empty[v1.StakingCredential.StakingHash, BigInt]
        for w <- withdrawals.asScala do
            val addr = Address(w.getRewardAddress)
            val cred = addr.getDelegationCredential.map(getCredential).get
            wdwls.put(v1.StakingCredential.StakingHash(cred), BigInt(w.getCoin))
        prelude.List.from(wdwls)
    }

    def getDCert(cert: Certificate): v1.DCert = {
        cert match
            case c: StakeRegistration =>
                v1.DCert.DelegRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDeregistration =>
                v1.DCert.DelegDeRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDelegation =>
                v1.DCert.DelegDelegate(
                  getStakingCredential(c.getStakeCredential),
                  v1.PubKeyHash(ByteString.fromArray(c.getStakePoolId.getPoolKeyHash))
                )
            case c: PoolRegistration =>
                v1.DCert.PoolRegister(
                  v1.PubKeyHash(ByteString.fromArray(c.getOperator)),
                  v1.PubKeyHash(ByteString.fromArray(c.getVrfKeyHash))
                )
            case c: PoolRetirement =>
                v1.DCert.PoolRetire(
                  v1.PubKeyHash(ByteString.fromArray(c.getPoolKeyHash)),
                  BigInt(c.getEpoch)
                )
            case c: GenesisKeyDelegation => v1.DCert.Genesis
            case c: MoveInstataneous     => v1.DCert.Mir
    }

    def getTxInfoV1(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        slotConfig: SlotConfig
    ): v1.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val datums = tx.getWitnessSet.getPlutusDataList ?? util.List.of()
        v1.TxInfo(
          // sorted inputs
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV1(_, utxos))),
          // outputs as in the transaction
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV1)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          // sorted mint values
          mint = getValue(body.getMint ?? util.List.of()),
          // certificates as is
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = getWithdrawals(body.getWithdrawals ?? util.List.of()),
          validRange = getInterval(tx, slotConfig),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          data = prelude.List.from(
            datums.asScala
                .map { data =>
                    val hash = ByteString.fromArray(data.getDatumHashAsBytes)
                    hash -> toScalusData(data)
                }
                .sortBy(_._1)
          ),
          id = v1.TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
        )
    }

    def getTxInfoV2(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        slotConfig: SlotConfig
    ): v2.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val datums = tx.getWitnessSet.getPlutusDataList ?? util.List.of()
        v2.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))),
          referenceInputs = prelude.List.from(
            body.getReferenceInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))
          ),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          mint = getValue(body.getMint ?? util.List.of()),
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = AssocMap(getWithdrawals(body.getWithdrawals ?? util.List.of())),
          validRange = getInterval(tx, slotConfig),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          redeemers = AssocMap(prelude.List.from(rdmrs.asScala.sorted.map { redeemer =>
              val purpose = getScriptPurpose(
                redeemer,
                body.getInputs,
                body.getMint,
                body.getCerts,
                body.getWithdrawals
              )
              purpose -> toScalusData(redeemer.getData)
          })),
          data = AssocMap(
            prelude.List.from(
              datums.asScala
                  .map { data =>
                      val hash = ByteString.fromArray(data.getDatumHashAsBytes)
                      hash -> toScalusData(data)
                  }
                  .sortBy(_._1)
            )
          ),
          id = v1.TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
        )
    }

    def getTxOutRefV1(input: TransactionInput) = {
        v1.TxOutRef(
          v1.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    def getScriptPurpose(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose =
        // Cardano Ledger code is stupidly complex and unreadable. We need to make sure this is correct
        val index = redeemer.getIndex.intValue
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sorted
                if ins.isDefinedAt(index) then
                    val input = ins(index)
                    v1.ScriptPurpose.Spending(getTxOutRefV1(input))
                else throw new IllegalStateException(s"Input not found: $index in $inputs")
            case RedeemerTag.Mint =>
                val policyIds = mint.asScala.map(_.getPolicyId).sorted
                if policyIds.isDefinedAt(index) then
                    v1.ScriptPurpose.Minting(ByteString.fromHex(policyIds(index)))
                else throw new IllegalStateException(s"Wrong mint index: $index in $mint")
            case RedeemerTag.Cert =>
                val certs = certificates.asScala
                if certs.isDefinedAt(index) then v1.ScriptPurpose.Certifying(getDCert(certs(index)))
                else throw new IllegalStateException(s"Wrong cert index: $index in $certificates")
            case RedeemerTag.Reward =>
                val rewardAccounts = withdrawals.asScala
                    .map(ra => Address(ra.getRewardAddress))
                    .sortBy(a => ByteString.fromArray(a.getBytes)) // for ordering
                if rewardAccounts.isDefinedAt(index) then
                    val address = rewardAccounts(index)
                    if address.getAddressType == AddressType.Reward then
                        val cred = getCredential(address.getDelegationCredential.get)
                        v1.ScriptPurpose.Rewarding(v1.StakingCredential.StakingHash(cred))
                    else
                        throw new IllegalStateException(
                          s"Wrong reward address type: $address in $withdrawals"
                        )
                else throw new IllegalStateException(s"Wrong reward index: $index in $withdrawals")
}
