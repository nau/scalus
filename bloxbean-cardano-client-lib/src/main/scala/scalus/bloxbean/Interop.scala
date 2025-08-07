package scalus.bloxbean

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.address.AddressType
import com.bloxbean.cardano.client.address.Credential
import com.bloxbean.cardano.client.address.CredentialType
import com.bloxbean.cardano.client.crypto.Blake2bUtil.blake2bHash224
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.spec.Rational
import com.bloxbean.cardano.client.spec.UnitInterval
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.*
import com.bloxbean.cardano.client.transaction.spec.governance.DRep
import com.bloxbean.cardano.client.transaction.spec.governance.DRepType
import com.bloxbean.cardano.client.transaction.spec.governance.ProposalProcedure
import com.bloxbean.cardano.client.transaction.spec.governance.Vote
import com.bloxbean.cardano.client.transaction.spec.governance.Voter
import com.bloxbean.cardano.client.transaction.spec.governance.VoterType
import com.bloxbean.cardano.client.transaction.spec.governance.VotingProcedure
import com.bloxbean.cardano.client.transaction.spec.governance.VotingProcedures
import com.bloxbean.cardano.client.transaction.spec.governance.actions.GovAction
import com.bloxbean.cardano.client.transaction.spec.governance.actions.GovActionId
import com.bloxbean.cardano.client.transaction.spec.governance.actions.HardForkInitiationAction
import com.bloxbean.cardano.client.transaction.spec.governance.actions.InfoAction
import com.bloxbean.cardano.client.transaction.spec.governance.actions.NewConstitution
import com.bloxbean.cardano.client.transaction.spec.governance.actions.NoConfidence
import com.bloxbean.cardano.client.transaction.spec.governance.actions.ParameterChangeAction
import com.bloxbean.cardano.client.transaction.spec.governance.actions.TreasuryWithdrawalsAction
import com.bloxbean.cardano.client.transaction.spec.governance.actions.UpdateCommittee
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import io.bullet.borer.Cbor
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.ToData
import scalus.builtin.Data.toData
import scalus.builtin.Pair
import scalus.cardano.ledger.Script
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.v1
import scalus.ledger.api.v1.DCert
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v1.StakingCredential
import scalus.ledger.api.v2
import scalus.ledger.api.v3
import scalus.ledger.api.v3.GovernanceActionId
import scalus.ledger.babbage.PlutusV1Params
import scalus.ledger.babbage.PlutusV2Params
import scalus.ledger.babbage.PlutusV3Params
import scalus.prelude
import scalus.prelude.SortedMap
import scalus.prelude.List
import scalus.prelude.asScalus
import scalus.uplc.eval.*

import java.math.BigInteger
import java.util
import scala.collection.{immutable, mutable}
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

@deprecated("Use Script instead", "0.10.1")
case class ScriptInfo(hash: ByteString, scriptVersion: ScriptVersion)

/** Interoperability between Cardano Client Lib and Scalus */
object Interop {
    given Ordering[Voter] with {
        def compare(x: Voter, y: Voter): Int =
            x.getType.compareTo(y.getType) match
                case 0 =>
                    util.Arrays.compareUnsigned(
                      x.getCredential.getBytes,
                      y.getCredential.getBytes
                    )
                case c => c
    }

    given Ordering[GovActionId] with {
        def compare(x: GovActionId, y: GovActionId): Int =
            x.getTransactionId.compareTo(y.getTransactionId) match
                case 0 => x.getGovActionIndex.compareTo(y.getGovActionIndex)
                case c => c
    }

    given ToData[BigInteger] = (x: BigInteger) => iData(x)
    given ToData[Integer] = (x: Integer) => iData(BigInt(x))
    given ToData[java.lang.Long] = (x: java.lang.Long) => iData(BigInt(x))

    given ToData[ProtocolParamUpdate] =
        given ToData[Rational] = (x: Rational) =>
            listData(builtin.List(x.getNumerator.toData, x.getDenominator.toData))
        given ToData[UnitInterval] = (x: UnitInterval) =>
            listData(builtin.List(x.getNumerator.toData, x.getDenominator.toData))
        given ToData[ExUnitPrices] = (x: ExUnitPrices) =>
            listData(builtin.List(x.getMemPrice.toData, x.getStepPrice.toData))
        given ToData[ExUnits] = (x: ExUnits) =>
            listData(builtin.List(x.getMem.toData, x.getSteps.toData))

        (x: ProtocolParamUpdate) => {
            val params = mutable.ArrayBuffer.empty[Pair[Data, Data]]
            def add[A: ToData](idx: Int, value: A): Unit =
                if value != null then params.append(Pair(iData(idx), value.toData))

            add(0, x.getMinFeeA)
            add(1, x.getMinFeeB)
            add(2, x.getMaxBlockSize)
            add(3, x.getMaxTxSize)
            add(4, x.getMaxBlockHeaderSize)
            add(5, x.getKeyDeposit)
            add(6, x.getPoolDeposit)
            add(7, x.getMaxEpoch)
            add(8, x.getNOpt)
            add(9, x.getPoolPledgeInfluence)
            add(10, x.getExpansionRate)
            add(11, x.getTreasuryGrowthRate)
            add(16, x.getMinPoolCost)
            add(17, x.getAdaPerUtxoByte)
            // FIXME: implement 18 x.getCostModels
            add(19, x.getExecutionCosts)
            add(20, x.getMaxTxExUnits)
            add(21, x.getMaxBlockExUnits)
            add(22, x.getMaxValSize)
            add(23, x.getCollateralPercent)
            add(24, x.getMaxCollateralInputs)
            // FIXME: add missing fields when they are implemented in the client lib
            mapData(builtin.List.from(params))
        }

    /// Helper for null check
    extension [A](inline a: A) inline infix def ??(b: => A): A = if a != null then a else b

    @deprecated("Use getScriptFromScriptRefBytes", "0.10.1")
    def getScriptInfoFromScriptRef(scriptRef: Array[Byte]): ScriptInfo = {
        // script_ref is encoded as CBOR Array
        val (scriptType, scriptCbor) = Cbor.decode(scriptRef).to[(Byte, Array[Byte])].value
        // and script hash is calculated from the script type byte and the scriptCbor
        val scriptBytesForScriptHash = Array(scriptType) ++ scriptCbor
        val hash = ByteString.fromArray(blake2bHash224(scriptBytesForScriptHash))
        scriptType match
            case 0 =>
                ScriptInfo(hash, ScriptVersion.Native)
            case 1 => // Plutus V1
                val script = ByteString.fromArray(Cbor.decode(scriptCbor).to[Array[Byte]].value)
                ScriptInfo(hash, ScriptVersion.PlutusV1(script))
            case 2 => // Plutus V2
                val script = ByteString.fromArray(Cbor.decode(scriptCbor).to[Array[Byte]].value)
                ScriptInfo(hash, ScriptVersion.PlutusV2(script))
            case 3 => // Plutus V3
                val script = ByteString.fromArray(Cbor.decode(scriptCbor).to[Array[Byte]].value)
                ScriptInfo(hash, ScriptVersion.PlutusV3(script))
    }

    private[bloxbean] def getScriptFromScriptRefBytes(scriptRefBytes: Array[Byte]): Script = {
        Cbor.decode(scriptRefBytes).to[Script].value
    }

    /** Converts Cardano Client Lib's [[PlutusData]] to Scalus' [[Data]] */
    extension (datum: PlutusData)
        def toScalusData: Data = {
            datum match
                case c: ConstrPlutusData =>
                    val args = c.getData.getPlutusDataList.asScala.map(_.toScalusData).toList
                    Data.Constr(c.getAlternative, args)
                case m: MapPlutusData =>
                    val values = m.getMap.asScala.map { case (k, v) =>
                        (k.toScalusData, v.toScalusData)
                    }.toList
                    Data.Map(values)
                case l: ListPlutusData =>
                    val values = l.getPlutusDataList.asScala.map(_.toScalusData).toList
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
    @deprecated("Use the version with MajorProtocolVersion", "0.9.0")
    def translateMachineParamsFromCostMdls(
        costMdls: CostMdls,
        plutus: PlutusLedgerLanguage,
        protocolVersion: api.ProtocolVersion
    ): MachineParams = {
        translateMachineParamsFromCostMdls(
          costMdls,
          plutus,
          api.MajorProtocolVersion(protocolVersion.major)
        )
    }

    /** Creates [[MachineParams]] from a [[CostMdls]] and a [[PlutusLedgerLanguage]] */
    def translateMachineParamsFromCostMdls(
        costMdls: CostMdls,
        plutus: PlutusLedgerLanguage,
        protocolVersion: api.MajorProtocolVersion
    ): MachineParams = {
        import scalus.cardano.ledger.Language as L
        val (lang, params) = plutus match
            case PlutusLedgerLanguage.PlutusV1 =>
                val costs = costMdls.get(Language.PLUTUS_V1)
                L.PlutusV1 -> PlutusV1Params.fromSeq(
                  immutable.ArraySeq.unsafeWrapArray(costs.getCosts)
                )
            case PlutusLedgerLanguage.PlutusV2 =>
                val costs = costMdls.get(Language.PLUTUS_V2)
                L.PlutusV2 -> PlutusV2Params.fromSeq(
                  immutable.ArraySeq.unsafeWrapArray(costs.getCosts)
                )
            case PlutusLedgerLanguage.PlutusV3 =>
                val costs = costMdls.get(Language.PLUTUS_V3)
                L.PlutusV3 -> PlutusV3Params.fromSeq(
                  immutable.ArraySeq.unsafeWrapArray(costs.getCosts)
                )

        val semvar = BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
          protocolVersion,
          plutus
        )
        val builtinCostModel = BuiltinCostModel.fromPlutusParams(params, lang, semvar)
        val machineCosts = CekMachineCosts.fromPlutusParams(params)
        MachineParams(
          machineCosts = machineCosts,
          builtinCostModel = builtinCostModel,
          semanticVariant = semvar
        )
    }

    def getCredential(cred: Credential): v1.Credential = {
        cred.getType match
            case CredentialType.Key =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getBytes)))
            case CredentialType.Script =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getBytes))
    }

    def getCredential(cred: StakeCredential): v1.Credential = {
        cred.getType match
            case StakeCredType.ADDR_KEYHASH =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getHash)))
            case StakeCredType.SCRIPTHASH =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getHash))
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
            .map(cred => prelude.Option.Some(getStakingCredential(cred)))
            .orElse(prelude.Option.None)
        v1.Address(cred, staking)
    }

    def getTxInInfoV1(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v1.TxInInfo = {
        val out = utxos.getOrElse(input, throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        val optionDatumHash =
            if out.getDatumHash != null then
                prelude.Option.Some(ByteString.fromArray(out.getDatumHash))
            else prelude.Option.None
        v1.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v1.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            optionDatumHash
          )
        )
    }

    def getTxInInfoV2(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v2.TxInInfo = {
        val out = utxos.getOrElse(input, throw new IllegalStateException("Input Not Found"))
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
            then prelude.Option.Some(getScriptFromScriptRefBytes(out.getScriptRef).scriptHash)
            else prelude.Option.None
          )
        )
    }

    def getValue(value: Value): v1.Value = {
        val ma = getValue(value.getMultiAssets)
        if value.getCoin != null then
            val lovelace = v1.Value.lovelace(value.getCoin)
            lovelace + ma
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

        v1.Value.fromList(
          prelude.List.from(
            for (policyId, assets) <- multi.iterator
            yield policyId -> prelude.List.from(assets)
          )
        )
    }

    def getMintValue(value: util.List[MultiAsset]): v1.Value = {
        getValue(
          // add Lovelace asset if not present, don't ask me why :(
          value.asScala.toSeq
              .appended(
                MultiAsset
                    .builder()
                    .policyId("")
                    .assets(util.List.of(Asset.builder().name("").value(BigInteger.ZERO).build()))
                    .build()
              )
              .asJava
        )
    }

    def getTxOutV1(out: TransactionOutput): v1.TxOut = {
        val addr = Address(out.getAddress)
        val optionDatumHash =
            if out.getDatumHash != null then
                prelude.Option.Some(ByteString.fromArray(out.getDatumHash))
            else prelude.Option.None
        v1.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          optionDatumHash
        )
    }

    def getTxOutV2(out: TransactionOutput): v2.TxOut = {
        val addr = Address(out.getAddress)
        v2.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          getOutputDatum(out),
          if out.getScriptRef != null then
              prelude.Option.Some(getScriptFromScriptRefBytes(out.getScriptRef).scriptHash)
          else prelude.Option.None
        )
    }

    def getOutputDatum(out: TransactionOutput): v2.OutputDatum = {
        if out.getDatumHash != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromArray(out.getDatumHash))
        else if out.getInlineDatum != null then
            v2.OutputDatum.OutputDatum(toScalusData(out.getInlineDatum))
        else v2.OutputDatum.NoOutputDatum
    }

    @deprecated("Use SlotConfig.slotToTime instead", "0.9.0")
    def slotToBeginPosixTime(slot: Long, sc: SlotConfig): Long = {
        sc.slotToTime(slot)
    }

    // https://github.com/IntersectMBO/cardano-ledger/blob/28ab3884cac8edbb7270fd4b8628a16429d2ec9e/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/TxInfo.hs#L186
    def getInterval(tx: Transaction, slotConfig: SlotConfig, protocolVersion: Int): v1.Interval = {
        val validFrom = tx.getBody.getValidityStartInterval
        (validFrom, tx.getBody.getTtl) match
            case (0, 0) => v1.Interval.always
            case (0, validTo) =>
                val closure =
                    if protocolVersion > 8 then false
                    else true // don't ask me why, I know it's stupid
                val upper = v1.IntervalBound(
                  v1.IntervalBoundType.Finite(BigInt(slotConfig.slotToTime(validTo))),
                  closure
                )
                v1.Interval(v1.Interval.negInf, upper)
            case (validFrom, 0) =>
                v1.Interval(
                  v1.Interval.finite(BigInt(slotConfig.slotToTime(validFrom))),
                  v1.Interval.posInf
                )
            case (validFrom, validTo) =>
                val lower = v1.Interval.finite(BigInt(slotConfig.slotToTime(validFrom)))
                val upper = v1.IntervalBound(
                  v1.IntervalBoundType.Finite(BigInt(slotConfig.slotToTime(validTo))),
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

    def getTxCertV3(cert: Certificate): v3.TxCert = {
        cert match
            case c: AuthCommitteeHotCert =>
                v3.TxCert.AuthHotCommittee(
                  getCredential(c.getCommitteeColdCredential),
                  getCredential(c.getCommitteeHotCredential)
                )
            case c: GenesisKeyDelegation =>
                throw new IllegalArgumentException("GenesisKeyDelegation not supported in V3")
            case c: MoveInstataneous =>
                throw new IllegalArgumentException("MoveInstantaneous not supported in V3")
            case c: PoolRegistration =>
                v3.TxCert.PoolRegister(
                  v1.PubKeyHash(ByteString.fromArray(c.getOperator)),
                  v1.PubKeyHash(ByteString.fromArray(c.getVrfKeyHash))
                )
            case c: PoolRetirement =>
                v3.TxCert.PoolRetire(
                  v1.PubKeyHash(ByteString.fromArray(c.getPoolKeyHash)),
                  BigInt(c.getEpoch)
                )
            case c: RegCert =>
                v3.TxCert.RegStaking(getCredential(c.getStakeCredential), prelude.Option.None)
            case c: RegDRepCert =>
                v3.TxCert.RegDRep(getCredential(c.getDrepCredential), BigInt(c.getCoin))
            case c: ResignCommitteeColdCert =>
                v3.TxCert.ResignColdCommittee(getCredential(c.getCommitteeColdCredential))
            case c: StakeDelegation =>
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Stake(
                    v1.PubKeyHash(ByteString.fromArray(c.getStakePoolId.getPoolKeyHash))
                  )
                )
            case c: StakeDeregistration =>
                throw new IllegalArgumentException("StakeDeregistration not supported in V3")
            case c: StakeRegDelegCert =>
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Stake(v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash))),
                  BigInt(c.getCoin)
                )
            case c: StakeRegistration =>
                throw new IllegalArgumentException("StakeRegistration not supported in V3")
            case c: StakeVoteDelegCert =>
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash)),
                    getDRep(c.getDrep)
                  )
                )
            case c: StakeVoteRegDelegCert =>
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.StakeVote(
                    v1.PubKeyHash(ByteString.fromHex(c.getPoolKeyHash)),
                    getDRep(c.getDrep)
                  ),
                  BigInt(c.getCoin)
                )
            case c: UnregCert =>
                v3.TxCert.UnRegStaking(getCredential(c.getStakeCredential), prelude.Option.None)
            case c: UnregDRepCert =>
                v3.TxCert.UnRegDRep(getCredential(c.getDrepCredential), BigInt(c.getCoin))
            case c: UpdateDRepCert =>
                v3.TxCert.UpdateDRep(getCredential(c.getDrepCredential))
            case c: VoteDelegCert =>
                val dRep = getDRep(c.getDrep)
                v3.TxCert.DelegStaking(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Vote(dRep)
                )
            case c: VoteRegDelegCert =>
                val dRep = getDRep(c.getDrep)
                v3.TxCert.RegDeleg(
                  getCredential(c.getStakeCredential),
                  v3.Delegatee.Vote(dRep),
                  BigInt(c.getCoin)
                )
    }

    private def getDRep(drep: DRep): v3.DRep = {
        drep.getType match
            case DRepType.ADDR_KEYHASH =>
                v3.DRep.DRep(
                  v1.Credential.PubKeyCredential(
                    v1.PubKeyHash(ByteString.fromHex(drep.getHash))
                  )
                )
            case DRepType.SCRIPTHASH =>
                v3.DRep.DRep(
                  v1.Credential.ScriptCredential(ByteString.fromHex(drep.getHash))
                )
            case DRepType.ABSTAIN =>
                v3.DRep.AlwaysAbstain
            case DRepType.NO_CONFIDENCE =>
                v3.DRep.AlwaysNoConfidence
    }

    def getTxInfoV1(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v1.TxInfo = getTxInfoV1(
      tx,
      TransactionUtil.getTxHash(tx),
      datums,
      utxos,
      slotConfig,
      protocolVersion
    )

    def getTxInfoV1(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v1.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        v1.TxInfo(
          // sorted inputs
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV1(_, utxos))),
          // outputs as in the transaction
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV1)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          // sorted mint values
          mint = getMintValue(body.getMint ?? util.List.of()),
          // certificates as is
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = getWithdrawals(body.getWithdrawals ?? util.List.of()),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          data = prelude.List.from(datums.sortBy(_._1)),
          id = v1.TxId(ByteString.fromHex(txhash))
        )
    }

    def getTxInfoV2(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.TxInfo = getTxInfoV2(
      tx,
      TransactionUtil.getTxHash(tx),
      datums,
      utxos,
      slotConfig,
      protocolVersion
    )

    def getTxInfoV2(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        v2.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))),
          referenceInputs = prelude.List.from(
            body.getReferenceInputs.asScala.sorted.map(getTxInInfoV2(_, utxos))
          ),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          mint = getMintValue(body.getMint ?? util.List.of()),
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = SortedMap.fromList(getWithdrawals(body.getWithdrawals ?? util.List.of())),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          redeemers = SortedMap.fromList(prelude.List.from(rdmrs.asScala.sorted.map { redeemer =>
              val purpose = getScriptPurposeV2(
                redeemer,
                body.getInputs,
                body.getMint,
                body.getCerts,
                body.getWithdrawals
              )
              purpose -> toScalusData(redeemer.getData)
          })),
          data = SortedMap.fromList(prelude.List.from(datums.sortBy(_._1))),
          id = v1.TxId(ByteString.fromHex(txhash))
        )
    }

    def getTxOutRefV1(input: TransactionInput): v1.TxOutRef = {
        v1.TxOutRef(
          v1.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    def getTxOutRefV3(input: TransactionInput): v3.TxOutRef = {
        v3.TxOutRef(
          v3.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    def getScriptPurposeV1(
        redeemer: Redeemer,
        tx: Transaction
    ): v1.ScriptPurpose =
        getScriptPurposeV1(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )

    def getScriptPurposeV1(
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
            case _ =>
                throw new IllegalStateException(
                  s"Unsupported redeemer tag: ${redeemer.getTag} in PlutusV1/V2"
                )

    @deprecated("Use getScriptPurposeV1 or getScriptPurposeV2", "0.8.0")
    def getScriptPurpose(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose = getScriptPurposeV1(redeemer, inputs, mint, certificates, withdrawals)

    def getScriptPurposeV2(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose = getScriptPurposeV1(redeemer, inputs, mint, certificates, withdrawals)

    def getScriptPurposeV2(redeemer: Redeemer, tx: Transaction): v2.ScriptPurpose = {
        getScriptPurposeV2(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )
    }

    def getScriptPurposeV3(tx: Transaction, redeemer: Redeemer): v3.ScriptPurpose = {
        val inputs = tx.getBody.getInputs
        val mint = tx.getBody.getMint
        val certificates = tx.getBody.getCerts
        val withdrawals = tx.getBody.getWithdrawals
        // Cardano Ledger code is stupidly complex and unreadable. We need to make sure this is correct
        val index = redeemer.getIndex.intValue
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sorted
                if ins.isDefinedAt(index) then
                    val input = ins(index)
                    v3.ScriptPurpose.Spending(getTxOutRefV3(input))
                else throw new IllegalStateException(s"Input not found: $index in $inputs")
            case RedeemerTag.Mint =>
                val policyIds = mint.asScala.map(_.getPolicyId).sorted
                if policyIds.isDefinedAt(index) then
                    v3.ScriptPurpose.Minting(ByteString.fromHex(policyIds(index)))
                else throw new IllegalStateException(s"Wrong mint index: $index in $mint")
            case RedeemerTag.Cert =>
                val certs = certificates.asScala
                if certs.isDefinedAt(index) then
                    v3.ScriptPurpose.Certifying(index, getTxCertV3(certs(index)))
                else throw new IllegalStateException(s"Wrong cert index: $index in $certificates")
            case RedeemerTag.Reward =>
                val rewardAccounts = withdrawals.asScala
                    .map(ra => Address(ra.getRewardAddress))
                    .sortBy(a => ByteString.fromArray(a.getBytes)) // for ordering
                if rewardAccounts.isDefinedAt(index) then
                    val address = rewardAccounts(index)
                    if address.getAddressType == AddressType.Reward then
                        val cred = getCredential(address.getDelegationCredential.get)
                        v3.ScriptPurpose.Rewarding(cred)
                    else
                        throw new IllegalStateException(
                          s"Wrong reward address type: $address in $withdrawals"
                        )
                else throw new IllegalStateException(s"Wrong reward index: $index in $withdrawals")

            case RedeemerTag.Proposing =>
                val proposals = certificates.asScala.collect { case p: ProposalProcedure => p }
                if proposals.isDefinedAt(index) then
                    v3.ScriptPurpose.Proposing(index, getProposalProcedureV3(proposals(index)))
                else
                    throw new IllegalStateException(
                      s"Wrong proposal index: $index in $certificates"
                    )
            case RedeemerTag.Voting =>
                val voting = tx.getBody.getVotingProcedures.getVoting.asScala.toSeq.sortBy(_._1)
                if voting.isDefinedAt(index) then
                    v3.ScriptPurpose.Voting(getVoterV3(voting(index)._1))
                else
                    throw new IllegalStateException(
                      s"Wrong voter index: $index in $certificates"
                    )
    }

    def getScriptInfoV3(tx: Transaction, redeemer: Redeemer, datum: Option[Data]): v3.ScriptInfo = {
        getScriptPurposeV3(tx, redeemer) match
            case v3.ScriptPurpose.Spending(ref) =>
                v3.ScriptInfo.SpendingScript(ref, datum.asScalus)
            case v3.ScriptPurpose.Minting(policyId) =>
                v3.ScriptInfo.MintingScript(policyId)
            case v3.ScriptPurpose.Certifying(index, cert) =>
                v3.ScriptInfo.CertifyingScript(index, cert)
            case v3.ScriptPurpose.Rewarding(cred) =>
                v3.ScriptInfo.RewardingScript(cred)
            case v3.ScriptPurpose.Proposing(index, proposal) =>
                v3.ScriptInfo.ProposingScript(index, proposal)
            case v3.ScriptPurpose.Voting(voter) =>
                v3.ScriptInfo.VotingScript(voter)
    }

    def getScriptContextV2(
        redeemer: Redeemer,
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.ScriptContext = getScriptContextV2(
      redeemer,
      tx,
      TransactionUtil.getTxHash(tx),
      utxos,
      slotConfig,
      protocolVersion
    )

    def getScriptContextV2(
        redeemer: Redeemer,
        tx: Transaction,
        txhash: String,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v2.ScriptContext = {
        import scala.jdk.CollectionConverters.*
        val purpose = getScriptPurposeV2(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )
        val datums = tx.getWitnessSet.getPlutusDataList.asScala.map { plutusData =>
            ByteString.fromArray(plutusData.getDatumHashAsBytes) -> Interop.toScalusData(plutusData)
        }
        val txInfo = getTxInfoV2(tx, txhash, datums, utxos, slotConfig, protocolVersion)
        val scriptContext = v2.ScriptContext(txInfo, purpose)
        scriptContext
    }

    def getProposalProcedureV3(proposal: ProposalProcedure): v3.ProposalProcedure = {
        v3.ProposalProcedure(
          proposal.getDeposit,
          getCredential(Address(proposal.getRewardAccount).getPaymentCredential.get),
          getGovernanceActionV3(proposal.getGovAction)
        )
    }

    def getGovernanceActionV3(action: GovAction): v3.GovernanceAction = {
        action match
            case a: ParameterChangeAction =>
                v3.GovernanceAction.ParameterChange(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  parameters = a.getProtocolParamUpdate.toData,
                  constitutionScript = prelude.Option(a.getPolicyHash).map(ByteString.fromArray)
                )
            case a: TreasuryWithdrawalsAction =>
                v3.GovernanceAction.TreasuryWithdrawals(
                  withdrawals =
                      SortedMap.fromList(prelude.List.from(a.getWithdrawals.asScala.map { w =>
                          getCredential(
                            Address(w.getRewardAddress).getPaymentCredential.get
                          ) -> BigInt(
                            w.getCoin
                          )

                      }.toList)),
                  constitutionScript = prelude.Option(ByteString.fromArray(a.getPolicyHash))
                )
            case a: HardForkInitiationAction =>
                v3.GovernanceAction.HardForkInitiation(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  protocolVersion = getProtocolVersion(a.getProtocolVersion)
                )
            case _: InfoAction => v3.GovernanceAction.InfoAction
            case a: NewConstitution =>
                v3.GovernanceAction.NewConstitution(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  constitution =
                      if a.getConstitution.getScripthash != null then
                          prelude.Option.Some(ByteString.fromHex(a.getConstitution.getScripthash))
                      else prelude.Option.None
                )
            case a: NoConfidence =>
                v3.GovernanceAction.NoConfidence(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId))
                )
            case a: UpdateCommittee =>
                v3.GovernanceAction.UpdateCommittee(
                  id =
                      if a.getPrevGovActionId == null then prelude.Option.None
                      else prelude.Option.Some(getGovActionId(a.getPrevGovActionId)),
                  removedMembers = prelude.List.from(a.getMembersForRemoval.asScala.map { m =>
                      getCredential(m)
                  }),
                  addedMembers = SortedMap.fromList(
                    prelude.List.from(a.getNewMembersAndTerms.asScala.map { (c, t) =>
                        getCredential(c) -> BigInt(t)
                    })
                  ),
                  newQuorum = prelude.Rational(
                    BigInt(a.getQuorumThreshold.getNumerator),
                    BigInt(a.getQuorumThreshold.getDenominator)
                  )
                )

    }

    def getProtocolVersion(version: ProtocolVersion): v3.ProtocolVersion = {
        v3.ProtocolVersion(version.getMajor, version.getMinor)
    }

    def getGovActionId(id: GovActionId): v3.GovernanceActionId = {
        v3.GovernanceActionId(
          v3.TxId(ByteString.fromHex(id.getTransactionId)),
          id.getGovActionIndex
        )
    }

    def getVoterV3(voter: Voter): v3.Voter = {
        voter.getType match
            case VoterType.CONSTITUTIONAL_COMMITTEE_HOT_KEY_HASH |
                VoterType.CONSTITUTIONAL_COMMITTEE_HOT_SCRIPT_HASH =>
                v3.Voter.CommitteeVoter(getCredential(voter.getCredential))
            case VoterType.DREP_KEY_HASH | VoterType.DREP_SCRIPT_HASH =>
                v3.Voter.DRepVoter(getCredential(voter.getCredential))
            case VoterType.STAKING_POOL_KEY_HASH =>
                v3.Voter.StakePoolVoter(
                  v1.PubKeyHash(ByteString.fromArray(voter.getCredential.getBytes))
                )
    }

    def getVotingProcedures(
        voting: VotingProcedures
    ): SortedMap[v3.Voter, SortedMap[GovernanceActionId, v3.Vote]] = {
        if voting == null then return SortedMap.empty
        SortedMap.fromList(
          prelude.List.from(
            voting.getVoting.asScala.toSeq
                .sortBy(_._1)
                .map: (voter, procedures) =>
                    getVoterV3(voter) -> SortedMap.fromList(
                      prelude.List.from(
                        procedures.asScala.toSeq
                            .sortBy(_._1)
                            .map: (govActionId, procedure) =>
                                getGovActionId(govActionId) -> getVoteV3(procedure)
                      )
                    )
          )
        )
    }

    def getVoteV3(procedure: VotingProcedure): v3.Vote = {
        procedure.getVote match
            case Vote.YES     => v3.Vote.Yes
            case Vote.NO      => v3.Vote.No
            case Vote.ABSTAIN => v3.Vote.Abstain
    }

    def getTxInInfoV3(
        input: TransactionInput,
        utxos: Map[TransactionInput, TransactionOutput]
    ): v3.TxInInfo = {
        val txInInfoV2 = getTxInInfoV2(input, utxos)
        v3.TxInInfo(
          outRef = v3.TxOutRef(v3.TxId(txInInfoV2.outRef.id.hash), txInInfoV2.outRef.idx),
          resolved = txInInfoV2.resolved
        )
    }

    def getTxInfoV3(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.TxInfo =
        getTxInfoV3(tx, TransactionUtil.getTxHash(tx), datums, utxos, slotConfig, protocolVersion)

    def getTxInfoV3(
        tx: Transaction,
        txhash: String,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val withdrawals =
            val wdvls = getWithdrawals(body.getWithdrawals ?? util.List.of())
            wdvls.map {
                case (v1.StakingCredential.StakingHash(cred), coin) => cred -> coin
                case w => throw new IllegalStateException(s"Invalid withdrawal: $w")
            }
        v3.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.sorted.map(getTxInInfoV3(_, utxos))),
          referenceInputs = prelude.List.from(
            body.getReferenceInputs.asScala.sorted.map(getTxInInfoV3(_, utxos))
          ),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = body.getFee ?? BigInteger.ZERO,
          mint = getValue(body.getMint ?? util.List.of()),
          certificates = prelude.List.from(certs.asScala.map(getTxCertV3)),
          withdrawals = SortedMap.fromList(withdrawals),
          validRange = getInterval(tx, slotConfig, protocolVersion),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala
                .map(ByteString.fromArray)
                .sorted
                .map(v1.PubKeyHash.apply)
                .toSeq
          ),
          redeemers = SortedMap.fromList(prelude.List.from(rdmrs.asScala.sorted.map { redeemer =>
              val purpose = getScriptPurposeV3(tx, redeemer)
              purpose -> toScalusData(redeemer.getData)
          })),
          data = SortedMap.fromList(prelude.List.from(datums.sortBy(_._1))),
          id = v3.TxId(ByteString.fromHex(txhash)),
          votes = getVotingProcedures(body.getVotingProcedures),
          proposalProcedures = prelude.List
              .from(
                (body.getProposalProcedures ?? util.List.of()).asScala
                    .map(getProposalProcedureV3)
              ),
          currentTreasuryAmount =
              if tx.getBody.getCurrentTreasuryValue != null then
                  prelude.Option.Some(
                    BigInt(tx.getBody.getCurrentTreasuryValue)
                  )
              else prelude.Option.None,
          treasuryDonation =
              if tx.getBody.getDonation != null then
                  prelude.Option.Some(BigInt(tx.getBody.getDonation))
              else prelude.Option.None
        )
    }

    def getScriptContextV3(
        redeemer: Redeemer,
        datum: Option[Data],
        tx: Transaction,
        txhash: String,
        utxos: Map[TransactionInput, TransactionOutput],
        slotConfig: SlotConfig,
        protocolVersion: Int
    ): v3.ScriptContext = {
        import scala.jdk.CollectionConverters.*
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val datums = tx.getWitnessSet.getPlutusDataList.asScala.map { plutusData =>
            ByteString.fromArray(plutusData.getDatumHashAsBytes) -> plutusData.toScalusData
        }
        val txInfo = getTxInfoV3(tx, txhash, datums, utxos, slotConfig, protocolVersion)
        val scriptContext = v3.ScriptContext(txInfo, redeemer.getData.toScalusData, scriptInfo)
        scriptContext
    }
}
