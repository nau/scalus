package scalus.ledger.api.v3

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.ledger.api.v1.*
import scalus.ledger.api.v2
import scalus.prelude.{*, given}
import scalus.builtin.ByteString.*

export scalus.ledger.api.v1.Hash
export scalus.ledger.api.v1.Address
export scalus.ledger.api.v1.Closure
export scalus.ledger.api.v1.Credential
export scalus.ledger.api.v1.CurrencySymbol
export scalus.ledger.api.v1.DCert
export scalus.ledger.api.v1.Datum
export scalus.ledger.api.v1.DatumHash
export scalus.ledger.api.v1.IntervalBoundType
export scalus.ledger.api.v1.Interval
export scalus.ledger.api.v1.IntervalBound
export scalus.ledger.api.v1.PosixTime
export scalus.ledger.api.v1.PosixTimeRange
export scalus.ledger.api.v1.PubKeyHash
export scalus.ledger.api.v1.Redeemer
export scalus.ledger.api.v1.RedeemerHash
export scalus.ledger.api.v1.ScriptHash
export scalus.ledger.api.v1.StakingCredential
export scalus.ledger.api.v1.TokenName
export scalus.ledger.api.v1.ValidatorHash
export scalus.ledger.api.v2.TxOut
//type Value = scalus.ledger.api.v1.Value
export scalus.ledger.api.v1.Value

@Compile
@deprecated("Not need anymore, use companion objects instead")
object FromDataInstances

@Compile
@deprecated("Not need anymore, use companion objects instead")
object ToDataInstances

case class TxId(hash: ByteString)

@Compile
object TxId:
    given Eq[TxId] = (a: TxId, b: TxId) => a.hash === b.hash
    given FromData[TxId] = (d: Data) => TxId(d.toByteString)
    given ToData[TxId] = (x: TxId) => bData(x.hash)

case class TxOutRef(id: TxId, idx: BigInt)

@Compile
object TxOutRef:
    given Eq[TxOutRef] = (a: TxOutRef, b: TxOutRef) => a.id === b.id && a.idx === b.idx
    given FromData[TxOutRef] = FromData.derived
    given ToData[TxOutRef] = ToData.derived

type Lovelace = BigInt
type ColdCommitteeCredential = Credential

type HotCommitteeCredential = Credential

type DRepCredential = Credential

enum DRep:
    case DRep(credential: DRepCredential)
    case AlwaysAbstain
    case AlwaysNoConfidence

@Compile
object DRep:

    given Eq[scalus.ledger.api.v3.DRep] =
        (lhs: scalus.ledger.api.v3.DRep, rhs: scalus.ledger.api.v3.DRep) =>
            lhs match
                case DRep(lhsCredential) =>
                    rhs match
                        case DRep(rhsCredential) => lhsCredential === rhsCredential
                        case _                   => false
                case AlwaysAbstain =>
                    rhs match
                        case AlwaysAbstain => true
                        case _             => false
                case AlwaysNoConfidence =>
                    rhs match
                        case AlwaysNoConfidence => true
                        case _                  => false

    given FromData[scalus.ledger.api.v3.DRep] = FromData.derived

    given ToData[scalus.ledger.api.v3.DRep] = ToData.derived

end DRep

enum Delegatee:
    case Stake(pubKeyHash: PubKeyHash)
    case Vote(dRep: DRep)
    case StakeVote(pubKeyHash: PubKeyHash, dRep: DRep)

@Compile
object Delegatee:

    given Eq[Delegatee] = (lhs: Delegatee, rhs: Delegatee) =>
        lhs match
            case Stake(lhsPubKeyHash) =>
                rhs match
                    case Stake(rhsPubKeyHash) => lhsPubKeyHash === rhsPubKeyHash
                    case _                    => false
            case Vote(lhsDRep) =>
                rhs match
                    case Vote(rhsDRep) => lhsDRep === rhsDRep
                    case _             => false
            case StakeVote(lhsPubKeyHash, lhsDRep) =>
                rhs match
                    case StakeVote(rhsPubKeyHash, rhsDRep) =>
                        lhsPubKeyHash === rhsPubKeyHash && lhsDRep === rhsDRep
                    case _ => false

    given FromData[Delegatee] = FromData.derived
    given ToData[Delegatee] = ToData.derived

end Delegatee

enum TxCert:
    case RegStaking(credential: Credential, deposit: Option[Lovelace])
    case UnRegStaking(credential: Credential, refund: Option[Lovelace])
    case DelegStaking(credential: Credential, delegatee: Delegatee)
    case RegDeleg(credential: Credential, delegatee: Delegatee, deposit: Lovelace)
    case RegDRep(credential: DRepCredential, deposit: Lovelace)
    case UpdateDRep(credential: DRepCredential)
    case UnRegDRep(credential: DRepCredential, refund: Lovelace)
    case PoolRegister(poolId: PubKeyHash, poolVFR: PubKeyHash)
    case PoolRetire(pubKeyHash: PubKeyHash, epoch: BigInt)
    case AuthHotCommittee(cold: ColdCommitteeCredential, hot: HotCommitteeCredential)
    case ResignColdCommittee(cold: ColdCommitteeCredential)

@Compile
object TxCert:

    given Eq[TxCert] = (lhs: TxCert, rhs: TxCert) =>
        lhs match
            case RegStaking(lhsCredential, lhsDeposit) =>
                rhs match
                    case RegStaking(rhsCredential, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDeposit === rhsDeposit
                    case _ => false
            case UnRegStaking(lhsCredential, lhsRefund) =>
                rhs match
                    case UnRegStaking(rhsCredential, rhsRefund) =>
                        lhsCredential === rhsCredential && lhsRefund === rhsRefund
                    case _ => false
            case DelegStaking(lhsCredential, lhsDelegatee) =>
                rhs match
                    case DelegStaking(rhsCredential, rhsDelegatee) =>
                        lhsCredential === rhsCredential && lhsDelegatee === rhsDelegatee
                    case _ => false
            case RegDeleg(lhsCredential, lhsDelegatee, lhsDeposit) =>
                rhs match
                    case RegDeleg(rhsCredential, rhsDelegatee, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDelegatee === rhsDelegatee && lhsDeposit === rhsDeposit
                    case _ => false
            case RegDRep(lhsCredential, lhsDeposit) =>
                rhs match
                    case RegDRep(rhsCredential, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDeposit === rhsDeposit
                    case _ => false
            case UpdateDRep(lhsCredential) =>
                rhs match
                    case UpdateDRep(rhsCredential) => lhsCredential === rhsCredential
                    case _                         => false
            case UnRegDRep(lhsCredential, lhsRefund) =>
                rhs match
                    case UnRegDRep(rhsCredential, rhsRefund) =>
                        lhsCredential === rhsCredential && lhsRefund === rhsRefund
                    case _ => false
            case PoolRegister(lhsPoolId, lhsPoolVFR) =>
                rhs match
                    case PoolRegister(rhsPoolId, rhsPoolVFR) =>
                        lhsPoolId === rhsPoolId && lhsPoolVFR === rhsPoolVFR
                    case _ => false
            case PoolRetire(lhsPubKeyHash, lhsEpoch) =>
                rhs match
                    case PoolRetire(rhsPubKeyHash, rhsEpoch) =>
                        lhsPubKeyHash === rhsPubKeyHash && lhsEpoch === rhsEpoch
                    case _ => false
            case AuthHotCommittee(lhsCold, lhsHot) =>
                rhs match
                    case AuthHotCommittee(rhsCold, rhsHot) =>
                        lhsCold === rhsCold && lhsHot === rhsHot
                    case _ => false
            case ResignColdCommittee(lhsCold) =>
                rhs match
                    case ResignColdCommittee(rhsCold) => lhsCold === rhsCold
                    case _                            => false

    given FromData[TxCert] = FromData.derived
    given ToData[TxCert] = ToData.derived

end TxCert

enum Voter:
    case CommitteeVoter(credential: HotCommitteeCredential)
    case DRepVoter(credential: DRepCredential)
    case StakePoolVoter(pubKeyHash: PubKeyHash)

@Compile
object Voter:

    given Eq[Voter] = (lhs: Voter, rhs: Voter) =>
        lhs match
            case CommitteeVoter(lhsCredential) =>
                rhs match
                    case CommitteeVoter(rhsCredential) => lhsCredential === rhsCredential
                    case _                             => false
            case DRepVoter(lhsCredential) =>
                rhs match
                    case DRepVoter(rhsCredential) => lhsCredential === rhsCredential
                    case _                        => false
            case StakePoolVoter(lhsPubKeyHash) =>
                rhs match
                    case StakePoolVoter(rhsPubKeyHash) => lhsPubKeyHash === rhsPubKeyHash
                    case _                             => false

    given ToData[Voter] = ToData.derived
    given FromData[Voter] = FromData.derived

end Voter

enum Vote:
    case No, Yes, Abstain

@Compile
object Vote:

    given Eq[Vote] = (lhs: Vote, rhs: Vote) =>
        lhs match
            case No =>
                rhs match
                    case No => true
                    case _  => false
            case Yes =>
                rhs match
                    case Yes => true
                    case _   => false
            case Abstain =>
                rhs match
                    case Abstain => true
                    case _       => false

    given ToData[Vote] = ToData.derived
    given FromData[Vote] = FromData.derived

end Vote

case class GovernanceActionId(txId: TxId, govActionIx: BigInt)

@Compile
object GovernanceActionId:

    given Eq[GovernanceActionId] = (lhs: GovernanceActionId, rhs: GovernanceActionId) =>
        lhs.txId === rhs.txId && lhs.govActionIx === rhs.govActionIx

    given FromData[GovernanceActionId] = FromData.derived
    given ToData[GovernanceActionId] = ToData.derived

end GovernanceActionId

case class Committee(
    members: AssocMap[ColdCommitteeCredential, BigInt],
    quorum: BigInt
)

@Compile
object Committee:
    given Eq[Committee] = (lhs: Committee, rhs: Committee) =>
        lhs.members === rhs.members && lhs.quorum === rhs.quorum

    given FromData[Committee] = FromData.derived
    given ToData[Committee] = ToData.derived

end Committee

type Constitution = Option[ScriptHash]

case class ProtocolVersion(pvMajor: BigInt, pvMinor: BigInt)

@Compile
object ProtocolVersion:
    given Eq[ProtocolVersion] = (lhs: ProtocolVersion, rhs: ProtocolVersion) =>
        lhs.pvMajor === rhs.pvMajor && lhs.pvMinor === rhs.pvMinor

    given FromData[ProtocolVersion] = FromData.derived
    given ToData[ProtocolVersion] = ToData.derived

end ProtocolVersion

type ChangedParameters = Data

enum GovernanceAction:
    case ParameterChange(
        id: Option[GovernanceActionId],
        parameters: ChangedParameters,
        constitutionScript: Option[ScriptHash]
    )
    case HardForkInitiation(id: Option[GovernanceActionId], protocolVersion: ProtocolVersion)
    case TreasuryWithdrawals(
        withdrawals: AssocMap[Credential, Lovelace],
        constitutionScript: Option[ScriptHash]
    )
    case NoConfidence(id: Option[GovernanceActionId])
    case UpdateCommittee(
        id: Option[GovernanceActionId],
        removedMembers: List[ColdCommitteeCredential],
        addedMembers: AssocMap[ColdCommitteeCredential, BigInt],
        newQuorum: Rational
    )
    case NewConstitution(id: Option[GovernanceActionId], constitution: Constitution)
    case InfoAction

@Compile
object GovernanceAction:
    given Eq[GovernanceAction] = (lhs: GovernanceAction, rhs: GovernanceAction) =>
        lhs match
            case ParameterChange(lhsId, lhsParameters, lhsConstitutionScript) =>
                rhs match
                    case ParameterChange(rhsId, rhsParameters, rhsConstitutionScript) =>
                        lhsId === rhsId && lhsParameters === rhsParameters
                        && lhsConstitutionScript === rhsConstitutionScript
                    case _ => false
            case HardForkInitiation(lhsId, lhsProtocolVersion) =>
                rhs match
                    case HardForkInitiation(rhsId, rhsProtocolVersion) =>
                        lhsId === rhsId && lhsProtocolVersion === rhsProtocolVersion
                    case _ => false
            case TreasuryWithdrawals(lhsWithdrawals, lhsConstitutionScript) =>
                rhs match
                    case TreasuryWithdrawals(rhsWithdrawals, rhsConstitutionScript) =>
                        lhsWithdrawals === rhsWithdrawals && lhsConstitutionScript === rhsConstitutionScript
                    case _ => false
            case NoConfidence(lhsId) =>
                rhs match
                    case NoConfidence(rhsId) => lhsId === rhsId
                    case _                   => false
            case UpdateCommittee(lhsId, lhsRemovedMembers, lhsAddedMembers, lhsNewQuorum) =>
                rhs match
                    case UpdateCommittee(rhsId, rhsRemovedMembers, rhsAddedMembers, rhsNewQuorum) =>
                        lhsId === rhsId && lhsRemovedMembers === rhsRemovedMembers
                        && lhsAddedMembers === rhsAddedMembers && lhsNewQuorum === rhsNewQuorum
                    case _ => false
            case NewConstitution(lhsId, lhsConstitution) =>
                rhs match
                    case NewConstitution(rhsId, rhsConstitution) =>
                        lhsId === rhsId
                        && lhsConstitution === rhsConstitution
                    case _ => false
            case InfoAction =>
                rhs match
                    case InfoAction => true
                    case _          => false

    given FromData[GovernanceAction] = FromData.derived
    given ToData[GovernanceAction] = ToData.derived

end GovernanceAction

case class ProposalProcedure(
    deposit: Lovelace,
    returnAddress: Credential,
    governanceAction: GovernanceAction
)

@Compile
object ProposalProcedure:

    given Eq[ProposalProcedure] = (lhs: ProposalProcedure, rhs: ProposalProcedure) =>
        lhs.deposit === rhs.deposit && lhs.returnAddress === rhs.returnAddress
            && lhs.governanceAction === rhs.governanceAction

    given ToData[ProposalProcedure] = ToData.derived
    given FromData[ProposalProcedure] = FromData.derived

end ProposalProcedure

enum ScriptPurpose:
    case Minting(currencySymbol: CurrencySymbol)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(credential: Credential)
    case Certifying(index: BigInt, cert: TxCert)
    case Voting(voter: Voter)
    case Proposing(index: BigInt, procedure: ProposalProcedure)

@Compile
object ScriptPurpose:
    given Eq[ScriptPurpose] = (lhs: ScriptPurpose, rhs: ScriptPurpose) =>
        lhs match
            case Minting(lhsCurrencySymbol) =>
                rhs match
                    case Minting(rhsCurrencySymbol) => lhsCurrencySymbol === rhsCurrencySymbol
                    case _                          => false
            case Spending(lhsTxOutRef) =>
                rhs match
                    case Spending(rhsTxOutRef) => lhsTxOutRef === rhsTxOutRef
                    case _                     => false
            case Rewarding(lhsCredential) =>
                rhs match
                    case Rewarding(rhsCredential) => lhsCredential === rhsCredential
                    case _                        => false
            case Certifying(lhsIndex, lhsCert) =>
                rhs match
                    case Certifying(rhsIndex, rhsCert) =>
                        lhsIndex === rhsIndex && lhsCert === rhsCert
                    case _ => false
            case Voting(lhsVoter) =>
                rhs match
                    case Voting(rhsVoter) => lhsVoter === rhsVoter
                    case _                => false
            case Proposing(lhsIndex, lhsProcedure) =>
                rhs match
                    case Proposing(rhsIndex, rhsProcedure) =>
                        lhsIndex === rhsIndex && lhsProcedure === rhsProcedure
                    case _ => false

    given FromData[ScriptPurpose] = FromData.derived
    given ToData[ScriptPurpose] = ToData.derived

end ScriptPurpose

enum ScriptInfo:
    case MintingScript(currencySymbol: CurrencySymbol)
    case SpendingScript(txOutRef: TxOutRef, datum: Option[Datum] = Option.None)
    case RewardingScript(credential: Credential)
    case CertifyingScript(index: BigInt, cert: TxCert)
    case VotingScript(voter: Voter)
    case ProposingScript(index: BigInt, procedure: ProposalProcedure)

@Compile
object ScriptInfo:

    given FromData[ScriptInfo] = FromData.derived
    given ToData[ScriptInfo] = ToData.derived

end ScriptInfo

@deprecated("Use ScriptInfo instead")
case class SpendingScriptInfo(txOutRef: TxOutRef, datum: Option[Datum])

object SpendingScriptInfo:

    @deprecated("Use ScriptInfo instead")
    given FromData[SpendingScriptInfo] = (d: Data) =>
        val pair = d.toConstr
        if pair.fst == BigInt(1) then
            val args = pair.snd
            SpendingScriptInfo(args.head.to[TxOutRef], args.tail.head.to[Option[Datum]])
        else throw new Exception("Invalid SpendingScriptInfo")

    given ToData[SpendingScriptInfo] = ToData.derived

end SpendingScriptInfo

@deprecated("Use ScriptInfo instead")
case class MintingScriptInfo(currencySymbol: CurrencySymbol)

@deprecated("Use ScriptInfo instead")
case class RewardingScriptInfo(credential: Credential)

case class TxInInfo(
    outRef: TxOutRef,
    resolved: v2.TxOut
)

@Compile
object TxInInfo:

    given FromData[TxInInfo] = FromData.derived
    given ToData[TxInInfo] = ToData.derived

end TxInInfo

case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo] = List.Nil,
    outputs: List[v2.TxOut] = List.Nil,
    fee: Lovelace = 0,
    mint: Value = Value.zero,
    certificates: List[TxCert] = List.Nil,
    withdrawals: AssocMap[Credential, Lovelace] = AssocMap.empty,
    validRange: Interval = Interval.always,
    signatories: List[PubKeyHash] = List.Nil,
    redeemers: AssocMap[ScriptPurpose, Redeemer] = AssocMap.empty,
    data: AssocMap[DatumHash, Datum] = AssocMap.empty,
    id: TxId,
    votes: AssocMap[Voter, AssocMap[GovernanceActionId, Vote]] = AssocMap.empty,
    proposalProcedures: List[ProposalProcedure] = List.Nil,
    currentTreasuryAmount: Option[Lovelace] = Option.None,
    treasuryDonation: Option[Lovelace] = Option.None
)

@Compile
object TxInfo {

    given FromData[TxInfo] = FromData.derived
    given ToData[TxInfo] = ToData.derived

    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

}

case class ScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer = Data.unit,
    scriptInfo: ScriptInfo
)

@Compile
object ScriptContext {

    given FromData[ScriptContext] = FromData.derived
    given ToData[ScriptContext] = ToData.derived

}

@deprecated("Use ScriptContext instead")
case class SpendingScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer,
    scriptInfo: SpendingScriptInfo
)

@Compile
@deprecated("Use ScriptContext instead")
object SpendingScriptContext {

    given FromData[SpendingScriptContext] = FromData.derived
    given ToData[SpendingScriptContext] = ToData.derived

}
