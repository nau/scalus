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
import scalus.prelude.AssocMap
import scalus.prelude.List
import scalus.prelude.Maybe
import scalus.prelude.Rational

@Compile
object FromDataInstances {
    import scalus.builtin.FromDataInstances.given
    import scalus.ledger.api.v1.FromDataInstances.given
    import scalus.ledger.api.v2.FromDataInstances.given

    given FromData[TxId] = (d: Data) => new TxId(d.toByteString)
    given FromData[TxOutRef] = FromData.deriveCaseClass
    given FromData[DRep] = FromData.deriveEnum
    given FromData[Delegatee] = FromData.deriveEnum
    given FromData[TxCert] = FromData.deriveEnum
    given FromData[Voter] = FromData.deriveEnum
    given FromData[Vote] = FromData.deriveEnum
    given FromData[GovernanceActionId] = FromData.deriveCaseClass
    given FromData[Committee] = FromData.deriveCaseClass
    given FromData[ProtocolVersion] = FromData.deriveCaseClass
    given FromData[GovernanceAction] = FromData.deriveEnum
    given FromData[ProposalProcedure] = FromData.deriveCaseClass
    given FromData[ScriptPurpose] = FromData.deriveEnum
    given FromData[ScriptInfo] = FromData.deriveEnum
    given FromData[TxInInfo] = FromData.deriveCaseClass
    given FromData[TxInfo] = FromData.deriveCaseClass
    given FromData[ScriptContext] = FromData.deriveCaseClass
}

@Compile
object ToDataInstances {
    import scalus.builtin.ToDataInstances.given
    import scalus.ledger.api.v1.ToDataInstances.given
    import scalus.ledger.api.v2.ToDataInstances.given

    given ToData[TxId] = (x: TxId) => bData(x.hash)
    given ToData[TxOutRef] = ToData.deriveCaseClass[TxOutRef](0)
    given ToData[DRep] = ToData.deriveEnum
    given ToData[Delegatee] = ToData.deriveEnum
    given ToData[TxCert] = ToData.deriveEnum
    given ToData[Voter] = ToData.deriveEnum
    given ToData[Vote] = ToData.deriveEnum
    given ToData[GovernanceActionId] = ToData.deriveCaseClass[GovernanceActionId](0)
    given ToData[Committee] = ToData.deriveCaseClass[Committee](0)
    given ToData[ProtocolVersion] = ToData.deriveCaseClass[ProtocolVersion](0)
    given ToData[GovernanceAction] = ToData.deriveEnum
    given ToData[ProposalProcedure] = ToData.deriveCaseClass[ProposalProcedure](0)
    given ToData[ScriptPurpose] = ToData.deriveEnum
    given ToData[ScriptInfo] = ToData.deriveEnum
    given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)
    given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)
    given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}

case class TxId(hash: ByteString)

case class TxOutRef(
    id: TxId,
    idx: BigInt
)

type Lovelace = BigInt
type ColdCommitteeCredential = Credential

type HotCommitteeCredential = Credential

type DRepCredential = Credential

enum DRep:
    case DRep(credential: DRepCredential)
    case AlwaysAbstain
    case AlwaysNoConfidence

enum Delegatee:
    case Stake(pubKeyHash: PubKeyHash)
    case Vote(dRep: DRep)
    case StakeVote(pubKeyHash: PubKeyHash, dRep: DRep)

enum TxCert:
    case RegStaking(credential: Credential, deposit: Maybe[Lovelace])
    case UnRegStaking(credential: Credential, refund: Maybe[Lovelace])
    case DelegStaking(credential: Credential, delegatee: Delegatee)
    case RegDeleg(credential: Credential, delegatee: Delegatee, deposit: Lovelace)
    case RegDRep(credential: DRepCredential, deposit: Lovelace)
    case UpdateDRep(credential: DRepCredential)
    case UnRegDRep(credential: DRepCredential, refund: Lovelace)
    case PoolRegister(poolId: PubKeyHash, poolVFR: PubKeyHash)
    case PoolRetire(pubKeyHash: PubKeyHash, epoch: BigInt)
    case AuthHotCommittee(cold: ColdCommitteeCredential, hot: HotCommitteeCredential)
    case ResignColdCommittee(cold: ColdCommitteeCredential)

enum Voter:
    case CommitteeVoter(credential: HotCommitteeCredential)
    case DRepVoter(credential: DRepCredential)
    case StakePoolVoter(pubKeyHash: PubKeyHash)

enum Vote:
    case No, Yes, Abstain

case class GovernanceActionId(txId: TxId, govActionIx: BigInt)

case class Committee(
    members: AssocMap[ColdCommitteeCredential, BigInt],
    quorum: BigInt
)

type Constitution = Maybe[ScriptHash]

case class ProtocolVersion(pvMajor: BigInt, pvMinor: BigInt)

type ChangedParameters = Data

enum GovernanceAction:
    case ParameterChange(
        id: Maybe[GovernanceActionId],
        parameters: ChangedParameters,
        constitutionScript: Maybe[ScriptHash]
    )
    case HardForkInitiation(id: Maybe[GovernanceActionId], protocolVersion: ProtocolVersion)
    case TreasuryWithdrawals(
        withdrawals: AssocMap[Credential, Lovelace],
        constitutionScript: Maybe[ScriptHash]
    )
    case NoConfidence(id: Maybe[GovernanceActionId])
    case UpdateCommittee(
        id: Maybe[GovernanceActionId],
        removedMembers: List[ColdCommitteeCredential],
        addedMembers: AssocMap[ColdCommitteeCredential, BigInt],
        newQuorum: Rational
    )
    case NewConstitution(id: Maybe[GovernanceActionId], constitution: Constitution)
    case InfoAction

case class ProposalProcedure(
    deposit: Lovelace,
    returnAddress: Credential,
    governanceAction: GovernanceAction
)

enum ScriptPurpose:
    case Minting(currencySymbol: CurrencySymbol)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(credential: Credential)
    case Certifying(index: BigInt, cert: TxCert)
    case Voting(voter: Voter)
    case Proposing(index: BigInt, procedure: ProposalProcedure)

enum ScriptInfo:
    case MintingScript(currencySymbol: CurrencySymbol)
    case SpendingScript(txOutRef: TxOutRef, datum: Maybe[Datum])
    case RewardingScript(credential: Credential)
    case CertifyingScript(index: BigInt, cert: TxCert)
    case VotingScript(voter: Voter)
    case ProposingScript(index: BigInt, procedure: ProposalProcedure)

case class TxInInfo(
    outRef: TxOutRef,
    resolved: v2.TxOut
)

case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo],
    outputs: List[v2.TxOut],
    fee: Lovelace,
    mint: Value,
    certificates: List[TxCert],
    withdrawals: AssocMap[Credential, Lovelace],
    validRange: Interval,
    signatories: List[PubKeyHash],
    redeemers: AssocMap[ScriptPurpose, Redeemer],
    data: AssocMap[DatumHash, Datum],
    id: TxId,
    votes: AssocMap[Voter, AssocMap[GovernanceActionId, Vote]],
    proposalProcedures: List[ProposalProcedure],
    currentTreasuryAmount: Maybe[Lovelace],
    treasuryDonation: Maybe[Lovelace]
)

case class ScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer,
    scriptInfo: ScriptInfo
)
