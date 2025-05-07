package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given

/** Represents a certificate for stake operations on the blockchain
  */
enum Certificate:
    case StakeRegistration(credential: Credential)
    case StakeDeregistration(credential: Credential)
    case StakeDelegation(credential: Credential, poolKeyHash: Hash28)
    case PoolRegistration(
        operator: AddrKeyHash,
        vrfKeyHash: Hash32,
        pledge: Coin,
        cost: Coin,
        margin: UnitInterval,
        rewardAccount: RewardAccount,
        poolOwners: Set[AddrKeyHash],
        relays: Seq[Relay],
        poolMetadata: Option[PoolMetadata]
    )
    case PoolRetirement(poolKeyHash: Hash28, epochNo: Long)
    case RegCert(credential: Credential, coin: Coin)
    case UnregCert(credential: Credential, coin: Coin)
    case VoteDelegCert(credential: Credential, drep: DRep)
    case StakeVoteDelegCert(credential: Credential, poolKeyHash: Hash28, drep: DRep)
    case StakeRegDelegCert(credential: Credential, poolKeyHash: Hash28, coin: Coin)
    case VoteRegDelegCert(credential: Credential, drep: DRep, coin: Coin)
    case StakeVoteRegDelegCert(credential: Credential, poolKeyHash: Hash28, drep: DRep, coin: Coin)
    case AuthCommitteeHotCert(
        committeeColdCredential: Credential,
        committeeHotCredential: Credential
    )
    case ResignCommitteeColdCert(
        committeeColdCredential: Credential,
        anchor: Nullable[Option[Anchor]]
    )
    case RegDRepCert(drepCredential: Credential, coin: Coin, anchor: Nullable[Option[Anchor]])
    case UnregDRepCert(drepCredential: Credential, coin: Coin)
    case UpdateDRepCert(drepCredential: Credential, anchor: Nullable[Option[Anchor]])

object Certificate:
    given Encoder[Certificate] with
        def write(w: Writer, value: Certificate): Writer = value match
            case Certificate.StakeRegistration(credential) =>
                w.writeArrayHeader(2)
                    .writeInt(0)
                    .write(credential)

            case Certificate.StakeDeregistration(credential) =>
                w.writeArrayHeader(2)
                    .writeInt(1)
                    .write(credential)

            case Certificate.StakeDelegation(credential, poolKeyHash) =>
                w.writeArrayHeader(3)
                    .writeInt(2)
                    .write(credential)
                    .write(poolKeyHash)

            case Certificate.PoolRegistration(
                  operator,
                  vrfKeyHash,
                  pledge,
                  cost,
                  margin,
                  rewardAccount,
                  poolOwners,
                  relays,
                  poolMetadata
                ) =>
                w.writeArrayHeader(2)
                w.writeInt(3)
                // Write operator
                w.write(operator)

                // Write VRF key hash
                w.write(vrfKeyHash)

                // Write pledge
                w.writeLong(pledge.value)

                // Write cost
                w.writeLong(cost.value)

                // Write margin
                w.write(margin)

                // Write reward account
                w.write(rewardAccount)

                // Write pool owners as a set
                w.write(poolOwners)

                // Write relays as an array
                w.write(relays)

                // Write pool metadata or null
                w.write(poolMetadata)

            case Certificate.PoolRetirement(poolKeyHash, epochNo) =>
                w.writeArrayHeader(3)
                    .writeInt(4)
                    .write(poolKeyHash)
                    .writeLong(epochNo)

            case Certificate.RegCert(credential, coin) =>
                w.writeArrayHeader(3)
                    .writeInt(7)
                    .write(credential)
                    .write(coin)

            case Certificate.UnregCert(credential, coin) =>
                w.writeArrayHeader(3)
                    .writeInt(8)
                    .write(credential)
                    .write(coin)

            case Certificate.VoteDelegCert(credential, drep) =>
                w.writeArrayHeader(3)
                    .writeInt(9)
                    .write(credential)
                    .write(drep)

            case Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep) =>
                w.writeArrayHeader(4)
                    .writeInt(10)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(drep)

            case Certificate.StakeRegDelegCert(credential, poolKeyHash, coin) =>
                w.writeArrayHeader(4)
                    .writeInt(11)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(coin)

            case Certificate.VoteRegDelegCert(credential, drep, coin) =>
                w.writeArrayHeader(4)
                    .writeInt(12)
                    .write(credential)
                    .write(drep)
                    .write(coin)

            case Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, coin) =>
                w.writeArrayHeader(5)
                    .writeInt(13)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(drep)
                    .write(coin)

            case Certificate.AuthCommitteeHotCert(
                  committeeColdCredential,
                  committeeHotCredential
                ) =>
                w.writeArrayHeader(3)
                    .writeInt(14)
                    .write(committeeColdCredential)
                    .write(committeeHotCredential)

            case Certificate.ResignCommitteeColdCert(committeeColdCredential, anchor) =>
                w.writeArrayHeader(3)
                    .writeInt(15)
                    .write(committeeColdCredential)
                    .write(anchor)

            case Certificate.RegDRepCert(drepCredential, coin, anchor) =>
                w.writeArrayHeader(4)
                    .writeInt(16)
                    .write(drepCredential)
                    .write(coin)
                    .write(anchor)

            case Certificate.UnregDRepCert(drepCredential, coin) =>
                w.writeArrayHeader(3)
                    .writeInt(17)
                    .write(drepCredential)
                    .write(coin)

            case Certificate.UpdateDRepCert(drepCredential, anchor) =>
                w.writeArrayHeader(3)
                    .writeInt(18)
                    .write(drepCredential)
                    .write(anchor)

    given Decoder[Certificate] with
        def read(r: Reader): Certificate =
//            println(s"Certificate: ${DataItem.stringify(r.dataItem())}")
            r.readArrayHeader()
            val tag = r.readInt()
//            println(s"Certificate tag: ${tag}")

            tag match
                case 0 => Certificate.StakeRegistration(r.read[Credential]())
                case 1 => Certificate.StakeDeregistration(r.read[Credential]())
                case 2 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[Hash28]()
                    Certificate.StakeDelegation(credential, poolKeyHash)
                case 3 =>
                    val operator = r.read[AddrKeyHash]()
                    val vrfKeyHash = r.read[Hash32]()
                    val pledge = r.read[Coin]()
                    val cost = r.read[Coin]()
                    val margin = r.read[UnitInterval]()
                    val rewardAccount = r.read[RewardAccount]()
                    val poolOwners = readSet[AddrKeyHash](r).get
                    val relays = r.read[Seq[Relay]]()
                    val poolMetadata = r.read[Option[PoolMetadata]]()

                    Certificate.PoolRegistration(
                      operator = operator,
                      vrfKeyHash = vrfKeyHash,
                      pledge = pledge,
                      cost = cost,
                      margin = margin,
                      rewardAccount = rewardAccount,
                      poolOwners = poolOwners,
                      relays = relays,
                      poolMetadata = poolMetadata
                    )
                case 4 =>
                    val poolKeyHash = r.read[Hash28]()
                    val epochNo = r.readLong()
                    Certificate.PoolRetirement(poolKeyHash, epochNo)
                case 7 =>
                    val credential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.RegCert(credential, coin)
                case 8 =>
                    val credential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.UnregCert(credential, coin)
                case 9 =>
                    val credential = r.read[Credential]()
                    val drep = r.read[DRep]()
                    Certificate.VoteDelegCert(credential, drep)
                case 10 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[Hash28]()
                    val drep = r.read[DRep]()
                    Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep)
                case 11 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[Hash28]()
                    val coin = r.read[Coin]()
                    Certificate.StakeRegDelegCert(credential, poolKeyHash, coin)
                case 12 =>
                    val credential = r.read[Credential]()
                    val drep = r.read[DRep]()
                    val coin = r.read[Coin]()
                    Certificate.VoteRegDelegCert(credential, drep, coin)
                case 13 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[Hash28]()
                    val drep = r.read[DRep]()
                    val coin = r.read[Coin]()
                    Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, coin)
                case 14 =>
                    val committeeColdCredential = r.read[Credential]()
                    val committeeHotCredential = r.read[Credential]()
                    Certificate.AuthCommitteeHotCert(
                      committeeColdCredential,
                      committeeHotCredential
                    )
                case 15 =>
                    val committeeColdCredential = r.read[Credential]()
                    val anchor = r.read[Nullable[Option[Anchor]]]()
                    Certificate.ResignCommitteeColdCert(committeeColdCredential, anchor)
                case 16 =>
                    val drepCredential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    val anchor = r.read[Nullable[Option[Anchor]]]()
                    Certificate.RegDRepCert(drepCredential, coin, anchor)
                case 17 =>
                    val drepCredential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.UnregDRepCert(drepCredential, coin)
                case 18 =>
                    val drepCredential = r.read[Credential]()
                    val anchor = r.read[Nullable[Option[Anchor]]]()
                    Certificate.UpdateDRepCert(drepCredential, anchor)
                case _ =>
                    r.validationFailure(s"Unknown certificate type: $tag")

    /** Helper to read a Set from CBOR */
    private def readSet[A](r: Reader)(using decoder: Decoder[A]): Option[Set[A]] =
        // Check for indefinite array tag (258)
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then
                r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            val set = r.read[Set[A]]()
            if set.isEmpty then r.validationFailure("Set must be non-empty")
            Some(set)
        else
            val set = r.read[Set[A]]()
            if set.isEmpty then None else Some(set)
