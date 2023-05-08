package scalus.ledger.api.v1

import scalus.uplc.Data.ToData
import scalus.uplc.Data

object ToDataInstances {
  import scalus.uplc.Data.toData

  given ToData[TxId] with
    def toData(a: TxId): Data = a.hash.toData

  given ToData[PubKeyHash] with
    def toData(a: PubKeyHash): Data = a.hash.toData

  given DCertLift[T <: DCert]: ToData[T] with
    def toData(a: T): Data =
      a match
        case DCert.DelegRegKey(cred: StakingCredential)   => Data.Constr(0, cred.toData :: Nil)
        case DCert.DelegDeRegKey(cred: StakingCredential) => Data.Constr(1, cred.toData :: Nil)
        case DCert.DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash) =>
          Data.Constr(2, cred.toData :: delegatee.toData :: Nil)
        case DCert.PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash) =>
          Data.Constr(3, poolId.toData :: vrf.toData :: Nil)
        case DCert.PoolRetire(poolId: PubKeyHash, epoch: BigInt) =>
          Data.Constr(4, poolId.toData :: epoch.toData :: Nil)
        case DCert.Genesis => Data.Constr(5, Nil)
        case DCert.Mir     => Data.Constr(6, Nil)

  given ExtendedLift[A: ToData, T[A] <: Extended[A]]: ToData[T[A]] with
    def toData(a: T[A]): Data =
      a match
        case Extended.NegInf    => Data.Constr(0, Nil)
        case Extended.Finite(a) => Data.Constr(1, a.toData :: Nil)
        case Extended.PosInf    => Data.Constr(2, Nil)

  given CredentialToData[T <: Credential]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: Credential.PubKeyCredential =>
          ToData.deriveProduct[Credential.PubKeyCredential](0).toData(a)
        case a: Credential.ScriptCredential =>
          ToData.deriveProduct[Credential.ScriptCredential](1).toData(a)

  given StakingCredentialLift[T <: StakingCredential]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: StakingCredential.StakingHash =>
          ToData.deriveProduct[StakingCredential.StakingHash](0).toData(a)
        case a: StakingCredential.StakingPtr =>
          ToData.deriveProduct[StakingCredential.StakingPtr](1).toData(a)

  given ScriptPurposeLift[T <: ScriptPurpose]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: ScriptPurpose.Minting  => ToData.deriveProduct[ScriptPurpose.Minting](0).toData(a)
        case a: ScriptPurpose.Spending => ToData.deriveProduct[ScriptPurpose.Spending](1).toData(a)
        case a: ScriptPurpose.Rewarding =>
          ToData.deriveProduct[ScriptPurpose.Rewarding](2).toData(a)
        case a: ScriptPurpose.Certifying =>
          ToData.deriveProduct[ScriptPurpose.Certifying](3).toData(a)
}
