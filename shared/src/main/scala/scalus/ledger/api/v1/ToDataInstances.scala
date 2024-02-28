package scalus.ledger.api.v1

import scalus.Compile
import scalus.builtin.Builtins
import scalus.builtin.Builtins.*
import scalus.builtin.Data.ToData
import scalus.builtin.ToData
import scalus.builtin.*

@Compile
object ToDataInstances {
    import scalus.builtin.ToDataInstances.given
    import scalus.builtin.Data.toData

    given ToData[PubKeyHash] = (a: PubKeyHash) => a.hash.toData
    given ToData[TxId] = (a: TxId) => constrData(0, mkCons(a.hash.toData, mkNilData()))

    given ToData[TxOutRef] = ToData.deriveCaseClass[TxOutRef](0)

    given DCertLift[T <: DCert]: ToData[T] = (a: T) =>
        a match
            case DCert.DelegRegKey(cred) =>
                constrData(0, mkCons(cred.toData, mkNilData()))
            case DCert.DelegDeRegKey(cred) =>
                constrData(1, mkCons(cred.toData, mkNilData()))
            case DCert.DelegDelegate(cred, delegatee) =>
                constrData(
                  2,
                  mkCons(
                    cred.toData,
                    mkCons(delegatee.toData, mkNilData())
                  )
                )
            case DCert.PoolRegister(poolId, vrf) =>
                constrData(
                  3,
                  mkCons(
                    poolId.toData,
                    mkCons(vrf.toData, mkNilData())
                  )
                )
            case DCert.PoolRetire(poolId, epoch) =>
                constrData(
                  4,
                  mkCons(
                    poolId.toData,
                    mkCons(epoch.toData, mkNilData())
                  )
                )
            case DCert.Genesis => constrData(5, mkNilData())
            case DCert.Mir     => constrData(6, mkNilData())

    given ExtendedLift[A: ToData, T[A] <: Extended[A]]: ToData[T[A]] = (a: T[A]) =>
        a match
            case Extended.NegInf    => constrData(0, mkNilData())
            case Extended.Finite(a) => constrData(1, a.toData :: mkNilData())
            case Extended.PosInf    => constrData(2, mkNilData())

    given CredentialToData[T <: Credential]: ToData[T] = (a: T) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                constrData(0, mkCons(hash.toData, mkNilData()))
            case Credential.ScriptCredential(hash) =>
                constrData(1, hash.toData :: mkNilData())

    given StakingCredentialLift[T <: StakingCredential]: ToData[T] = (a: T) =>
        a match
            case StakingCredential.StakingHash(cred) =>
                constrData(0, mkCons(cred.toData, mkNilData()))
            case StakingCredential.StakingPtr(a, b, c) =>
                constrData(
                  1,
                  mkCons(
                    a.toData,
                    mkCons(b.toData, mkCons(c.toData, mkNilData()))
                  )
                )

    given ScriptPurposeLift[T <: ScriptPurpose]: ToData[T] = (a: T) =>
        a match
            case ScriptPurpose.Minting(curSymbol) =>
                constrData(0, mkCons(curSymbol.toData, mkNilData()))
            case ScriptPurpose.Spending(txOutRef) =>
                constrData(1, mkCons(txOutRef.toData, mkNilData()))
            case ScriptPurpose.Rewarding(stakingCred) =>
                constrData(2, mkCons(stakingCred.toData, mkNilData()))
            case ScriptPurpose.Certifying(cert) =>
                constrData(3, mkCons(cert.toData, mkNilData()))

    given ToData[Address] = ToData.deriveCaseClass[Address](0)

    given ToData[TxOut] = ToData.deriveCaseClass[TxOut](0)

    given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)

    given LowerBoundToData[A: ToData]: ToData[LowerBound[A]] = (a: LowerBound[A]) =>
        a match
            case LowerBound(a, b) =>
                constrData(
                  0,
                  mkCons(
                    a.toData,
                    mkCons(b.toData, mkNilData())
                  )
                )
    given UpperBoundToData[A: ToData]: ToData[UpperBound[A]] = (a: UpperBound[A]) =>
        a match
            case UpperBound(a, b) =>
                constrData(
                  0,
                  mkCons(
                    a.toData,
                    mkCons(b.toData, mkNilData())
                  )
                )

    given intervalToData[A: ToData]: ToData[Interval[A]] = (a: Interval[A]) =>
        a match
            case Interval(a, b) =>
                constrData(
                  0,
                  mkCons(
                    a.toData,
                    mkCons(b.toData, mkNilData())
                  )
                )

    given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)

    given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}
