package scalus.ledger.api.v1

import scalus.uplc.Data.ToData
import scalus.uplc.ToData
import scalus.builtins
import scalus.builtins.Builtins
import scalus.Compile

@Compile
object ToDataInstances {
    import scalus.uplc.ToDataInstances.given
    import scalus.uplc.Data.toData

    given ToData[PubKeyHash] = (a: PubKeyHash) => a.hash.toData
    given ToData[TxId] = (a: TxId) =>
        Builtins.mkConstr(0, Builtins.mkCons(a.hash.toData, Builtins.mkNilData()))

    given ToData[TxOutRef] = ToData.deriveCaseClass[TxOutRef](0)

    given DCertLift[T <: DCert]: ToData[T] = (a: T) =>
        a match
            case DCert.DelegRegKey(cred) =>
                Builtins.mkConstr(0, Builtins.mkCons(cred.toData, Builtins.mkNilData()))
            case DCert.DelegDeRegKey(cred) =>
                Builtins.mkConstr(1, Builtins.mkCons(cred.toData, Builtins.mkNilData()))
            case DCert.DelegDelegate(cred, delegatee) =>
                Builtins.mkConstr(
                  2,
                  Builtins.mkCons(
                    cred.toData,
                    Builtins.mkCons(delegatee.toData, Builtins.mkNilData())
                  )
                )
            case DCert.PoolRegister(poolId, vrf) =>
                Builtins.mkConstr(
                  3,
                  Builtins.mkCons(
                    poolId.toData,
                    Builtins.mkCons(vrf.toData, Builtins.mkNilData())
                  )
                )
            case DCert.PoolRetire(poolId, epoch) =>
                Builtins.mkConstr(
                  4,
                  Builtins.mkCons(
                    poolId.toData,
                    Builtins.mkCons(epoch.toData, Builtins.mkNilData())
                  )
                )
            case DCert.Genesis => Builtins.mkConstr(5, Builtins.mkNilData())
            case DCert.Mir     => Builtins.mkConstr(6, Builtins.mkNilData())

    given ExtendedLift[A: ToData, T[A] <: Extended[A]]: ToData[T[A]] = (a: T[A]) =>
        a match
            case Extended.NegInf    => Builtins.mkConstr(0, Builtins.mkNilData())
            case Extended.Finite(a) => Builtins.mkConstr(1, a.toData :: Builtins.mkNilData())
            case Extended.PosInf    => Builtins.mkConstr(2, Builtins.mkNilData())

    given CredentialToData[T <: Credential]: ToData[T] = (a: T) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                Builtins.mkConstr(0, Builtins.mkCons(hash.toData, Builtins.mkNilData()))
            case Credential.ScriptCredential(hash) =>
                Builtins.mkConstr(1, hash.toData :: Builtins.mkNilData())

    given StakingCredentialLift[T <: StakingCredential]: ToData[T] = (a: T) =>
        a match
            case StakingCredential.StakingHash(cred) =>
                Builtins.mkConstr(0, Builtins.mkCons(cred.toData, Builtins.mkNilData()))
            case StakingCredential.StakingPtr(a, b, c) =>
                Builtins.mkConstr(
                  1,
                  Builtins.mkCons(
                    a.toData,
                    Builtins.mkCons(b.toData, Builtins.mkCons(c.toData, Builtins.mkNilData()))
                  )
                )

    given ScriptPurposeLift[T <: ScriptPurpose]: ToData[T] = (a: T) =>
        a match
            case ScriptPurpose.Minting(curSymbol) =>
                Builtins.mkConstr(0, Builtins.mkCons(curSymbol.toData, Builtins.mkNilData()))
            case ScriptPurpose.Spending(txOutRef) =>
                Builtins.mkConstr(1, Builtins.mkCons(txOutRef.toData, Builtins.mkNilData()))
            case ScriptPurpose.Rewarding(stakingCred) =>
                Builtins.mkConstr(2, Builtins.mkCons(stakingCred.toData, Builtins.mkNilData()))
            case ScriptPurpose.Certifying(cert) =>
                Builtins.mkConstr(3, Builtins.mkCons(cert.toData, Builtins.mkNilData()))

    given ToData[Address] = ToData.deriveCaseClass[Address](0)

    given ToData[TxOut] = ToData.deriveCaseClass[TxOut](0)

    given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)

    given LowerBoundToData[A: ToData]: ToData[LowerBound[A]] = (a: LowerBound[A]) =>
        a match
            case LowerBound(a, b) =>
                Builtins.mkConstr(
                  0,
                  Builtins.mkCons(
                    a.toData,
                    Builtins.mkCons(b.toData, Builtins.mkNilData())
                  )
                )
    given UpperBoundToData[A: ToData]: ToData[UpperBound[A]] = (a: UpperBound[A]) =>
        a match
            case UpperBound(a, b) =>
                Builtins.mkConstr(
                  0,
                  Builtins.mkCons(
                    a.toData,
                    Builtins.mkCons(b.toData, Builtins.mkNilData())
                  )
                )

    given intervalToData[A: ToData]: ToData[Interval[A]] = (a: Interval[A]) =>
        a match
            case Interval(a, b) =>
                Builtins.mkConstr(
                  0,
                  Builtins.mkCons(
                    a.toData,
                    Builtins.mkCons(b.toData, Builtins.mkNilData())
                  )
                )

    given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)

    given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}
