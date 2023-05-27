package scalus.ledger.api.v1

import scalus.uplc.Data.ToData
import scalus.uplc.Data
import scalus.builtins
import scalus.builtins.Builtins

object ToDataInstances {
  import scalus.uplc.ToDataInstances.given
  import scalus.uplc.Data.toData

  given ToData[PubKeyHash] = (a: PubKeyHash) => a.hash.toData
  given ToData[TxId] = (a: TxId) =>
    Builtins.mkConstr(0, new builtins.List.Cons(a.hash.toData, builtins.List.Nil))
  given ToData[TxOutRef] = (a: TxOutRef) =>
    a match
      case TxOutRef(txId, idx) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(txId.toData, new builtins.List.Cons(idx.toData, builtins.List.Nil))
        )

  given DCertLift[T <: DCert]: ToData[T] = (a: T) =>
    a match
      case DCert.DelegRegKey(cred: StakingCredential) =>
        Builtins.mkConstr(0, new builtins.List.Cons(cred.toData, builtins.List.Nil))
      case DCert.DelegDeRegKey(cred: StakingCredential) =>
        Builtins.mkConstr(1, new builtins.List.Cons(cred.toData, builtins.List.Nil))
      case DCert.DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash) =>
        Builtins.mkConstr(
          2,
          new builtins.List.Cons(
            cred.toData,
            new builtins.List.Cons(delegatee.toData, builtins.List.Nil)
          )
        )
      case DCert.PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash) =>
        Builtins.mkConstr(
          3,
          new builtins.List.Cons(
            poolId.toData,
            new builtins.List.Cons(vrf.toData, builtins.List.Nil)
          )
        )
      case DCert.PoolRetire(poolId: PubKeyHash, epoch: BigInt) =>
        Builtins.mkConstr(
          4,
          new builtins.List.Cons(
            poolId.toData,
            new builtins.List.Cons(epoch.toData, builtins.List.Nil)
          )
        )
      case DCert.Genesis => Builtins.mkConstr(5, builtins.List.Nil)
      case DCert.Mir     => Builtins.mkConstr(6, builtins.List.Nil)

  given ExtendedLift[A: ToData, T[A] <: Extended[A]]: ToData[T[A]] = (a: T[A]) =>
    a match
      case Extended.NegInf    => Builtins.mkConstr(0, builtins.List.Nil)
      case Extended.Finite(a) => Builtins.mkConstr(1, a.toData :: builtins.List.Nil)
      case Extended.PosInf    => Builtins.mkConstr(2, builtins.List.Nil)

  given CredentialToData[T <: Credential]: ToData[T] = (a: T) =>
    a match
      case Credential.PubKeyCredential(hash) =>
        Builtins.mkConstr(0, new builtins.List.Cons(hash.toData, builtins.List.Nil))
      case Credential.ScriptCredential(hash) =>
        Builtins.mkConstr(1, hash.toData :: builtins.List.Nil)

  given StakingCredentialLift[T <: StakingCredential]: ToData[T] = (a: T) =>
    a match
      case StakingCredential.StakingHash(cred) =>
        Builtins.mkConstr(0, new builtins.List.Cons(cred.toData, builtins.List.Nil))
      case StakingCredential.StakingPtr(a, b, c) =>
        Builtins.mkConstr(
          1,
          new builtins.List.Cons(
            a.toData,
            new builtins.List.Cons(b.toData, new builtins.List.Cons(c.toData, builtins.List.Nil))
          )
        )

  given ScriptPurposeLift[T <: ScriptPurpose]: ToData[T] = (a: T) =>
    a match
      case ScriptPurpose.Minting(curSymbol) =>
        Builtins.mkConstr(0, new builtins.List.Cons(curSymbol.toData, builtins.List.Nil))
      case ScriptPurpose.Spending(txOutRef) =>
        Builtins.mkConstr(1, new builtins.List.Cons(txOutRef.toData, builtins.List.Nil))
      case ScriptPurpose.Rewarding(stakingCred) =>
        Builtins.mkConstr(2, new builtins.List.Cons(stakingCred.toData, builtins.List.Nil))
      case ScriptPurpose.Certifying(cert) =>
        Builtins.mkConstr(3, new builtins.List.Cons(cert.toData, builtins.List.Nil))

  given ToData[Address] = (a: Address) =>
    a match
      case Address(credential, stakingCredential) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            credential.toData,
            new builtins.List.Cons(stakingCredential.toData, builtins.List.Nil)
          )
        )

  given ToData[TxOut] = (a: TxOut) =>
    a match
      case TxOut(address, value, datumHash) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            address.toData,
            new builtins.List.Cons(
              value.toData,
              new builtins.List.Cons(datumHash.toData, builtins.List.Nil)
            )
          )
        )
  given ToData[TxInInfo] = (a: TxInInfo) =>
    a match
      case TxInInfo(outRef, resolved) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            outRef.toData,
            new builtins.List.Cons(resolved.toData, builtins.List.Nil)
          )
        )

  given LowerBoundToData[A: ToData]: ToData[LowerBound[A]] = (a: LowerBound[A]) =>
    a match
      case LowerBound(a, b) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            a.toData,
            new builtins.List.Cons(b.toData, builtins.List.Nil)
          )
        )
  given UpperBoundToData[A: ToData]: ToData[UpperBound[A]] = (a: UpperBound[A]) =>
    a match
      case UpperBound(a, b) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            a.toData,
            new builtins.List.Cons(b.toData, builtins.List.Nil)
          )
        )

  given intervalToData[A: ToData]: ToData[Interval[A]] = (a: Interval[A]) =>
    a match
      case Interval(a, b) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            a.toData,
            new builtins.List.Cons(b.toData, builtins.List.Nil)
          )
        )

  given ToData[TxInfo] = (a: TxInfo) =>
    a match
      case TxInfo(
            inputs,
            outputs,
            fee,
            mint,
            dcert,
            withdrawals,
            validRange,
            signatories,
            data,
            id
          ) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            inputs.toData,
            new builtins.List.Cons(
              outputs.toData,
              new builtins.List.Cons(
                fee.toData,
                new builtins.List.Cons(
                  mint.toData,
                  new builtins.List.Cons(
                    dcert.toData,
                    new builtins.List.Cons(
                      withdrawals.toData,
                      new builtins.List.Cons(
                        validRange.toData,
                        new builtins.List.Cons(
                          signatories.toData,
                          new builtins.List.Cons(
                            data.toData,
                            new builtins.List.Cons(id.toData, builtins.List.Nil)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

  given ToData[ScriptContext] = (a: ScriptContext) =>
    a match
      case ScriptContext(txInfo, purpose) =>
        Builtins.mkConstr(
          0,
          new builtins.List.Cons(
            txInfo.toData,
            new builtins.List.Cons(purpose.toData, builtins.List.Nil)
          )
        )

}
