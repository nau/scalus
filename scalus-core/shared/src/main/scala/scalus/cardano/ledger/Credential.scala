package scalus.cardano.ledger

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key

/** Represents a credential in the Cardano blockchain. A credential can be either a key hash or a
  * script hash.
  */
enum Credential derives Codec.All:
    /** Key hash credential */
    @key(0) case KeyHash(keyHash: AddrKeyHash)

    /** Script hash credential */
    @key(1) case ScriptHash(scriptHash: scalus.cardano.ledger.ScriptHash)

    /** Check if this credential is a key hash */
    def isKeyHash: Boolean = this match
        case KeyHash(_) => true
        case _          => false

    /** Check if this credential is a script hash */
    def isScriptHash: Boolean = this match
        case ScriptHash(_) => true
        case _             => false
