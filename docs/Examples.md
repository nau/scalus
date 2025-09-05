---
sidebar_position: 3
---
# Examples

## AdaStream

## AdaStream Example

[AdaStream](https://github.com/nau/adastream) - Decentralized File Hosting Incentivised via Cardano Ada Payments

This project is a Cardano implementation of the [BitStream](https://github.com/RobinLinus/BitStream) protocol by Robin
Linus.

Original paper: [BitStream: Decentralized File Hosting Incentivised via Bitcoin Payments
](https://robinlinus.com/bitstream.pdf)

### TL;DR

* Alice wants to buy a file from Bob.
* Bob encrypts the file with a random key and sends it to Alice.
* Bob creates a bond contract on Cardano with a collateral and a commitment to the key and the file.
* Alice pays Bob for the file via a HTLC (Hash Time Lock Contract), using Cardano or Bitcoin Lightning Network.
* Alice decrypts the file with the key from the HTLC or takes the money back after the timeout.
* If Bob cheats, Alice can prove it and get the collateral from the bond contract.
* Bob can withdraw the collateral by revealing the key.

The project includes a bond contract and a HTLC contract for a fair exchange of files for ADA.

It's a CLI tool and a REST API server that allows you to create a bond contract, pay for a file, and decrypt it.

It has a set of tests that check the contract logic and its execution costs.

### AdaStream Contract

```scala 3 mdoc:compile-only
package adastream

import scalus.*
import scalus.Compiler.fieldAsData
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data, FromData, List, ToData, given}
import scalus.ledger.api.v1.Extended
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v2.*
import scalus.utils.Utils

extension (a: Array[Byte]) def toHex: String = Utils.bytesToHex(a)

@Compile
object BondContract {
    case class BondConfig(
        preimageHash: ByteString,
        encId: ByteString,
        serverPubKey: ByteString,
        serverPkh: ByteString
    )

    enum BondAction:
        case Withdraw(preimage: ByteString)
        case FraudProof(
            signature: ByteString,
            preimage: ByteString,
            encryptedChunk: ByteString,
            chunkHash: ByteString,
            chunkIndex: BigInt,
            merkleProof: Data // List of ByteString
        )

    import scalus.builtin.FromDataInstances.given
    given FromData[BondConfig] = FromData.deriveCaseClass
    given ToData[BondConfig] = ToData.deriveCaseClass[BondConfig](0)

    given FromData[BondAction] = FromData.deriveEnum {
        case 0 => FromData.deriveConstructor[BondAction.Withdraw]
        case 1 => FromData.deriveConstructor[BondAction.FraudProof]
    }
    given ToData[BondAction] = (a: BondAction) =>
        a match
            case BondAction.Withdraw(preimage) =>
                constrData(0, preimage.toData :: mkNilData())
            case BondAction.FraudProof(
                  signature,
                  preimage,
                  encryptedChunk,
                  chunkHash,
                  chunkIndex,
                  merkleProof
                ) =>
                constrData(
                  1,
                  signature.toData
                      :: preimage.toData
                      :: encryptedChunk.toData
                      :: chunkHash.toData
                      :: chunkIndex.toData
                      :: merkleProof
                      :: mkNilData()
                )

    def integerToByteString(num: BigInt): ByteString =
        def loop(div: BigInt, result: ByteString): ByteString = {
            val shifted = num / div
            val newResult = consByteString(shifted % 256, result)
            if shifted == BigInt(0) then newResult
            else loop(div * 256, newResult)
        }
        loop(1, ByteString.empty)

    def xorBytes(a: BigInt, b: BigInt): BigInt = {
        def xorHelper(a: BigInt, b: BigInt, pow: BigInt, result: BigInt): BigInt = {
            if pow == BigInt(256) then result
            else xorHelper(a / 2, b / 2, pow * 2, result + ((a + b) % 2) * pow)
        }
        xorHelper(a, b, 1, 0)
    }

    // a and b are of the same length
    def xor(a: ByteString, b: ByteString): ByteString = {
        val l1 = lengthOfByteString(a)
        val l2 = lengthOfByteString(b)
        def xorHelper(a: ByteString, b: ByteString, i: BigInt, result: ByteString): ByteString = {
            if i < 0 then result
            else {
                val byteA = indexByteString(a, i)
                val byteB = indexByteString(b, i)
                val xorByte = xorBytes(byteA, byteB)
                xorHelper(a, b, i - 1, consByteString(xorByte, result))
            }
        }
        if l1 == l2 then xorHelper(a, b, l1 - 1, ByteString.empty)
        else throw new Exception("X")
    }

    inline def verifyMerkleInclusionProof(
        merkleProof: Data,
        encryptedChunk: ByteString,
        chunkHash: ByteString,
        chunkIndex: BigInt,
        encId: ByteString
    ): Boolean =
        val encryptedChunkAndChunkHashHash = sha2_256(
          appendByteString(encryptedChunk, chunkHash)
        )
        def loop(index: BigInt, curHash: ByteString, siblings: List[Data]): ByteString =
            if siblings.isEmpty then curHash
            else
                val sibling = unBData(siblings.head)
                val nextHash =
                    if index % 2 == BigInt(0)
                    then sha2_256(appendByteString(curHash, sibling))
                    else sha2_256(appendByteString(sibling, curHash))
                loop(index / 2, nextHash, siblings.tail)

        val merkleRoot = loop(chunkIndex, encryptedChunkAndChunkHashHash, unListData(merkleProof))
        merkleRoot == encId

    def verifyPreimage(preimage: ByteString, preimageHash: ByteString): Boolean =
        preimageHash == sha2_256(preimage)

    inline def verifyFraudProof(
        chunkHash: ByteString,
        chunkIndex: BigInt,
        encId: ByteString,
        encryptedChunk: ByteString,
        merkleProof: Data,
        preimage: ByteString,
        preimageHash: ByteString,
        serverPubKey: ByteString,
        signature: ByteString
    ): Boolean =
        val verifyWrongChunkHash =
            // hash( Ei ⊕ hash( preimage || i) ) ≠ Hi
            val expectedChunkHash = sha2_256(
              trace("xor")(
                xor(
                  encryptedChunk,
                  trace("sha2_256")(
                    sha2_256(
                      appendByteString(
                        preimage,
                        trace("integerToByteString")(integerToByteString(chunkIndex))
                      )
                    )
                  )
                )
              )
            )
            expectedChunkHash != chunkHash
        trace("verifyWrongChunkHash")(true)
        val verifyValidClaimSignature = {
            val claim = appendByteString(encId, preimageHash)
            verifyEd25519Signature(
              serverPubKey,
              claim,
              signature
            )
        }
        trace("verifyValidClaimSignature")(true)

        val verifyValidPreimage = verifyPreimage(preimage, preimageHash)
        trace("verifyValidPreimage")(true)

        val merkleInclusionProofValid = verifyMerkleInclusionProof(
          merkleProof,
          encryptedChunk,
          chunkHash,
          chunkIndex,
          encId
        )
        (verifyWrongChunkHash || (throw new Exception("W")))
        && (verifyValidClaimSignature || (throw new Exception("S")))
        && (verifyValidPreimage || (throw new Exception("P")))
        && (merkleInclusionProofValid || (throw new Exception("M")))

    def bondContractValidator(datum: Data, redeemer: Data, ctxData: Data): Boolean = {
        fromData[BondConfig](datum) match
            case BondConfig(preimageHash, encId, serverPubKey, serverPkh) =>
                val a = trace("fromData[BondConfig]")(true)
                fromData[BondAction](redeemer) match
                    case BondAction.Withdraw(preimage) =>
                        val a = trace("BondAction.Withdraw(preimage)")(true)
                        val signatories = fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
                        val pkh =
                            unBData(unListData(signatories).head)
                        val verifySignature = pkh == serverPkh
                        val verifyValidPreimage = verifyPreimage(preimage, preimageHash)
                        (verifySignature || (throw new Exception("S")))
                        && (verifyValidPreimage || (throw new Exception("P")))
                    case BondAction.FraudProof(
                          signature,
                          preimage,
                          encryptedChunk,
                          chunkHash,
                          chunkIndex,
                          merkleProof
                        ) =>
                        val a = trace("BondAction.FraudProof")(true)
                        verifyFraudProof(
                          chunkHash,
                          chunkIndex,
                          encId,
                          encryptedChunk,
                          merkleProof,
                          preimage,
                          preimageHash,
                          serverPubKey,
                          signature
                        )
    }

    /*
     * HTLC contract validator
     */
    def makeHtlcContractValidator(
        clientPubKeyHash: Data,
        expiration: POSIXTime,
        hash: ByteString
    )(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        val validPreimage = hash == sha2_256(unBData(redeemer))
        val expired = {
            val txInfoData = fieldAsData[ScriptContext](_.txInfo)(ctxData)
            val signatoriesData = fieldAsData[TxInfo](_.signatories)(txInfoData)
            val txtime = fromData[POSIXTimeRange](
              fieldAsData[TxInfo](_.validRange)(txInfoData)
            )
            txtime.from.extended match
                case Extended.Finite(txtime) =>
                    val expired = expiration < txtime
                    val signedByClient = {
                        val signaturePubKeyHashData = unListData(signatoriesData).head
                        signaturePubKeyHashData == clientPubKeyHash
                    }
                    expired && signedByClient
                case _ => false
        }
        if validPreimage || expired then () else throw new Exception()
    }
}
```

## PreImage Validator

[PreImage Validator](https://github.com/nau/scalus/blob/ce7a37edb06ef2e39794825ee4f81ff061198666/jvm/src/test/scala/scalus/PreImageExampleSpec.scala)

## Minting Policy

[MintingPolicy](https://github.com/nau/scalus/blob/612b4bd581c55cb6c68339247cfecfbe22e4e61d/shared/src/main/scala/scalus/examples/MintingPolicy.scala)

## Betting

[Specification](https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/bet)

[Betting](https://github.com/nau/scalus/blob/master/scalus-examples/jvm/src/main/scala/scalus/examples/Betting.scala)
