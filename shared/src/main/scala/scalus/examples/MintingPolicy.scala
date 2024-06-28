package scalus.examples

import scalus.Compile
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.*
import scalus.prelude.List
import scalus.prelude.Maybe.*
import scalus.prelude.Prelude.given
import scalus.prelude.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.fromData
import scalus.builtin.FromDataInstances.given

case class TxInInfoTxOutRefOnly(txInInfoOutRef: TxOutRef)

case class MintingContext(inputs: List[TxOutRef], minted: Value, ownSymbol: CurrencySymbol)

@Compile
object MintingPolicy {
    import List.*
    import ScriptPurpose.*

    given Data.FromData[TxInInfoTxOutRefOnly] = (d: Data) =>
        new TxInInfoTxOutRefOnly(d.toConstr.snd.head.to[TxOutRef])

    protected final val hoskyMintTxOutRef = TxOutRef(
      TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
      0
    )
    protected final val hoskyMintTxOut = TxOut(
      address = Address(
        Credential.PubKeyCredential(
          PubKeyHash(
            hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"
          )
        ),
        Nothing
      ),
      Value.lovelace(BigInt("10000000")),
      Nothing
    )

    val simpleCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
        val ctx = ctxData.to[ScriptContext]
        val txInfo = ctx.txInfo
        val txInfoInputs = txInfo.inputs
        val minted = txInfo.mint
        val purpose = ctx.purpose
        val ownSymbol = purpose match
            case Minting(curSymbol) => curSymbol
            case Spending(txOutRef) => throw new RuntimeException("PS")
            case Rewarding(stakingCred) =>
                throw new RuntimeException("PR")
            case Certifying(cert) => throw new RuntimeException("PC")
        new MintingContext(
          List.map(txInfoInputs)(_.outRef),
          minted,
          ownSymbol
        )
    }

    val optimizedCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
        val txInfoData = ctxData.field[ScriptContext](_.txInfo)
        val txInfoInputs = txInfoData.field[TxInfo](_.inputs).to[List[TxInInfoTxOutRefOnly]]
        val minted = txInfoData.field[TxInfo](_.mint).to[Value]
        val ownSymbol =
            val purpose = ctxData.field[ScriptContext](_.purpose)
            val pair = purpose.toConstr
            val tag = pair.fst
            val args = pair.snd
            if tag == BigInt(0) then args.head.toByteString
            else throw new Exception("P")
        new MintingContext(
          List.map(txInfoInputs)(_.txInInfoOutRef),
          minted,
          ownSymbol
        )
    }
    /* Here we use a custom ScriptContext deserializer
       to avoid deserializing from Data fields that are not used in the script.
       This saves us more than 1000 bytes of the script size.
     */
    def mintingPolicyScript(deserializer: Data => MintingContext)(
        txId: ByteString,
        txOutIdx: BigInt,
        tokensToMint: AssocMap[ByteString, BigInt]
    ) = (redeemer: Unit, ctxData: Data) => {
        deserializer(ctxData) match
            case MintingContext(txOutRefs, minted, ownSymbol) =>
                val mintedTokens = AssocMap.lookup(minted)(ownSymbol) match
                    case Just(mintedTokens) => mintedTokens
                    case Nothing =>
                        throw new Exception("T")

                val checkSpendsTxOut = List.find(txOutRefs) {
                    case TxOutRef(txOutRefTxId, txOutRefIdx) =>
                        txOutRefTxId.hash == txId && txOutRefIdx == txOutIdx
                }

                val check = (b: Boolean, msg: String) => if b then () else throw new Exception(msg)
                checkSpendsTxOut match
                    // If the transaction spends the TxOut, then it's a minting transaction
                    case Just(input) => check(Value.equalsAssets(mintedTokens, tokensToMint), "M")
                    // Otherwise, it's a burn transaction
                    case Nothing =>
                        // check burned
                        val burned = List.all(AssocMap.toList(mintedTokens)) {
                            case (tokenName, amount) =>
                                lessThanInteger(amount, BigInt(0))
                        }
                        check(burned, "B")
    }

    val compiledOptimizedMintingPolicyScript = compile(
      mintingPolicyScript(optimizedCtxDeserializer)
    )

    val compiledMintingPolicyScript = compile(
      mintingPolicyScript(simpleCtxDeserializer)
    )

    // val cbor = Cbor.encode(flatEncoded).toByteArray
    // val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    // val doubleCborHex = Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
}

@Compile
object MintingPolicyV2 {
    import scalus.ledger.api.v2.*
    import scalus.ledger.api.v2.FromDataInstances.given
    import ScriptPurpose.*

    val simpleCtxV2Deserializer: Data => MintingContext = (ctxData: Data) => {
        val ctx = fromData[ScriptContext](ctxData)
        val txInfo = ctx.txInfo
        val txInfoInputs = txInfo.inputs
        val minted = txInfo.mint
        val purpose = ctx.purpose
        val ownSymbol = purpose match
            case Minting(curSymbol) => curSymbol
            case Spending(txOutRef) => throw new RuntimeException("PS")
            case Rewarding(stakingCred) =>
                throw new RuntimeException("PR")
            case Certifying(cert) => throw new RuntimeException("PC")
        new MintingContext(
          List.map(txInfoInputs)(_.outRef),
          minted,
          ownSymbol
        )
    }

    val compiledMintingPolicyScriptV2 = compile(
      MintingPolicy.mintingPolicyScript(simpleCtxV2Deserializer)
    )
}
