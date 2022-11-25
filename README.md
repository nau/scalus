# Scalus - Scala implementation of Cardano Plutus

## What is it planned to be?

Scalus is a set of libraries to work with Cardano Untyped Plutus Core that works on both JVM and JavaScript.
This includes:

- Untyped Plutus Core (UPLC) data types and functions
- Flat, CBOR, JSON serialization
- CEK UPLC evaluation machine including execution cost calculation
- UPLC parser and pretty printer
- Type safe UPLC expression builder, think of Plutarch
- Macros to generate UPLC code from Scala code, think of PlutusTx but simpler

## Preimage Validator Example

Here is a simple validator that checks that an signer of `pkh` PubKeyHash provided a preimage of a `hash` in a `redeemer`.
Below example is taken from `PreImageExampleSpec.scala` and it actually works!

```scala
def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
    // deserialize from Data
    val (hash, pkh) = fromData[(ByteString, ByteString)](datum)
    val preimage = fromData[ByteString](redeemer)
    val ctx = fromData[ScriptContext](ctxData)
    // get the transaction signatories
    val signatories = ctx.scriptContextTxInfo.txInfoSignatories

    def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
        case Nil              => throw new Exception("Not found")
        case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)
    // check that the transaction is signed by the public key hash
    findOrFail(signatories) { sig => sig.hash === pkh }
    // check that the preimage hashes to the hash
    if Builtins.sha2_256(preimage) === hash then ()
    else throw new RuntimeException("Wrong preimage")
    // throwing an exception compiles to UPLC error
}
// compile to Scalus Intermediate Representation, SIR
val compiled = compile(preimageValidator)
// convert SIR to UPLC
val validator = new SimpleSirToUplcLowering().lower(compiled)
val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
assert(flatEncoded.length == 1550)
```

## Why?

Haskell is great. It is a functional language with a lot of features.
But the developer experience is not great: huge compilation time, Nix rebuilds the whole Universe every time you git
pull, IDE support is not even close to what IntelliJ Idea has.
And there is no debugger.

Scala is great too. It is a functional language with a lot of features.
And the developer experience is great: debugger, code navigation and refactorings that just work.
Compilation time is instant compared to Haskell.

And you get a decent JavaScript support!

And you get the whole Java/Scala/Kotlin ecosystem for free!

And you write once, run anywhere! Including Node.js and browsers.

And it's easy to package and distribute! (Yes, the JAR is free!)

Maybe even Spring Plutus some day :)

## What currently works?

WARNING. It's not safe for production use! Yet.
This project seeks funding to make it production ready.

What you can play with:

- CEK UPLC evaluation machine works on both JVM and JavaScript (not all builtins are implemented yet)
- textual UPLC parser and pretty printer works on both JVM and JavaScript
- Flat UPLC serialization works on both JVM and JavaScript
- CBOR serialization of Data works on both JVM and JavaScript
- DeBruijn/unDeBruijn conversion works on both JVM and JavaScript
- Type safe UPLC expression builder prototype works on both JVM and JavaScript
- There are a couple of simple validators that can be used for real.
- The PubKey validator is 95 bytes long! It's 20x smaller than the 1992 bytes long PlutusTx version!
- Scala macros to convert simple Scala extpressions to UPLC

## Comparison to PlutusTx, Aiken, Plutarch

PlutusTx compiles almost any Haskell program to UPLC.
Cons are that you can barely understand how the UPLC is generated and how to make it smaller.

Aiken is a separate programming language which is a pro and a con.

Plutarch is very low-level. Use it when you need precise control over a script generation.

Scalus aimes to be a better version of all the above.

- Scalus allows only a limited subset of Scala, that can be reasonably efficiently
compiled to UPLC without bloating the code.

- It's compiled to a fairly high-level human readable intermediate representation, SIR.

- The huge part of any usefull script is `ScriptContext` deserialization from `Data` representation.
Scalus also provides primitives to do your custom deserialization to reduce validator size.

Here is an optimized version of Preimage Validator from the above:

```scala
def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
  summon[FromData[(ByteString, ByteString)]](datum) match
    case (hash, pkh) =>
      val preimage = summon[FromData[ByteString]](redeemer)
      val signatories = summon[FromData[List[PubKeyHash]]](
        // deserialize only the signatories from the ScriptContext
        fieldAsData[ScriptContext](_.scriptContextTxInfo.txInfoSignatories)(ctxData)
      )

      def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
        case Nil              => throw new Exception("Not found")
        case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)

      findOrFail(signatories) { sig =>
        sig.hash === pkh
      }
      if Builtins.sha2_256(preimage) === hash then ()
      else throw new RuntimeException("Wrong preimage")
}

val compiled = compile(preimageValidator)
val validator = new SimpleSirToUplcLowering().lower(compiled)
val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator)).length
assert(flatSize == 261)
```

You can see that deserialising only the fields we actually need significantly reduces the script size:
261 bytes versus 1550!

This script compiles to `SIR` that can be pretty-printed in an Haskell-like syntax:

```scala
val compiled = compile(preimageValidator)
compiled.pretty.render(100)
```

outputs

```haskell
data List = Cons(head, tail) | Nil
data PubKeyHash = PubKeyHash(hash)
data Tuple2 = Tuple2(_1, _2)
let scalus.uplc.Data$.ByteStringFromData = {λ d -> unBData(d) } in
fun scalus.Predef$.List$.Cons$.apply head tail = Cons(head, tail)
in fun scalus.uplc.Data$.ListFromData evidence$1 d =
       let fromA = evidence$1 in
       let ls = unListData(d) in
       fun loop ls =
           if nullList(ls) then Nil()
           else
               scalus.Predef$.List$.Cons$.apply(fromA(headList(ls)), loop(tailList(ls)))
       in loop(ls)
in let scalus.ledger.api.v1.Instances$.given_FromData_PubKeyHash =
  {λ d -> let hash = scalus.uplc.Data$.ByteStringFromData(d) in PubKeyHash(hash) }
in
fun scalus.uplc.Data$.unsafeTupleFromData fromA fromB d =
    let pair = unConstrData(d) in
    let args = sndPair(pair) in
    Tuple2(fromA(headList(args)), fromB(headList(tailList(args))))
in fun scalus.PreImageExampleSpec._$preimageValidator datum redeemer ctxData =
       match scalus.uplc.Data$.unsafeTupleFromData(
         scalus.uplc.Data$.ByteStringFromData,
         scalus.uplc.Data$.ByteStringFromData,
         datum
       ) with
         case Tuple2(hash, pkh) ->
           let preimage = scalus.uplc.Data$.ByteStringFromData(redeemer) in
           let signatories =
             scalus.uplc.Data$.ListFromData(
               scalus.ledger.api.v1.Instances$.given_FromData_PubKeyHash,
               {λ ddd ->
                 {λ d ->
                   headList(
                     tailList(
                       tailList(
                         tailList(tailList(tailList(tailList(tailList(sndPair(unConstrData(d)))))))
                       )
                     )
                   )
                 }({λ d -> headList(sndPair(unConstrData(d))) }(ddd))
               }(ctxData)
             )
           in
           fun findOrFail lst p =
               match lst with
                 case Cons(head, tail) -> if p(head) then () else findOrFail(tail, p)
                 case Nil -> ERROR
           in let _ =
             findOrFail(signatories, {λ sig -> equalsByteString(sig({λ hash -> hash }), pkh) })
           in
           if equalsByteString(sha2_256(preimage), hash) then () else ERROR
in {λ datum redeemer ctxData ->
     scalus.PreImageExampleSpec._$preimageValidator(datum, redeemer, ctxData)
   }
```

## Minting Policy Example

A simple minting policy script. The source is in `MintingPolicyExampleSpec`.
This example compiles to UPLC and correctly works using either Scalus implementation of CEK machine or Plutus CEK machine.
I use `uplc` utility from the Plutus repository.

```scala
  def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokenName: ByteString,
        amount: BigInt,
        redeemer: Unit,
        ctxData: Data
    ): Unit = {
      val ctx = summon[Data.FromData[ScriptContext]](ctxData)
      val txInfo = ctx.scriptContextTxInfo
      val txInfoInputs = txInfo.txInfoInputs
      val minted = txInfo.txInfoMint
      val purpose = ctx.scriptContextPurpose
      val ownSymbol = purpose match
        case Minting(curSymbol) => curSymbol
        case Spending(txOutRef) => throw new RuntimeException("Spending context is not supported")
        case Rewarding(stakingCred) =>
          throw new RuntimeException("Rewarding context is not supported")
        case Certifying(cert) => throw new RuntimeException("Certifying context is not supported")

      def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
        case Nil              => throw new Exception("Not found")
        case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)

      def findToken(tokens: List[(ByteString, BigInt)]): Unit =
        findOrFail(tokens) { token =>
          token match
            case (tn, amt) => tn === tokenName && amt === amount
        }

      def ensureMinted(minted: Value): Unit = {
        findOrFail(minted) { asset =>
          asset match
            case (curSymbol, tokens) =>
              if curSymbol === ownSymbol
              then
                findOrFail(tokens) { tokens =>
                  tokens match
                    case (tn, amt) => tn === tokenName && amt === amount
                }
                true
              else false
        }
      }

      def ensureSpendsTxOut(inputs: List[TxInInfo]): Unit = findOrFail(inputs) { txInInfo =>
        txInInfo.txInInfoOutRef match
          case TxOutRef(txOutRefTxId, txOutRefIdx) =>
            txOutRefTxId.hash === txId && txOutRefIdx === txOutIdx
      }
      ensureMinted(minted)
      ensureSpendsTxOut(txInfoInputs)
    }

    val compiled = compile(
      mintingPolicyScript(
        hoskyMintTxOutRef.txOutRefId.hash,
        hoskyMintTxOutRef.txOutRefIdx,
        ByteString.fromHex("484f534b59"),
        BigInt("1000000000000000"),
        _,
        _
      )
    )
```

## Taste of type safe low level UPLC API

```scala
/// PubKey style validator. Checks whether the transaction has a specific signature
def pubKeyValidator(pkh: PubKeyHash): Expr[Unit => Unit => Data => Unit] =
  lam { redeemer =>
    lam { datum =>
      lam { ctx =>
        val txInfoSignatories: Expr[List[Data]] = unListData(
          fieldAsData[ScriptContext](_.scriptContextTxInfo.txInfoSignatories).apply(ctx)
        )

        val search = rec[List[Data], Unit] { self =>
          lam { signatories =>
            // signatories.head.pubKeyHash
            val head = headList.apply(signatories)
            val headPubKeyHash = unBData(head)
            !chooseList(signatories)(error("Signature not found")) {
              ~ifThenElse2(equalsByteString(headPubKeyHash)(pkh.hash))(()) {
                self(tailList(signatories))
              }
            }
          }
        }
        search(txInfoSignatories)
      }
    }
  }
```