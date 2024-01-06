---
hide_table_of_contents: true
---
# Scalus - DApps Development Platform for Cardano

## Scalus allows to write Cardano smart contracts in Scala 3

Leverage the whole Scala ecosystem developing your application: libraries, IDEs, debuggers, testing frameworks, etc.

This includes:

- Scala 3 to Cardano Plutus Core compiler
- Standard library for Plutus contracts development
- Plutus VM Interpreter
- Property-based testing library
- Untyped Plutus Core (UPLC) data types and functions
- Flat, CBOR, JSON serialization
- UPLC parser and pretty printer
- Type safe UPLC expression builder, think of Plutarch

## Scalus Starter Project

You can use the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.
Clone the repository and follow the instructions in the README.

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
  val signatories = ctx.txInfo.signatories
  // check that the transaction is signed by the public key hash
  List.findOrFail(signatories) { sig => sig.hash === pkh }
  // check that the preimage hashes to the hash
  if Builtins.sha2_256(preimage) === hash then ()
  else throw new RuntimeException("Wrong preimage")
  // throwing an exception compiles to UPLC error
}
// compile to Scalus Intermediate Representation, SIR
val compiled = compile(preimageValidator)
// convert SIR to UPLC
val validator = compiled.toUplc()
val flatSize = Program((1, 0, 0), validator).flatEncoded.length
assert(flatSize == 1586)
```

Here is a full example of a token minting/burning validator that works on both JVM and JavaScript:

[MintingPolicy.scala](https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/MintingPolicy.scala)

And here is a project that uses it in web frontend:
[Scalus Minting Example](https://github.com/nau/scalus/tree/master/examples-js)

## Why Scala?

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

- Scala compiler plugin to convert Scala code to UPLC
- All the Plutus builtins are implemented
- You can write Plutus V1 and V2 scripts
- Flat UPLC serialization works on both JVM and JavaScript
- CBOR serialization of Data works on both JVM and JavaScript
- DeBruijn/unDeBruijn conversion works on both JVM and JavaScript
- Type safe UPLC expression builder prototype works on both JVM and JavaScript
- There are a couple of simple validators that can be used for real.
- CEK UPLC evaluation machine works on both JVM and JavaScript (not all builtins are implemented yet)
- textual UPLC parser and pretty printer works on both JVM and JavaScript
- The PubKey validator is 138 bytes long! It's 14x smaller than the 1992 bytes long PlutusTx version!

## Comparison to PlutusTx, Aiken, Plutarch

PlutusTx compiles almost any Haskell program to UPLC.
Cons are that you can barely understand how the UPLC is generated and how to make it smaller.

Aiken is a separate programming language which is a pro and a con.

Plutarch is very low-level. Use it when you need precise control over a script generation.

Scalus aimes to be a better version of all the above.

- You can actually reuse Scala code for your validator, frontend and backend! The goal that PlutusTx failed to achieve.

- You can use existing Scala libraries for testing, including ScalaCheck and ScalaTest.

- Scalus leverages all the development tools that Scala has, including IntelliJ Idea, VSCode, sbt, even GitHub CoPilot and ChatGPT! No need to learn new tools and languages.

- Debugger! It works!

- Scalus allows only a limited subset of Scala, that can be reasonably efficiently
compiled to UPLC without bloating the code.

- It's compiled to a fairly high-level human readable intermediate representation, SIR.

- The huge part of any usefull script is `ScriptContext` deserialization from `Data` representation.
Scalus also provides primitives to do your custom deserialization to reduce validator size.

Here is an optimized version of Preimage Validator from the above:

```scala
def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
  fromData[(ByteString, ByteString)](datum) match
    case (hash, pkh) =>
      val preimage = fromData[ByteString](redeemer)
      val signatories = fromData[List[PubKeyHash]](
        // deserialize only the signatories from the ScriptContext
        fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
      )

      List.findOrFail(signatories) { sig => sig.hash === pkh }
      if Builtins.sha2_256(preimage) === hash then ()
      else throw new RuntimeException("Wrong preimage")
}

val compiled = compile(preimageValidator)
val validator = compiled.toUplc()
val flatSize = Program((1, 0, 0), validator).flatEncoded.length
assert(flatSize == 257)
```

You can see that deserialising only the fields we actually need significantly reduces the script size:
274 bytes versus 1684!

This script compiles to `SIR` that can be pretty-printed in an OCaml-like syntax:

```scala
val compiled = compile(preimageValidator)
compiled.pretty.render(100)
```

<details>
  <summary>Click me to see SIR</summary>

```ocaml
data Tuple2 = Tuple2(_1, _2)
data List = Cons(head, tail) | Nil
data PubKeyHash = PubKeyHash(hash)
fun scalus.uplc.FromDataInstances$.unsafeTupleFromData fromA fromB d =
    let pair = unConstrData(d) in
    let args = sndPair(pair) in
    Tuple2(fromA(headList(args)), fromB(headList(tailList(args))))
in fun scalus.uplc.FromDataInstances$.given_FromData_ByteString d = unBData(d)
in fun scalus.uplc.FromDataInstances$.ListFromData evidence$1 d =
       let ls = unListData(d) in
       fun loop ls =
           if nullList(ls) then Nil() else Cons(evidence$1(headList(ls)), loop(tailList(ls)))
       in loop(ls)
in fun scalus.ledger.api.v1.FromDataInstances$.fromDataPubKeyHash d =
       let hash = scalus.uplc.FromDataInstances$.given_FromData_ByteString(d) in
       PubKeyHash(hash)
in fun scalus.prelude.List$.findOrFail lst p =
       match lst with
         case Cons(head, tail) -> if p(head) then head else scalus.prelude.List$.findOrFail(tail, p)
         case Nil -> ERROR 'Not found'
in fun scalus.prelude.Prelude$.given_Eq_ByteString x y = equalsByteString(x, y)
in fun scalus.OptimizedPreimageValidator$.preimageValidator datum redeemer ctxData =
       match scalus.uplc.FromDataInstances$.unsafeTupleFromData(
         scalus.uplc.FromDataInstances$.given_FromData_ByteString,
         scalus.uplc.FromDataInstances$.given_FromData_ByteString,
         datum
       ) with
         case Tuple2(hash, pkh) ->
           let preimage = scalus.uplc.FromDataInstances$.given_FromData_ByteString(redeemer) in
           let signatories =
             scalus.uplc.FromDataInstances$.ListFromData(
               {λ d -> scalus.ledger.api.v1.FromDataInstances$.fromDataPubKeyHash(d) },
               let d$proxy1 = headList(sndPair(unConstrData(ctxData))) in
               headList(
                 tailList(
                   tailList(
                     tailList(
                       tailList(tailList(tailList(tailList(sndPair(unConstrData(d$proxy1))))))
                     )
                   )
                 )
               )
             )
           in
           let _ =
             scalus.prelude.List$.findOrFail(
               signatories,
               {λ sig -> scalus.prelude.Prelude$.given_Eq_ByteString(sig({λ hash -> hash }), pkh) }
             )
           in
           if
               let x$proxy1 = sha2_256(preimage) in
               scalus.prelude.Prelude$.given_Eq_ByteString(x$proxy1, hash) then ()
           else
               ERROR 'Wrong preimage'
in scalus.OptimizedPreimageValidator$.preimageValidator
```

</details>

## Support

You can ask questions on Scalus Discord: https://discord.gg/ygwtuBybsy

The project is looking for funding to make it production ready.
If you are interested, please contact me at [@atlanter](https://twitter.com/atlanter) on Twitter.
Follow the official Scalus Twitter account: [@Scalus3](https://twitter.com/Scalus3).

You can support the project by donating ADA or BTC to the following addresses:

ADA: addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m

BTC: bc1qzefh9we0frls8ktm0dx428v2dx3wtp6xu4hd8k

Please, consider becoming a sponsor on GitHub.

And vote for the project on Cardano Catalyst!