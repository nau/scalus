# Scalus - DApps Development Platform for Cardano

![CI/Release](https://github.com/nau/scalus/actions/workflows/release.yml/badge.svg)
![Maven Central](https://img.shields.io/maven-central/v/org.scalus/scalus_3)
[![Discord](https://img.shields.io/discord/1105852427346911252.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/ygwtuBybsy)

## Write Cardano smart contracts in Scala 3

* Scala is a modern functional programming language that runs on JVM and JavaScript.
* Reuse your Scala code for your validator, frontend and backend.
* Write once, run anywhere. True full-stack development.
* Scala's powerful type system helps you write correct code.
* Benefit from a huge ecosystem of libraries and tools.
* Utilize testing frameworks like ScalaTest and ScalaCheck.
* Enjoy comprehensive IDE support: IntelliJ IDEA, VSCode.
* Advanced debugging support.
* Enhanced code formatting and linting, navigation, and refactoring.

## How It Works

Scalus compiles a subset of Scala code to Plutus Core, the language of Cardano smart contracts.

Scalus gives full control over the generated Plutus Core code.
Write efficient and compact smart contracts and squeeze the most out of the Cardano blockchain.

## Features

* Scala 3 to Cardano Plutus Core compiler
* Standard library for Plutus contracts development
* Plutus V1 and V2 support
* Plutus VM Interpreter
* Property-based testing library
* Untyped Plutus Core (UPLC) data types and functions
* Flat, CBOR, JSON serialization
* UPLC parser and pretty printer
* Type safe UPLC expression builder, think of Plutarch

## Scalus Starter Project

You can use the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.
Clone the repository and follow the instructions in the README.

## Show Me The Code

### Preimage Validator Example

Here is a simple validator that checks that an signer of `pkh` PubKeyHash provided a preimage of a `hash` in a `redeemer`.
Below example is taken from [`PreImageExampleSpec.scala`](https://github.com/nau/scalus/blob/master/jvm/src/test/scala/scalus/PreImageExampleSpec.scala)

```scala
def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
    // deserialize from Data
    val (hash, pkh) = fromData[(ByteString, ByteString)](datum)
    val preimage = fromData[ByteString](redeemer)
    val ctx = fromData[ScriptContext](ctxData)
    // get the transaction signatories
    val signatories = ctx.txInfo.signatories
    // check that the transaction is signed by the public key hash
    List.findOrFail(signatories) { sig => sig.hash == pkh }
    // check that the preimage hashes to the hash
    if Builtins.sha2_256(preimage) == hash then ()
    else throw new RuntimeException("Wrong preimage")
    // throwing an exception compiles to UPLC error
}
// compile to Scalus Intermediate Representation, SIR
val compiled = compile(preimageValidator)
// convert SIR to UPLC
val validator = compiled.toUplc()
val plutusScript = Program((1, 0, 0), validator).doubleCborHex
```

Here is a full example of a token minting/burning validator that works on both JVM and JavaScript:

[MintingPolicy.scala](https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/MintingPolicy.scala)

And here is a project that uses it in web frontend:
[Scalus Minting Example](https://github.com/nau/scalus/tree/master/examples-js)

## Comparison to PlutusTx, Aiken, Plutarch

PlutusTx compiles almost any Haskell program to UPLC.
Cons are that you can barely understand how the UPLC is generated and how to make it smaller.

Aiken is a new and young programming language which is a pro and a con. Can only be used for smart contracts.
Lacks property-based testing.

Plutarch is very low-level. Use it when you need precise control over a script generation.

Scalus aimes to be a better version of all the above.

* You can actually reuse Scala code for your validator, frontend and backend! The goal that PlutusTx failed to achieve.

* You can use existing Scala libraries for testing, including ScalaCheck and ScalaTest.

* Scala has a powerful type system that helps you write correct code. Check out [Stainless – Formal Verification for Scala](https://stainless.epfl.ch/) for formal verification.

* Scalus leverages all the development tools that Scala has, including IntelliJ Idea, VSCode, sbt, even GitHub CoPilot and ChatGPT! No need to learn new tools and languages.

* Debugger! It works!

* Scalus allows only a limited subset of Scala, that can be reasonably efficiently
compiled to UPLC without bloating the code.

* It's compiled to a fairly high-level human readable intermediate representation, SIR.

* The huge part of any usefull script is `ScriptContext` deserialization from `Data` representation.
Scalus also provides primitives to do your custom deserialization to reduce validator size.

## Support

You can ask questions on Scalus Discord: <https://discord.gg/ygwtuBybsy>

The project is looking for funding to make it production ready.
If you are interested, please contact me at [@atlanter](https://twitter.com/atlanter) on Twitter.
Follow the official Scalus Twitter account: [@Scalus3](https://twitter.com/Scalus3).

You can support the project by donating ADA or BTC to the following addresses:

ADA: addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m

BTC: bc1qzefh9we0frls8ktm0dx428v2dx3wtp6xu4hd8k

Please, consider becoming a sponsor on GitHub.

And vote for the project on Cardano Catalyst!
