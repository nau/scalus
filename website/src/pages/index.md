---
hide_table_of_contents: false
toc_max_heading_level: 2
---
# Scalus - DApps Development Platform for Cardano

![CI/Release](https://github.com/nau/scalus/actions/workflows/release.yml/badge.svg)
![Maven Central](https://img.shields.io/maven-central/v/org.scalus/scalus_3)
[![Discord](https://img.shields.io/discord/1105852427346911252.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/ygwtuBybsy)

## Vision

Scalus is a platform for developing decentralized applications (DApps) on the Cardano blockchain.

The goal is to make a full-stack development experience for Cardano DApps as smooth as possible.
Using the same language, tools and code for frontend, backend and smart contracts development.

## Write Cardano smart contracts in Scala 3

* Scala is a modern functional programming language that runs on JVM, JavaScript and natively via LLVM.
* Reuse your Scala code for your validator, frontend and backend.
* Write once, run anywhere. True full-stack development.
* Scala's powerful type system helps you write correct code.
* Benefit from a huge ecosystem of libraries and tools.
* Utilize testing frameworks like ScalaTest and ScalaCheck
  for [property-based testing](https://en.wikipedia.org/wiki/Property-based_testing).
* Enjoy comprehensive IDE support: IntelliJ IDEA, VSCode and syntax highlighting on GitHub.
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
* Plutus VM Interpreter and execution budget calculation
* Property-based testing library
* Untyped Plutus Core (UPLC) data types and functions
* Flat, CBOR, JSON serialization
* UPLC parser and pretty printer
* Type safe UPLC expression builder, think of Plutarch
* Bloxbean [Cardano Client Lib](https://cardano-client.dev) integration

## Scalus Starter Project

You can use the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.
Clone the repository and follow the instructions in the README.

## Show Me The Code

### Preimage Validator Example

Here is a simple validator that checks that an signer of `pkh` PubKeyHash provided a preimage of a `hash` in a `redeemer`.
Below example is taken from [PreimageValidator](https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/PreimageValidator.scala#L19)

```scala 3
def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit =
    // deserialize from Data
    val (hash, pkh) = datum.to[(ByteString, ByteString)]
    val preimage = redeemer.toByteString
    val ctx = ctxData.to[ScriptContext]
    // get the transaction signatories
    val signatories = ctx.txInfo.signatories
    // check that the transaction is signed by the public key hash
    List.findOrFail(signatories) { sig => sig.hash == pkh }
    // check that the preimage hashes to the hash
    if sha2_256(preimage) == hash then ()
    else throw new RuntimeException("Wrong preimage")
  // throwing an exception compiles to UPLC error

// compile to Untyped Plutus Core (UPLC)
val compiled = compile(preimageValidator).toUplc()
// create a validator script, Plutus program version 1.0.0
val validator = Program((1, 0, 0), compiled)
// HEX encoded Plutus script, ready to be used in with cardano-cli or Blockfrost
val plutusScript = validator.doubleCborHex
// Create a Cardano .plutus file for this validator
validator.writePlutusFile(path, PlutusLedgerLanguage.PlutusV2)
```

Look at [SendTx](https://github.com/nau/scalus/blob/master/examples/src/main/scala/scalus/examples/SendTx.scala#L31) example for a full example of how to create a transaction with this validator.

### Scalus for budget calculation with Cardano Client Lib

Scalus can calculate the execution budget for your validator using the Cardano Client Lib. Just provide `ScalusTransactionEvaluator` to your `QuickTxBuilder`:

```scala 3
val signedTx = quickTxBuilder
  .compose(scriptTx)
  .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))
  // build your transaction
  .buildAndSign()
```

This will calculate the execution budget for your validator and add it to the redeemer of the transaction.

### AdaStream Example

Sources: [AdaStream Contract](https://github.com/nau/adastream/blob/main/contract.scala)

This project is a Cardano implementation of the [BitStream](https://github.com/RobinLinus/BitStream) protocol by Robin Linus, inventor of [BitVM](https://bitvm.org/)

Original paper: [BitStream: Decentralized File Hosting Incentivised via Bitcoin Payments
](https://robinlinus.com/bitstream.pdf)

#### TL;DR

* Alice wants to buy a file from Bob.
* Bob encrypts the file with a random key and sends it to Alice.
* Bob creates a bond contract on Cardano with a collateral and a commitment to the key and the file.
* Alice pays Bob for the file via a HTLC (Hashed Timelock Contract), using Cardano or Bitcoin Lightning Network.
* Alice decrypts the file with the key from the HTLC or takes the money back after the timeout.
* If Bob cheats, Alice can prove it and get the collateral from the bond contract.
* Bob can withdraw the collateral by revealing the key.

The project includes a bond contract and a HTLC contract for a fair exchange of files for ADA or other Cardano Native Tokens.

It's a CLI tool and a REST API server that allows you to create a bond contract, pay for a file, and decrypt it.

It has a set of tests that check the contract logic and its execution costs.

### Minting/Burning Example

Here is a full example of a token minting/burning validator that works on both JVM and JavaScript:

[MintingPolicy.scala](https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/MintingPolicy.scala)

And here is a project that uses it in web frontend:
[Scalus Minting Example](https://github.com/nau/scalus/tree/master/examples-js)

### Minimal Size Withdrawal Validator

The challenge was to create the smallest possible validator that checks a certain withdrawal exists in the transaction.

[The result is 92 bytes long script](https://gist.github.com/nau/b8996fe3e51b0e21c20479c5d8548ec7)

```scala 3
val validator = compile:
    (script_withdrawal_credential: Data, datum: Data, redeemer: Data, ctx: Data) =>
        def list_has(list: List[Pair[Data, Data]]): Unit =
            if list.head.fst == script_withdrawal_credential then ()
            else list_has(list.tail) // fails on empty list

        inline def withdrawal_from_ctx =
            unMapData(fieldAsData[ScriptContext](_.txInfo.withdrawals)(ctx))
        list_has(withdrawal_from_ctx)
```

## Roadmap

### Efficiently convert user defined data types from/to Plutus Data

Now, Scalus takes the same approach as PlutusTx.
This change makes it similar to Aiken, which will result in smaller and more efficient Plutus scripts in most cases.

### Support for Plutus V3

Plutus V3 is coming soon. Scalus will support all new built-ins and features.

### Single transaction building and signing API for backend and frontend

This will allow you to build and sign transactions in Scala and JavaScript using the same code.

### DApp development framework

A framework that will help you build DApps faster and easier.

You define your smart contracts, data types, and interaction endpoints in Scala.
The framework will generate the frontend and backend code for you.

Yes, your REST API, WebSocket, and GraphQL endpoints for your DApp.
And the JavaScript code to interact with your DApp from the browser.

## Comparison to PlutusTx, Aiken, Plutarch

### PlutusTx

PlutusTx compiles almost any Haskell program to UPLC.
Cons are that you can barely understand how the UPLC is generated and how to make it smaller.
PlutusTx also tends to generate a lot of boilerplate code making the final script size bigger and more expensive to run.

### Aiken

Aiken is a new and young programming language which is a pro and a con.
Can only be used for writing on-chain smart contracts.
Can't reuse code for on-chain and off-chain parts.
Doesn't have macros.
Doesn't allow a low-level control over the generated UPLC code.

### Plutarch

Plutarch is very low-level. Use it when you need precise control over a script generation.
It's a Haskell library so be prepared to write Haskell code with all its developer experience drawbacks.

### Plu-ts

Plu-ts is a TypeScript DSL for writing Plutus scripts.
With Scalus you can do the same and much more but in Scala, and produce JavaScript code.

### Scalus

Scalus aimes to be a better version of all the above.

* You can actually reuse Scala code for your validator, frontend and backend!
The goal that PlutusTx failed to achieve.

* You can use existing Scala libraries for testing, including ScalaCheck and ScalaTest.

* Scala has a powerful type system that helps you write correct code. Check
  out [Stainless – Formal Verification for Scala](https://stainless.epfl.ch/) for formal verification.

* Scalus leverages all the development tools that Scala has, including IntelliJ Idea, VSCode, sbt, even GitHub CoPilot
  and ChatGPT! No need to learn new tools and languages.

* Debugger! It works!

* Scalus allows only a limited subset of Scala, that can be reasonably efficiently
  compiled to UPLC without bloating the code.

* It's compiled to a fairly high-level human-readable intermediate representation, SIR.

* The huge part of any usefull script is `ScriptContext` deserialization from `Data` representation.
  Scalus also provides primitives to do your custom deserialization to reduce validator size.

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
