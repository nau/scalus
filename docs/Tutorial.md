---
sidebar_position: 2
---
# Tutorial

## Basic workflow

The basic workflow is to write a Scala program and then compile it to a Plutus script,
similar to how PlutuxTx works.

You can store the Plutus script in a *.plutus file and use it with the Cardano CLI.
Or use one of the Java/JavaScript libraries to construct transactions with the script.
[This example](https://github.com/nau/scalus/blob/d42d24385666efdb2690321958aa4fb8108e2db5/examples/src/main/scala/scalus/SendTx.scala) shows how to use the [cardano-client-lib](https://github.com/bloxbean/cardano-client-lib) to send transactions.

You write a script using a small subset of Scala,
which is then compiled to a Scalus Intermediate Representation (SIR) with `compile` function.

The SIR can be pretty-printed and reviewed.

The SIR is then compiled to Untyped Plutus Core (UPLC) that can be executed on the Cardano blockchain.

## Simple validator

```scala mdoc
import scalus.Compiler.compile
import scalus.*
import scalus.uplc.Data

val validator = compile {
  (datum: Data, redeemer: Data, context: Data) => ()
}
validator.toUplc().pretty.render(80)
validator.doubleCborHex(version = (1, 0, 0))
```

## Constans and primitives

```scala mdoc:compile-only
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.*
import scalus.builtin.ByteString.given
import scalus.prelude.Prelude.{*, given}

val constants = compile {
  val unit = ()
  val bool = true || false
  val int = BigInt(123)
  val bigint = BigInt("12345678901234567890")
  val implicitBigIng: BigInt = 123
  val emptyByteString = ByteString.empty
  val byteString = ByteString.fromHex("deadbeef")
  val byteStringUtf8 = ByteString.fromString("hello") // utf8 encoded
  val byteString2 = hex"deadbeef"
  val string = "Scalus Rocks!"
  val emptyList = builtin.List.empty[BigInt]
  val list = builtin.List[BigInt](1, 2, 3)
  val pair = builtin.Pair(true, ())
}
```

## Builtin Functions

```scala mdoc:compile-only
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.prelude.Prelude.{*, given}
compile {
  // See scalus.builtin.Builtins for what is available
  val data = Builtins.mkI(123)
  val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
  val a = BigInt(1)
  val sum = a + 1 - a * 3 / 4 // arithmetic operators
  val equals = a === sum // comparison operators
}
```

## Data types

You can define your own data types using Scala case classes and enums.

```scala mdoc:compile-only
import scalus.builtin.ByteString
import scalus.prelude.Prelude.{*, given}
case class Account(hash: ByteString, balance: BigInt)

enum State:
  case Empty
  case Active(account: Account)

import State.*
compile {
  // Tuple2 literals are supported
  val tuple = (true, BigInt(123))
  val empty = State.Empty // simple constructor
  // Use `new` to create an instance
  val account = new Account(ByteString.empty, tuple._2) // access tuple fields
  val active: State = new State.Active(account)
  val hash = account.hash // access case class fields
  // A simple pattern matching is supported
  // no guards, no type ascriptions.
  // Inner matches can be done only on single constructor case classes
  // Wildcard patterns are supported
  active match
    case Empty                                 => true
    case Active(account @ Account(_, balance)) => balance === BigInt(123)
  // all cases must be covered or there must be a default case
  val isEmpty = active match
    case Empty => true
    case _     => false
}
```

## Control flow

```scala mdoc:compile-only
import scalus.prelude.Prelude.{*, given}
compile {
  val a = BigInt(1)
  // if-then-else
  if a === BigInt(2) then ()
  // throwing an exception compiles to Plutus ERROR,
  // which aborts the evaluation of the script
  // the exception message can be translated to a trace message
  // using sir.toUplc(generateErrorTraces = true)
  else throw new Exception("not 2")
}
```

## Functions

```scala mdoc:compile-only
import scalus.prelude.Prelude.{*, given}
compile {
  val nonRecursiveLambda = (a: BigInt) => a + 1
  def recursive(a: BigInt): BigInt =
    if a === BigInt(0) then 0
    else recursive(a - 1)
}
```

## Modules and reusable code

You can define reusable code in a Scala object annotated with `@Compile`.
Scalus will compile the code to *.sir files and include them in the jar file.
This way you can distribute your code as a library.

Use `@Ignore` to exclude a definition from the compilation.

The `compile` will link the modules together and compile them to a single script.

```scala

@Compile
object ReusableCode {
  val constant = BigInt(1)
  def usefulFunction(a: BigInt): BigInt = a + 1
  @Ignore // this function is not compiled to UPLC
  def shouldNotBeInUplc() = ???
}

val modules = compile {
  ReusableCode.usefulFunction(ReusableCode.constant)
}
```

## FromData

FromData type class is used to convert a Data value to a Scalus value.

```scala
import scalus.uplc.Data
import scalus.uplc.Data.FromData
import scalus.uplc.Data.fromData
import scalus.uplc.FromData
val fromDataExample = compile {
  // The `fromData` function is used to convert a `Data` value to a Scalus value.
  val data = Builtins.mkI(123)
  // fromData is a summoner method for the `FromData` type class
  // there are instances for all built-in types
  val a = fromData[BigInt](data)

  // you can define your own `FromData` instances
  {
    given FromData[Account] = (d: Data) => {
      val args = Builtins.unsafeDataAsConstr(d).snd
      new Account(fromData[ByteString](args.head), fromData[BigInt](args.tail.head))
    }
    val account = fromData[Account](data)
  }

  // or your can you a macro to derive the FromData instance
  {
    given FromData[Account] = FromData.deriveCaseClass
    given FromData[State] = FromData.deriveEnum[State] {
      case 0 => d => Empty
      case 1 => FromData.deriveConstructor[State.Active]
    }
  }
}
```

## Writing a validator

Here is a simple example of a Plutus V1 validator written in Scalus.

```scala mdoc:compile-only
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.builtin.ByteString.given
import scalus.prelude.List
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.uplc.Data.fromData
val pubKeyValidator = compile {
  def validator(datum: Data, redeamder: Data, ctxData: Data) = {
    val ctx = fromData[ScriptContext](ctxData)
    List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash === hex"deadbeef")
  }
}
```

## Converting the Scalus code to Flat/CBOR encoded UPLC

The `compile` function converts the Scalus code to a `SIR` value, Scalus Intermediate Representation.
You then need to convert the `SIR` value to a UPLC value and encode it to Flat and then to CBOR.

Many APIs require the HEX encoded string of double CBOR encoded Flat encoded UPLC program,
like `Hex(CborEncode(CborEncode(FlatEncode(Program(version, uplc)))))`.

```scala mdoc:compile-only
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.builtin.ByteString.given
import scalus.prelude.List
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.uplc.Data.fromData
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.*
import scalus.uplc.Program

val serializeToDoubleCborHex = {
  val pubKeyValidator = compile {
    def validator(datum: Data, redeamder: Data, ctxData: Data) = {
      val ctx = fromData[ScriptContext](ctxData)
      List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash === hex"deadbeef")
    }
  }
  // convert to UPLC
  // generateErrorTraces = true will add trace messages to the UPLC program
  val uplc = pubKeyValidator.toUplc(generateErrorTraces = true)
  val program = Program((1, 0, 0), uplc)
  val flatEncoded = program.flatEncoded // if needed
  val cbor = program.cborEncoded // if needed
  val doubleEncoded = program.doubleCborEncoded // if needed
  // in most cases you want to use the hex representation of the double CBOR encoded program
  program.doubleCborHex
  // also you can produce a pubKeyValidator.plutus file for use with cardano-cli
  import scalus.utils.Utils
  Utils.writePlutusFile("pubKeyValidator.plutus", program, PlutusLedgerLanguage.PlutusV1)
  // or simply
  program.writePlutusFile("pubKeyValidator.plutus", PlutusLedgerLanguage.PlutusV1)
}

```

## Validator examples

[PreImage Validator](https://github.com/nau/scalus/blob/ce7a37edb06ef2e39794825ee4f81ff061198666/jvm/src/test/scala/scalus/PreImageExampleSpec.scala)

[MintingPolicy](https://github.com/nau/scalus/blob/612b4bd581c55cb6c68339247cfecfbe22e4e61d/shared/src/main/scala/scalus/examples/MintingPolicy.scala)
