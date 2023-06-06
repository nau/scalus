# Scalus Tutorial

## Introduction

You can find the code for this tutorial in the [TutorialSpec.scala](https://github.com/nau/scalus/blob/master/shared/src/test/scala/scalus/TutorialSpec.scala) file.

## Project setup

Add the following to your `build.sbt` file:

```scala
scalaVersion := "3.2.2"
libraryDependencies += "org.scalus" %% "scalus" % "0.1.0"
addCompilerPlugin("org.scalus" %% "scalus-plugin" % "0.1.0")
```

## Basic workflow

The basic workflow is to write a Scala program and then compile it to a Plutus script, 
similar to how PlutuxTx works.

You write a script using a small subset of Scala, 
which is then compiled to a Scalus Intermediate Representation (SIR) with `compile` function.

The SIR can be pretty-printed and reviewed.

The SIR is then compiled to Untyped Plutus Core (UPLC) that can be executed on the Cardano blockchain.

## Constans and primitives

```scala
import scalus.Compiler.compile
import scalus.*
import scalus.builtins
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.uplc.FromDataInstances.given

val constants = compile {
  val unit = ()
  val bool = true
  val int = BigInt(123)
  val bigint = BigInt("12345678901234567890")
  val implicitBigIng: BigInt = 123
  val emptyByteString = ByteString.empty
  val byteString = ByteString.fromHex("deadbeef")
  val byteString2 = hex"deadbeef"
  val string = "Scalus Rocks!"
  val emptyList = builtins.List.empty[BigInt]
  val list = builtins.List[BigInt](1, 2, 3)
  val pair = builtins.Pair(true, ())
}
```

## Builtin Functions

```scala
compile {
  // See scalus.builtins.Builtins for what is available
  val data = Builtins.mkI(123)
  val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
  val a = BigInt(1)
  val sum = a + 1 - a * 3 / 4 // arithmetic operators
  val equals = a === sum // comparison operators
}
```

## Data types

You can define your own data types using Scala case classes and enums.

```scala
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
   // no guards, no nested patterns, no type ascription
  active match
    case Empty => true
    case Active(account) =>
      account match
        case Account(_, balance) => balance === BigInt(123)
  // all cases must be covered or there must be a default case
  val isEmpty = active match
    case Empty => true
    case _     => false
}
```

## Control flow

```scala
compile {
  val a = BigInt(1)
  // if-then-else
  if a === BigInt(2) then ()
  // throwing an exception compiles to Plutus ERROR,
  // which aborts the evaluation of the script
  // the exception message can be translated to a trace message
  // using new SimpleSIRToUplcLowering(generateErrorTraces = true)
  else throw new Exception("not 2")
}
```

## Functions

```scala
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
val fromDataExample = compile {
  // The `fromData` function is used to convert a `Data` value to a Scalus value.
  val data = Builtins.mkI(123)
  // fromData is a summoner method for the `FromData` type class
  // there are instances for all built-in types
  val a = fromData[BigInt](data)

  // you can define your own `FromData` instances
  given FromData[Account] = (d: Data) => {
    val args = Builtins.unsafeDataAsConstr(d).snd
    new Account(args.head, args.tail.head)
  }
  val account = fromData[Account](data)
}
```

## Writing a validator

Here is a simple example of a Plutus V1 validator written in Scalus.

```scala
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.prelude
val context = compile {
  def validator(datum: Data, redeamder: Data, ctxData: Data) = {
    val ctx = fromData[ScriptContext](ctxData)
    prelude.List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash === hex"deadbeef")
  }
}
```

## Converting the Scalus code to Flat/CBOR encoded UPLC

The `compile` function converts the Scalus code to a `SIR` value, Scalus Intermediate Representation.
You then need to convert the `SIR` value to a UPLC value and encode it to Flat and then to CBOR.

```scala
val serializeToDoubleCborHex: String = {
  import scalus.sir.SimpleSirToUplcLowering
  import scalus.uplc.ProgramFlatCodec
  import scalus.uplc.Program
  import io.bullet.borer.Cbor
  import scalus.utils.Utils

  val uplc = new SimpleSirToUplcLowering(generateErrorTraces = true).lower(context)
  val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), uplc))
  val cbor = Cbor.encode(flatEncoded).toByteArray
  val doubleEncoded = Cbor.encode(cbor).toByteArray
  Utils.bytesToHex(doubleEncoded)
}
```

## Validator examples

[PreImage Validator](https://github.com/nau/scalus/blob/9ab69d5bdbe6be5c20b1f5cf99b8a2c2b051ee09/jvm/src/test/scala/scalus/PreImageExampleSpec.scala)

[MintingPolicy](https://github.com/nau/scalus/blob/9ab69d5bdbe6be5c20b1f5cf99b8a2c2b051ee09/shared/src/main/scala/scalus/examples/MintingPolicy.scala)
