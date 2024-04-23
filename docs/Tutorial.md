---
sidebar_position: 2
---
# Tutorial

## Basic workflow

The basic workflow is to write a Scala program and then compile it to a Plutus script,
similar to how PlutuxTx works.

You can store the Plutus script in a `*.plutus` file and use it with the Cardano CLI.
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
import scalus.builtin.Data

// Compile Scala code to Scalus Intermediate Representation (SIR)
val validator = compile {
    // A simple validator that always succeeds
    (datum: Data, redeemer: Data, context: Data) => ()
}
validator.pretty.render(80)
validator.toUplc().pretty.render(80)
validator.doubleCborHex(version = (1, 0, 0))
```

## Constans and primitives

Plutus supports the following primitive types: `unit`, `bool`, `integer`, `bytestring`, `string`, `data`,  `list`, `pair`.
Those types are represented in Scalus as `Unit`, `Boolean`, `BigInt`, `ByteString`, `String`, `Data`, `List`, `Pair` respectively.

We use Scala native types to represent `Unit`, `Boolean`, `BigInt`, and `String`.

Here is an example of how to define constants and use built-in types.

```scala mdoc:compile-only
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.*
import scalus.builtin.ByteString.given

val constants = compile {
    val unit = () // unit type
    val bool = true || false // boolean type
    val int = BigInt(123) // integer type
    val bigint = BigInt("12345678901234567890") // large integer value
    val implicitBigIng: BigInt = 123
    val emptyByteString = ByteString.empty
    val byteString = ByteString.fromHex("deadbeef")
    val byteStringUtf8 = ByteString.fromString("hello") // from utf8 encoded string
    val byteString2 = hex"deadbeef" // ByteString from hex string
    val string = "Scalus Rocks!" // string type
    val emptyList = List.empty[BigInt] // empty list
    val list = List[BigInt](1, 2, 3) // list of integers
    val pair = Pair(true, ()) // pair of boolean and unit
}
```

## Builtin Functions

```scala mdoc:compile-only
import scalus.builtin.*
import scalus.builtin.ByteString.given
import scalus.prelude.Prelude.{*, given}
compile {
    // See scalus.builtin.Builtins for what is available
    val data = Builtins.iData(123)
    val eqData = data == Builtins.iData(123) || data != Builtins.iData(123)
    val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
    val byteStringEq = hex"deadbeef" == ByteString.empty || hex"deadbeef" != ByteString.empty
    val stringEq = "deadbeef" == "" || "deadbeef" != ""
    val a = BigInt(1)
    val sum = a + 1 - a * 3 / 4 // arithmetic operators
    val intEquality = a == sum || a != sum
    val bool = !true || (false == true) != false && true // boolean operators
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
        case Active(account @ Account(_, balance)) => balance == BigInt(123)
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
    if a == BigInt(2) then ()
    // throwing an exception compiles to Plutus ERROR,
    // which aborts the evaluation of the script
    // the exception message can be translated to a trace message
    // using sir.toUplc(generateErrorTraces = true)
    else throw new Exception("not 2")
}
```

## Functions

```scala mdoc:compile-only
compile {
    val nonRecursiveLambda = (a: BigInt) => a + 1

    def recursive(a: BigInt): BigInt =
        if a == BigInt(0) then 0
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

```scala mdoc:compile-only
import scalus.builtin.*, Builtins.*, Data.*
import scalus.builtin.FromDataInstances.given

case class Account(hash: ByteString, balance: BigInt)

enum State:
    case Empty
    case Active(account: Account)


val fromDataExample = compile {
    // The `fromData` function is used to convert a `Data` value to a Scalus value.
    val data = iData(123)
    // fromData is a summoner method for the `FromData` type class
    // there are instances for all built-in types
    val a = fromData[BigInt](data)

    // you can define your own `FromData` instances
    {
        given FromData[Account] = (d: Data) => {
            val args = unConstrData(d).snd
            new Account(fromData[ByteString](args.head), fromData[BigInt](args.tail.head))
        }
        val account = fromData[Account](data)
    }

    // or your can you a macro to derive the FromData instance
    {
        given FromData[Account] = FromData.deriveCaseClass
        given FromData[State] = FromData.deriveEnum[State] {
            case 0 => d => State.Empty
            case 1 => FromData.deriveConstructor[State.Active]
        }
    }
}
```

## Writing a validator

Here is a simple example of a PlutusV2 validator written in Scalus.

```scala mdoc:compile-only
import scalus.ledger.api.v2.*
import scalus.ledger.api.v2.FromDataInstances.given
import scalus.builtin.ByteString.given
import scalus.builtin.Data.fromData
import scalus.prelude.List

// Use Scala 3 indentation syntax. Look ma, no braces! Like Python!
val pubKeyValidator = compile:
    def validator(datum: Data, redeamder: Data, ctxData: Data) =
        val ctx = fromData[ScriptContext](ctxData)
        List.findOrFail[PubKeyHash](ctx.txInfo.signatories):
            sig => sig.hash == hex"deadbeef"
```

## Converting the Scalus code to Flat/CBOR encoded UPLC

The `compile` function converts the Scalus code to a `SIR` value, Scalus Intermediate Representation.
You then need to convert the `SIR` value to a UPLC value and encode it to Flat and then to CBOR.

Many APIs require the HEX encoded string of double CBOR encoded Flat encoded UPLC program,
like `Hex(CborEncode(CborEncode(FlatEncode(Program(version, uplc)))))`.

```scala mdoc:compile-only
import scalus.*
import scalus.builtin.ByteString.given
import scalus.builtin.Data.fromData
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.v2.*
import scalus.ledger.api.v2.FromDataInstances.given
import scalus.prelude.List
import scalus.uplc.Program

val serializeToDoubleCborHex = {
    val pubKeyValidator = compile {
        def validator(datum: Data, redeamder: Data, ctxData: Data) = {
            val ctx = fromData[ScriptContext](ctxData)
            List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash == hex"deadbeef")
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
    Utils.writePlutusFile("pubKeyValidator.plutus", program, PlutusLedgerLanguage.PlutusV2)
    // or simply
    program.writePlutusFile("pubKeyValidator.plutus", PlutusLedgerLanguage.PlutusV2)
}

```

## Evaluating scripts

```scala mdoc:compile-only
import scalus.builtin.{*, given}
import scalus.ledger.api.*
import scalus.uplc.*, eval.*

def evaluation() = {
    val sir = compile {
        def usefulFunction(a: BigInt): BigInt = a + 1
        usefulFunction(1)
    }
    val term = sir.toUplc()
    // simply evaluate the term
    VM.evaluateTerm(term).pretty.render(80) // (con integer 2)
    // evaluate a flat encoded script and calculate the execution budget and logs
    val result =
        VM.evaluateScriptCounting(MachineParams.defaultParams, Program((1, 0, 0), term).flatEncoded)
    println(s"Execution budget: ${result.budget}")
    println(s"Evaluated term: ${result.term.pretty.render(80)}")
    println(s"Logs: ${result.logs.mkString("\n")}")

    // you can get the actual execution costs from protocol parameters JSON from cardano-cli
    lazy val machineParams = MachineParams.fromCardanoCliProtocolParamsJson(
      "JSON with protocol parameters",
      PlutusLedgerLanguage.PlutusV2
    )
    // or from blockfrost API
    lazy val machineParams2 = MachineParams.fromBlockfrostProtocolParamsJson(
      "JSON with protocol parameters",
      PlutusLedgerLanguage.PlutusV2
    )

    // CountingBudgetSpender is a budget spender that counts the total cost of the evaluation
    val countingBudgetSpender = CountingBudgetSpender()
    // TallyingBudgetSpender is a budget spender that counts the costs of each operation
    val tallyingBudgetSpender = TallyingBudgetSpender(countingBudgetSpender)
    val logger = Log()
    // use NoLogger to disable logging
    val noopLogger = NoLogger
    val cekMachine = CekMachine(
      MachineParams.defaultParams, // or use default params
      tallyingBudgetSpender,
      logger,
      JVMPlatformSpecific // platform specific functions. Use JSPlatformSpecific for Scala.js
    )
    val debruijnedTerm = DeBruijn.deBruijnTerm(term)
    try {
        cekMachine.evaluateTerm(debruijnedTerm)
    } catch {
        case e: StackTraceMachineError =>
            println(s"Error: ${e.getMessage}")
            println(s"Stacktrace: ${e.getCekStack}")
            println(s"Env: ${e.env}")
    }
    println(s"Execution budget: ${tallyingBudgetSpender.budgetSpender.getSpentBudget}")
    println(s"Logs: ${logger.getLogs.mkString("\n")}")
    println(
      s"Execution stats:\n${tallyingBudgetSpender.costs.toArray.sortBy(_._1.toString()).map {
        case (k, v) => s"$k: $v"
      }.mkString("\n")}"
    )
}
```
