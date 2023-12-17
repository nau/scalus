package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*
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
  val byteStringUtf8 = ByteString.fromString("hello") // utf8 encoded
  val byteString2 = hex"deadbeef"
  val string = "Scalus Rocks!"
  val emptyList = builtins.List.empty[BigInt]
  val list = builtins.List[BigInt](1, 2, 3)
  val pair = builtins.Pair(true, ())
}

val builtinFunctions = compile {
  // See scalus.builtins.Builtins for what is available
  val data = Builtins.mkI(123)
  val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
  val a = BigInt(1)
  val sum = a + 1 - a * 3 / 4 // arithmetic operators
  val equals = a === sum // comparison operators
}

case class Account(hash: ByteString, balance: BigInt)

enum State:
  case Empty
  case Active(account: Account)

import State.*
val dataTypes = compile {
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

val controlFlow = compile {
  val a = BigInt(1)
  // if-then-else
  if a === BigInt(2) then ()
  // throwing an exception compiles to Plutus ERROR,
  // which aborts the evaluation of the script
  // the exception message can be translated to a trace message
  // using sir.toUplc(generateErrorTraces = true)
  else throw new Exception("not 2")
}

val functions = compile {
  val nonRecursiveLambda = (a: BigInt) => a + 1
  def recursive(a: BigInt): BigInt =
    if a === BigInt(0) then 0
    else recursive(a - 1)
}

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

import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.prelude.List
val pubKeyValidator = compile {
  def validator(datum: Data, redeamder: Data, ctxData: Data) = {
    val ctx = fromData[ScriptContext](ctxData)
    List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash === hex"deadbeef")
  }
}

val serializeToDoubleCborHex = {
  import scalus.*
  import scalus.uplc.Program
  // convert to UPLC
  // generateErrorTraces = true will add trace messages to the UPLC program
  val uplc = pubKeyValidator.toUplc(generateErrorTraces = true)
  val programV1 = Program((1, 0, 0), uplc)
  val flatEncoded = programV1.flatEncoded // if needed
  val cbor = programV1.cborEncoded // if needed
  val doubleEncoded = programV1.doubleCborEncoded // if needed
  // in most cases you want to use the hex representation of the double CBOR encoded program
  programV1.doubleCborHex
  // also you can produce a pubKeyValidator.plutus file for use with cardano-cli
  import scalus.utils.Utils
  Utils.writePlutusFile("pubKeyValidator.plutus", programV1)
}

class TutorialSpec extends AnyFunSuite {
  test("pretty print") {
    // println(constants.pretty.render(80))
    // println(builtinFunctions.pretty.render(80))
    // println(dataTypes.pretty.render(80))
    // println(controlFlow.pretty.render(80))
    // println(functions.pretty.render(80))
    // println(modules.pretty.render(80))
    // println(fromDataExample.pretty.render(80))
    // println(context.pretty.render(80))
    // println(serializeToDoubleCborHex)
  }
}
