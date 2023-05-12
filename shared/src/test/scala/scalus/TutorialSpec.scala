import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus._
import scalus.builtins
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given

val constants = compile {
  val unit = ()
  val bool = true
  val int = BigInt(123)
  val bigint = BigInt("12345678901234567890")
  val implicitBigIng: BigInt = 123
  val emptyByteString = ByteString.empty
  val byteString = ByteString.fromHex("deadbeef")
  val string = "Scalus Rocks!"
  val emptyList = builtins.List.empty[BigInt]
  val list = builtins.List[BigInt](1, 2, 3)
  val pair = builtins.Pair(true, ())
}

val builtinFunctions = compile {
  // See scalus.builtins.Builtins for what is available
  val data = Builtins.mkI(123)
  val eq = Builtins.equalsByteString(ByteString.fromHex("deadbeef"), ByteString.empty)
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
  // Only a simple match is supported
  // no guards, no nested patterns, no type ascription, no wildcard patterns
  // all cases must be covered
  active match
    case Empty           => true
    case Active(account) => false
}

val controlFlow = compile {
  val a = BigInt(1)
  // if-then-else
  if a === BigInt(2) then ()
  // throwing an exception compiles to PLutus ERROR,
  // which aborts the evaluation of the script
  // the exception message can be translated to a trace message
  // using new SimpleSIRToUplcLowering(generateErrorTraces = true)
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
}

val modules = compile {
  ReusableCode.usefulFunction(ReusableCode.constant)
}

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

import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.prelude
val context = compile {
  def validator(redeamder: Data, datum: Data, ctxData: Data) = {
    val ctx = fromData[ScriptContext](ctxData)
    prelude.List.findOrFail[PubKeyHash](ctx.scriptContextTxInfo.txInfoSignatories)(sig =>
      sig.hash === ByteString.fromHex("deadbeef")
    )
  }
}

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

class TutorialSpec extends AnyFunSuite {
  test("pretty print") {
    /* println(constants.pretty.render(80))
    println(builtinFunctions.pretty.render(80))
    println(dataTypes.pretty.render(80))
    println(controlFlow.pretty.render(80))
    println(functions.pretty.render(80))
    println(modules.pretty.render(80))
    println(fromDataExample.pretty.render(80))
    println(context.pretty.render(80))
    println(serializeToDoubleCborHex) */
  }
}
