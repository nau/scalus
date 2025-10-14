Note: since Scalus validators are, first and foremost, Scala programs, usual coding guidelines
hold true for the smart contracts that you're going to write.

The following are a set of recommendations, guided by our experience in designing and maintaining smart contracts
written using Scalus.

## Overview

A Scalus validator is an object that extends the `Validator` trait, annotated with `@Compile`.

When defining the contract, one can define logic for 6 distinct purposes by overriding a corresponding method. Each
method corresponds to a respective Plutus Redeemer Tag type:

- `spend`
- `mint`
- `reward`
- `certify`
- `vote`
- `propose`

Each validator method takes `ScriptContext` as a parameter.

Note: when overriding the `Validator` methods, one has to define a function that does not take a script context
directly, e.g. for spending, you override the following function:

```scala
def spend(
           datum: Option[Data],
           redeemer: Data,
           tx: TxInfo,
           ownRef: TxOutRef
         ): Unit
```

Datum, redeemer, txinfo and ownref are values that are derived from the script context for convenience and contain no
information that couldn't be derived from the context only.

Thus, a typical contract will consist of a number of functions, often just of 1 function. A successful contract returns
a `Unit`, while errors are indicated by throwing errors, more on that later.

Let's take a look at an example contract, `HTLCValidator`, to illustrate the concepts above.

```scala
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.blueprint.{Application, Blueprint}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

type Preimage = ByteString
type Image = ByteString
type PubKeyHash = ByteString

// Contract Datum
case class ContractDatum(
                          committer: PubKeyHash,
                          receiver: PubKeyHash,
                          image: Image,
                          timeout: PosixTime
                        )derives FromData, ToData

// Redeemer
enum Action derives FromData, ToData:
  case Timeout
  case Reveal(preimage: Preimage)

@Compile
object HtlcValidator extends Validator:
  /** Spending script purpose validation
   */
  override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
    val ContractDatum(committer, receiver, image, timeout) =
      datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

    redeemer.to[Action] match
      case Action.Timeout =>
        require(tx.isSignedBy(committer), UnsignedCommitterTransaction)
        require(tx.validRange.isAfter(timeout), InvalidCommitterTimePoint)

      case Action.Reveal(preimage) =>
        require(tx.isSignedBy(receiver), UnsignedReceiverTransaction)
        require(!tx.validRange.isAfter(timeout), InvalidReceiverTimePoint)
        require(sha3_256(preimage) === image, InvalidReceiverPreimage)
  }

  // Helper methods
  extension (self: TxInfo)
    private def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
      self.signatories.exists {
        _.hash === pubKeyHash
      }

  extension (self: Interval)
    private infix def isAfter(timePoint: PosixTime): Boolean =
      self.from.boundType match
        case IntervalBoundType.Finite(time) => timePoint < time
        case _ => false

  // Error messages
  inline val InvalidDatum =
    "Datum must be a HtlcValidator.ContractDatum(committer, receiver, image, timeout)"
  inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
  inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
  inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
  inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
  inline val InvalidReceiverPreimage = "Invalid receiver preimage"

end HtlcValidator
```

At the very top of the file, you can see imports, which are present in most Scala programs.
Then, we declare *type aliases*, which enhance our types with semantics relevant to the contract.

After that, we see a definition of the datum and redeemer types, which are then used to turn the `Data` corresponding to
a Redeemer to an instance of that types, allowing us to utilize Scalas strict type system.

In the body of the `object HTLCValidator`, where object is a Scala keyword meaning a stateless type with no instances,
we can see the `override spend` declaration, which contains the logic of the contract.

After deserializing the datum and the redeemer using the Scalus' `to[ContractDatum]` and `to[Action]`, which are
available
for every type that `derive`s `FromData`, we see the branching of behaviour. The branching is done via pattern
matching -- a feature of the Scala language that allows, for algebraic data types, to express the behaviour depending on
the type of the variable at runtime.

For each of the two redeemers, we define the requirements necessary for the `Spend` transaction to be successful.
If any of the required conditions don't hold true, the validator execution ends with an error. The list of all errors
is enumerated at the bottom of the contract, each containing a message describing why the spending was forbidden.

Each validator is, in essence, a boolean function -- it either allows to perform the desired action, e.g. spend
the script locked funds, or forbids it.

Thus, the logic is just a sequence of binary checks.
In Scalus, this usually means a series of `require` calls, where the first parameter is the boolean invariant to check,
and the second parameter is the error that is thrown if the condition is false.

In the HTLCValidator, you can see it clearly: the timeout is only valid if the initiating transaction is signed by the
correct key and the necessary amount of time has passed.
This is neatly expressed in 2 lines of Scala code:

```scala
require(tx.isSignedBy(committer), UnsignedCommitterTransaction)
require(tx.validRange.isAfter(timeout), InvalidCommitterTimePoint)
```

Note:
a transaction does not have `isSignedBy` by default, but since we're writing a Scala program, we can declare and
extension method and add a method to an object, simplifying the code, allowing us to express the requirements in one
simple statement.
