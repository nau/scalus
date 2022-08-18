# Scalus - Scala implementation of Cardano Plutus

## What it is going to be?

Scalus is a set of libraries to work with Cardano Untyped Plutus Core that works on both JVM and JavaScript.
This includes:

- Untyped Plutus Core (UPLC) data types and functions
- Flat, CBOR, JSON serialization
- CEK UPLC evaluation machine including execution cost calculation
- UPLC parser and pretty printer
- Type safe UPLC expression builder, think of Plutarch
- Macros to generate UPLC code from Scala code, think of PlutusTx but better

Scalus is also a Scala compiler plugin that converts Scala code into Plutus.

## Example

```scala
def validator(
               datum: Datum,
               redeemer: Redeemer,
               ctx: ScriptContext
             ): Boolean = {
  val (hash, pkh) = Builtins.unsafeFromBuiltinData[(ByteString, ByteString)](datum)
  Builtins.sha256(redeemer) == hash && ctx.scriptContextTxInfo.txInfoSignatories.contains(pkh)
}

val script = Scalus.compile(validator _)
// this can be run on Node.js or in a browser
val datum =
  (
    hex"c9d04c9565fc665c80681fb1d829938026871f66e14f501e08531df66938a789",
    hex"dfc1ed82c9fc06409cc0d137e561fef37d1072e415e637ca54a2d044f3777da9"
  )
val redeemer = "Test"
val (result, budget) = Scalus.eval(script, datum, redeemer, Scalus.mkContext())

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

## Taste of type safe low level UPLC API

```scala
  /// PubKey style validator. Checks whether the transaction has a specific signature
  def pubKeyValidator(pkh: PubKeyHash): Expr[Unit => Unit => Data => Unit] =
    lam[Unit]("redeemer") { _ =>
      lam[Unit]("datum") { _ =>
        lam[Data]("ctx") { ctx =>
          // ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
          // ctx.scriptContextTxInfo.txInfo
          val txInfoArgs: Expr[List[Data]] =
            sndPair(unConstrData(headList(sndPair(unConstrData(ctx)))))
          val txInfoSignatories: Expr[List[Data]] = unListData(
            headList(
              tailList(tailList(tailList(tailList(tailList(tailList(tailList(txInfoArgs)))))))
            )
          )

          val search = rec[List[Data], Unit] { self =>
            lam[List[Data]]("signatories") { signatories =>
              !(!ifThenElse(
                nullList(signatories),
                error,
                ~ifThenElse(
                  equalsByteString(
                    // signatories.head.pubKeyHash
                    unBData(headList(sndPair(unConstrData(headList(signatories)))))
                  )(
                    const(pkh.hash)
                  ),
                  ~const(()),
                  ~self(tailList(signatories))
                )
              ))
            }
          }
          search(txInfoSignatories)
        }
      }
    }
```

## What currently works?

WARNING. It's not safe for production use! Yet.
This project seeks funding to make it production ready.

Please, support my Catalyst Fund9 proposal: https://cardano.ideascale.com/c/idea/416933/ by voting!

What you can play with:
- CEK UPLC evaluation machine works on both JVM and JavaScript
- UPLC parser and pretty printer works on both JVM and JavaScript
- CBOR serialization of Data works on both JVM and JavaScript
- Type safe UPLC expression builder prototype works on both JVM and JavaScript
- There are a couple of simple validators that can be used for real.
- The PubKey validator is 104 bytes long! 

