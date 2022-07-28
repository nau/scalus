# Scalus - Scala implementation of Cardano Plutus

## What it is going to be?

Scalus is a set of libraries to work with Cardano Untyped Plutus Core that works on both JVM and JavaScript.
This includes:

- Untyped Plutus Core data types and functions
- Flat, CBOR, JSON serialization
- CEK UPLC evaluation machine including execution cost calculation
- UPLC parser and pretty printer

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
