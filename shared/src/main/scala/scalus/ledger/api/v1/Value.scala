package scalus.ledger.api.v1

import scalus.Compile
import scalus.prelude.AssocMap
import scalus.builtins.ByteString
import scalus.builtins.Builtins
import scalus.prelude.Prelude.given
import scalus.prelude.List
import scalus.prelude.These
import scalus.prelude.These.*
import scalus.prelude

@Compile
object Value:
  val zero: Value = AssocMap.empty
  def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value =
    AssocMap.singleton(cs, AssocMap.singleton(tn, v))
  def lovelace(v: BigInt): Value =
    AssocMap.singleton(ByteString.empty, AssocMap.singleton(ByteString.empty, v))

  def equalsAssets(
      a: AssocMap[TokenName, BigInt],
      b: AssocMap[TokenName, BigInt]
  ): Boolean = checkBinRel(Builtins.equalsInteger)(a, b)

  def checkBinRel(op: (BigInt, BigInt) => Boolean)(
      a: AssocMap[TokenName, BigInt],
      b: AssocMap[TokenName, BigInt]
  ): Boolean = {
    val combined = AssocMap.toList(AssocMap.union(a, b))
    // all values are equal, absent values are 0
    List.all(combined) { case (k, v) =>
      v match
        case These.These(v1, v2) => op(v1, v2)
        case This(v1)            => op(v1, 0)
        case That(v2)            => op(v2, 0)
    }
  }

  def unionVal(
      l: Value,
      r: Value
  ): AssocMap[CurrencySymbol, AssocMap[TokenName, These[BigInt, BigInt]]] =
    val combined: AssocMap[
      CurrencySymbol,
      prelude.These[AssocMap[TokenName, BigInt], AssocMap[TokenName, BigInt]]
    ] = AssocMap.union(l, r)
    AssocMap.map(combined) { case (cs, these) =>
      these match
        case These.These(v1, v2) => (cs, AssocMap.union(v1, v2))
        case This(v1)            => (cs, AssocMap.map(v1) { case (k, v) => (k, These.This(v)) })
        case That(v2)            => (cs, AssocMap.map(v2) { case (k, v) => (k, These.That(v)) })
    }

  def unionWith(op: (BigInt, BigInt) => BigInt)(a: Value, b: Value): Value =
    val combined = unionVal(a, b)
    val unThese: These[BigInt, BigInt] => BigInt = (k) =>
      k match
        case These.These(v1, v2) => op(v1, v2)
        case This(v1)            => op(v1, 0)
        case That(v2)            => op(0, v2)
    AssocMap.map(combined) { case (cs, v) =>
      (cs, AssocMap.map(v) { case (tn, v) => (tn, unThese(v)) })
    }

  val plus: (a: Value, b: Value) => Value = unionWith(Builtins.addInteger)
  val minus: (a: Value, b: Value) => Value = unionWith(Builtins.subtractInteger)
  val multiply: (a: Value, b: Value) => Value = unionWith(Builtins.multiplyInteger)
  val divide: (a: Value, b: Value) => Value = unionWith(Builtins.divideInteger)
