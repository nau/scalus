package scalus.ledger.api.v1

import scalus.Compile
import scalus.prelude.AssocMap
import scalus.builtins.ByteString
import scalus.builtins.Builtins
import scalus.prelude.Prelude.given
import scalus.prelude.List
import scalus.prelude.These.*

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
        case These(v1, v2) => op(v1, v2)
        case This(v1)      => op(v1, 0)
        case That(v2)      => op(v2, 0)
    }
  }
