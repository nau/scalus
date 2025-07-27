package scalus.ledger.api.v1

import scalus.Compile
import scalus.Ignore
import scalus.prelude.SortedMap
import scalus.builtin.ByteString
import scalus.builtin.Builtins.*
import scalus.prelude.List
import scalus.prelude.Option
import scalus.prelude.These
import scalus.prelude.These.*
import scalus.prelude
import scalus.prelude.Eq
import scalus.prelude.Ord
import scalus.prelude.Ord.{<=>, Order}
import scalus.prelude.{!==, given}
import scala.annotation.tailrec

@Compile
object Value:
    /** A value representing zero units of any currency or token.
      *
      * This is represented as an empty `SortedMap`, meaning it contains no currency symbols or
      * tokens.
      *
      * @example
      *   {{{
      *   Value.zero === SortedMap.empty[CurrencySymbol, SortedMap[TokenName, BigInt]]
      *   Value.zero.isZero === true
      *   Value.zero.getLovelace === BigInt(0)
      *   }}}
      */
    val zero: Value = SortedMap.empty

    /** Creates a `Value` containing a single token amount for a given currency symbol and token
      * name.
      *
      * @param cs
      *   The currency symbol of the token
      * @param tn
      *   The token name
      * @param v
      *   The amount of tokens
      * @return
      *   A new `Value` containing only the specified currency symbol, token and its amount
      * @example
      *   {{{
      *   Value(adaCurrencySymbol, adaTokenName, BigInt(1000000)) === Value.lovelace(BigInt(1000000))
      *
      *   val policyId: CurrencySymbol = ByteString.fromString("currencySymbol")
      *   val tokenName: TokenName = ByteString.fromString("tokenName")
      *   Value(policyId, tokenName, BigInt(100)) === SortedMap.singleton(policyId, SortedMap.singleton(tokenName, BigInt(100)))
      *   }}}
      */
    def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value =
        if v !== BigInt(0) then SortedMap.singleton(cs, SortedMap.singleton(tn, v)) else zero

    /** Creates a `Value` containing only the specified amount of Lovelace (ADA).
      *
      * Uses the empty ByteString for both currency symbol and token name, which represents ADA.
      *
      * @param v
      *   The amount of Lovelace
      * @return
      *   A new `Value` containing only the specified amount of Lovelace
      * @example
      *   {{{
      *   Value.lovelace(BigInt(0)) === Value.zero
      *   Value.lovelace(BigInt(1000000)) === Value(adaCurrencySymbol, adaTokenName, BigInt(1000000))
      *   Value.lovelace(BigInt(2000000)).getLovelace === BigInt(2000000)
      *   }}}
      */
    def lovelace(v: BigInt): Value = apply(adaCurrencySymbol, adaTokenName, v)

    def unsafeFromList(
        list: List[(CurrencySymbol, List[(TokenName, BigInt)])]
    ): Value =
        SortedMap.unsafeFromList(
          list.map { pair => (pair._1, SortedMap.unsafeFromList(pair._2)) }
        )

    def fromList(
        list: List[(CurrencySymbol, List[(TokenName, BigInt)])]
    ): Value =
        SortedMap.fromList(
          list.filterMap { pair =>
              val tokens = pair._2.filterMap { case (tn, v) =>
                  if v !== BigInt(0) then Option.Some((tn, v)) else Option.None
              }

              if tokens.nonEmpty then Option.Some((pair._1, SortedMap.fromList(tokens)))
              else Option.None
          }
        )

    def fromStrictlyAscendingListWithNonZeroAmounts(
        list: List[(CurrencySymbol, List[(TokenName, BigInt)])]
    ): Value = {
        SortedMap.fromStrictlyAscendingList(
          list.map { case (currencySymbol, tokens) =>
              scalus.prelude.require(
                tokens.nonEmpty && tokens.forall { case (_, v) => v !== BigInt(0) },
                "Token amounts must be non-zero and token lists must not be empty"
              )

              (currencySymbol, SortedMap.fromStrictlyAscendingList(tokens))
          }
        )
    }

    val adaCurrencySymbol: CurrencySymbol = ByteString.empty
    val adaTokenName: TokenName = ByteString.empty

    def equalsAssets(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = checkBinRelTokens(equalsInteger)(a, b)

    def eq(a: Value, b: Value): Boolean = checkBinRel(equalsInteger)(a, b)
    def nonEq(a: Value, b: Value): Boolean = !checkBinRel(equalsInteger)(a, b)

    def negate(v: Value): Value = v.mapValues { _.mapValues { subtractInteger(0, _) } }
    val plus: (a: Value, b: Value) => Value = unionWith(addInteger)
    val minus: (a: Value, b: Value) => Value = unionWith(subtractInteger)
    val multiply: (a: Value, b: Value) => Value = unionWith(multiplyInteger)
    val divide: (a: Value, b: Value) => Value = unionWith(divideInteger)

    @Ignore
    def debugToString(v: Value): String = {
        val pairs = v.toList.asScala.map { case (cs, tokens) =>
            val tokenPairs = tokens.toList.asScala.map { case (tn, amount) =>
                s"#${tn.toHex}: $amount"
            }
            s"policy#${cs.toHex} -> { ${tokenPairs.mkString(", ")} }"
        }
        s"{ ${pairs.mkString(", ")} }"
    }

    given valueEq: Eq[Value] = (a, b) => eq(a, b)

    given valueOrd: Ord[Value] = (a, b) => {
        val values = SortedMap.union(a, b).toList.flatMap { case (cs, tokens) =>
            tokens match
                case These.These(v1, v2) => SortedMap.union(v1, v2).toList.map { case (_, v) => v }
                case This(v1)            => v1.toList.map { case (_, v) => These.This(v) }
                case That(v2)            => v2.toList.map { case (_, v) => These.That(v) }
        }

        @tailrec
        def go(lst: List[These[BigInt, BigInt]]): Ord.Order = lst match
            case List.Nil => Order.Equal
            case List.Cons(h, t) =>
                h match
                    case These.These(v1, v2) =>
                        val cmp = v1 <=> v2
                        if cmp !== Order.Equal then cmp else go(t)
                    case This(v1) =>
                        val cmp = v1 <=> BigInt(0)
                        if cmp !== Order.Equal then cmp else go(t)
                    case That(v2) =>
                        val cmp = BigInt(0) <=> v2
                        if cmp !== Order.Equal then cmp else go(t)

        go(values)
    }

    extension (v: Value)
        inline def unary_- : Value = Value.negate(v)
        inline def +(other: Value): Value = Value.plus(v, other)
        inline def -(other: Value): Value = Value.minus(v, other)
        inline def *(other: Value): Value = Value.multiply(v, other)
        inline def /(other: Value): Value = Value.divide(v, other)
        @Ignore
        inline def showDebug: String = debugToString(v)

        def getLovelace: BigInt = quantityOf(adaCurrencySymbol, adaTokenName)

        inline def isZero: Boolean = v.isEmpty

        def quantityOf(
            cs: CurrencySymbol,
            tn: TokenName
        ): BigInt = v.get(cs) match
            case Option.Some(tokens) => tokens.get(tn).getOrElse(0)
            case Option.None         => 0

        def withoutLovelace: Value = v.delete(adaCurrencySymbol)

        def flatten: List[(CurrencySymbol, TokenName, BigInt)] =
            v.foldRight(List.empty) { case (pair1, acc1) =>
                pair1._2.foldRight(acc1) { case (pair2, acc2) =>
                    List.Cons((pair1._1, pair2._1, pair2._2), acc2)
                }
            }

    private def checkPred(l: Value, r: Value)(f: These[BigInt, BigInt] => Boolean): Boolean = {
        def inner(m: SortedMap[TokenName, These[BigInt, BigInt]]): Boolean =
            m.forall((_, v) => f(v))
        unionVal(l, r).forall((_, v) => inner(v))
    }

    private def checkBinRel(op: (BigInt, BigInt) => Boolean)(
        a: Value,
        b: Value
    ): Boolean = {
        // all values are equal, absent values are 0
        checkPred(a, b) {
            case These.These(v1, v2) => op(v1, v2)
            case This(v1)            => op(v1, 0)
            case That(v2)            => op(0, v2)
        }
    }

    private def checkBinRelTokens(op: (BigInt, BigInt) => Boolean)(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = {
        val combined = SortedMap.union(a, b).toList
        // all values are equal, absent values are 0
        combined.forall { case (k, v) =>
            v match
                case These.These(v1, v2) => op(v1, v2)
                case This(v1)            => op(v1, 0)
                case That(v2)            => op(0, v2)
        }
    }

    private def unionVal(
        l: Value,
        r: Value
    ): SortedMap[CurrencySymbol, SortedMap[TokenName, These[BigInt, BigInt]]] =
        val combined: SortedMap[
          CurrencySymbol,
          prelude.These[SortedMap[TokenName, BigInt], SortedMap[TokenName, BigInt]]
        ] = SortedMap.union(l, r)
        combined.mapValues {
            case These.These(v1, v2) => SortedMap.union(v1, v2)
            case This(v1)            => v1.mapValues { These.This(_) }
            case That(v2)            => v2.mapValues { These.That(_) }
        }

    private def unionWith(op: (BigInt, BigInt) => BigInt)(a: Value, b: Value): Value =
        val combined = unionVal(a, b)
        val unThese: These[BigInt, BigInt] => BigInt = {
            case These.These(v1, v2) => op(v1, v2)
            case This(v1)            => op(v1, 0)
            case That(v2)            => op(0, v2)
        }

        SortedMap.unsafeFromList(
          combined.toList.filterMap { pair =>
              val tokens = pair._2.toList.filterMap { case (tn, v) =>
                  val value = unThese(v)
                  if value !== BigInt(0) then Option.Some((tn, value))
                  else Option.None
              }

              if tokens.nonEmpty then Option.Some((pair._1, SortedMap.unsafeFromList(tokens)))
              else Option.None
          }
        )
