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
import scalus.prelude.{Eq, Ord}
import scalus.prelude.Ord.given

@Compile
object Value:
    val zero: Value = SortedMap.empty
    def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value =
        SortedMap.singleton(cs, SortedMap.singleton(tn, v))
    def lovelace(v: BigInt): Value =
        SortedMap.singleton(adaCurrencySymbol, SortedMap.singleton(adaTokenName, v))

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
          list.map { pair => (pair._1, SortedMap.fromList(pair._2)) }
        )

    def fromStrictlyAscendingList(
        list: List[(CurrencySymbol, List[(TokenName, BigInt)])]
    ): Value =
        SortedMap.fromStrictlyAscendingList(
          list.map { pair => (pair._1, SortedMap.fromStrictlyAscendingList(pair._2)) }
        )

    val adaCurrencySymbol: CurrencySymbol = ByteString.empty
    val adaTokenName: TokenName = ByteString.empty

    def equalsAssets(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = checkBinRelTokens(equalsInteger)(a, b)

    def eq(a: Value, b: Value): Boolean = checkBinRel(equalsInteger)(a, b)
    def lt(a: Value, b: Value): Boolean = checkBinRel(lessThanInteger)(a, b)
    def lte(a: Value, b: Value): Boolean = checkBinRel(lessThanEqualsInteger)(a, b)
    def gt(a: Value, b: Value): Boolean = checkBinRel(lessThanInteger)(b, a)
    def gte(a: Value, b: Value): Boolean = checkBinRel(lessThanEqualsInteger)(b, a)

    def checkPred(l: Value, r: Value)(f: These[BigInt, BigInt] => Boolean): Boolean = {
        def inner(m: SortedMap[TokenName, These[BigInt, BigInt]]): Boolean =
            m.forall((_, v) => f(v))
        unionVal(l, r).forall((_, v) => inner(v))
    }

    def checkBinRel(op: (BigInt, BigInt) => Boolean)(
        a: Value,
        b: Value
    ): Boolean = {
        // all values are equal, absent values are 0
        checkPred(a, b) {
            case These.These(v1, v2) => op(v1, v2)
            case This(v1)            => op(v1, 0)
            case That(v2)            => op(v2, 0)
        }
    }

    def checkBinRelTokens(op: (BigInt, BigInt) => Boolean)(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = {
        val combined = SortedMap.union(a, b).toList
        // all values are equal, absent values are 0
        combined.forall { case (k, v) =>
            v match
                case These.These(v1, v2) => op(v1, v2)
                case This(v1)            => op(v1, 0)
                case That(v2)            => op(v2, 0)
        }
    }

    def unionVal(
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

    def unionWith(op: (BigInt, BigInt) => BigInt)(a: Value, b: Value): Value =
        val combined = unionVal(a, b)
        val unThese: These[BigInt, BigInt] => BigInt = {
            case These.These(v1, v2) => op(v1, v2)
            case This(v1)            => op(v1, 0)
            case That(v2)            => op(0, v2)
        }
        combined.mapValues { _.mapValues { unThese(_) } }

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

    given Eq[Value] = (a, b) => eq(a, b)

    extension (v: Value)
        inline def unary_- : Value = Value.negate(v)
        inline def +(other: Value): Value = Value.plus(v, other)
        inline def -(other: Value): Value = Value.minus(v, other)
        inline def *(other: Value): Value = Value.multiply(v, other)
        inline def /(other: Value): Value = Value.divide(v, other)
        inline def <(other: Value): Boolean = Value.lt(v, other)
        inline def <=(other: Value): Boolean = Value.lte(v, other)
        inline def >(other: Value): Boolean = Value.gt(v, other)
        inline def >=(other: Value): Boolean = Value.gte(v, other)
        @Ignore
        inline def showDebug: String = debugToString(v)

        def getLovelace: BigInt = v.toList match
            case List.Nil                   => 0
            case List.Cons((cs, tokens), _) =>
                // Ada is always the first token. Only Ada can have empty CurrencySymbol. And its only token is Lovelace
                if cs == adaCurrencySymbol then
                    tokens.toList match {
                        case List.Nil => throw RuntimeException("No Lovelace token found in Value")
                        case List.Cons(head, tail) => head._2
                    }
                else 0

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
