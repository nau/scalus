package scalus.ledger.api.v1

import scalus.Compile
import scalus.Ignore
import scalus.prelude.AssocMap
import scalus.builtin.ByteString
import scalus.builtin.Builtins.*
import scalus.prelude.List
import scalus.prelude.These
import scalus.prelude.These.*
import scalus.prelude
import scalus.prelude.Eq
import scalus.prelude.given

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
    ): Boolean = checkBinRelTokens(equalsInteger)(a, b)

    def eq(a: Value, b: Value): Boolean = checkBinRel(equalsInteger)(a, b)
    def lt(a: Value, b: Value): Boolean = checkBinRel(lessThanInteger)(a, b)
    def lte(a: Value, b: Value): Boolean = checkBinRel(lessThanEqualsInteger)(a, b)
    def gt(a: Value, b: Value): Boolean = checkBinRel(lessThanInteger)(b, a)
    def gte(a: Value, b: Value): Boolean = checkBinRel(lessThanEqualsInteger)(b, a)

    def checkPred(l: Value, r: Value)(f: These[BigInt, BigInt] => Boolean): Boolean = {
        def inner(m: AssocMap[TokenName, These[BigInt, BigInt]]): Boolean =
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
        a: AssocMap[TokenName, BigInt],
        b: AssocMap[TokenName, BigInt]
    ): Boolean = {
        val combined = AssocMap.union(a, b).toList
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
    ): AssocMap[CurrencySymbol, AssocMap[TokenName, These[BigInt, BigInt]]] =
        val combined: AssocMap[
          CurrencySymbol,
          prelude.These[AssocMap[TokenName, BigInt], AssocMap[TokenName, BigInt]]
        ] = AssocMap.union(l, r)
        combined.mapValues {
            case These.These(v1, v2) => AssocMap.union(v1, v2)
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
                if cs == ByteString.empty then tokens.toList.head._2
                else 0
