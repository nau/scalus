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

    /** Creates a `Value` containing the specified amount of a specific currency and token. If the
      * amount is zero, it returns `Value.zero`.
      *
      * @param cs
      *   The currency symbol
      * @param tn
      *   The token name
      * @param v
      *   The amount of the token
      * @return
      *   A new `Value` containing the specified amount of the token, or `Value.zero` if the amount
      *   is zero
      * @example
      *   {{{
      *   Value(Value.adaCurrencySymbol, Value.adaTokenName, BigInt(1000000)) === Value.lovelace(BigInt(1000000))
      *
      *   val policyId: CurrencySymbol = ByteString.fromString("currencySymbol")
      *   val tokenName: TokenName = ByteString.fromString("tokenName")
      *   Value(policyId, tokenName, BigInt(100)) === SortedMap.singleton(policyId, SortedMap.singleton(tokenName, BigInt(100)))
      *
      *   Value(policyId, tokenName, BigInt(0)) === Value.zero
      *   }}}
      */
    def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value =
        if v !== BigInt(0) then SortedMap.singleton(cs, SortedMap.singleton(tn, v)) else zero

    /** Creates a `Value` representing a specific amount of ADA in lovelace.
      *
      * This is a convenience method for creating a `Value` with the ADA currency symbol and token
      * name, where the amount is specified in lovelace (1 ADA = 1,000,000 lovelace).
      *
      * @param v
      *   The amount of Lovelace
      * @return
      *   A new `Value` containing only the specified amount of Lovelace
      * @example
      *   {{{
      *   Value.lovelace(BigInt(1000000)) === SortedMap.singleton(Value.adaCurrencySymbol, SortedMap.singleton(Value.adaTokenName, BigInt(1000000)))
      *   Value.lovelace(BigInt(1000000)) === Value(Value.adaCurrencySymbol, Value.adaTokenName, BigInt(1000000))
      *   Value.lovelace(BigInt(1000000)).getLovelace === BigInt(1000000)
      *   Value.lovelace(BigInt(0)) === Value.zero
      *   }}}
      */
    def lovelace(v: BigInt): Value = apply(adaCurrencySymbol, adaTokenName, v)

    /** Creates a `Value` from a list of currency symbols paired with their token amounts, without
      * validation.
      *
      * This method directly constructs a `Value` from the input list without checking for zero
      * amounts or empty token lists. Use with caution as it may create invalid states.
      *
      * @param list
      *   A list of tuples containing currency symbols and their associated token amounts
      * @return
      *   A `Value` constructed directly from the input list
      * @example
      *   {{{
      *   val tokens = List.Cons(
      *     (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   Value.unsafeFromList(tokens)
      *   }}}
      * @see
      *   [[fromList]] for a safe version that validates the input
      */
    def unsafeFromList(
        list: List[(CurrencySymbol, List[(TokenName, BigInt)])]
    ): Value =
        SortedMap.unsafeFromList(
          list.map { pair => (pair._1, SortedMap.unsafeFromList(pair._2)) }
        )

    /** Creates a `Value` from a list of currency symbols paired with their token amounts, filtering
      * out zero amounts.
      *
      * This method safely constructs a `Value` by:
      *   - Removing all tokens with zero amounts
      *   - Removing currency symbols that have no remaining tokens after filtering
      *   - Converting the filtered lists to `SortedMap`s
      *
      * @param list
      *   A list of tuples containing currency symbols and their associated token amounts
      * @return
      *   A `Value` with zero amounts filtered out
      * @example
      *   {{{
      *   val tokens = List.Cons(
      *     (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(0)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   // The second entry will be filtered out due to zero amount
      *   Value.fromList(tokens) === Value.lovelace(BigInt(1000000))
      *   }}}
      * @see
      *   [[unsafeFromList]] for an unfiltered version
      */
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

    /** Creates a `Value` from a strictly ascending list of currency symbols and token amounts,
      * requiring non-zero amounts.
      *
      * This method enforces stricter requirements than [[fromList]]:
      *   - The input list must be strictly ascending by currency symbol
      *   - Each token list must be strictly ascending by token name
      *   - All token amounts must be non-zero
      *   - Token lists cannot be empty
      *
      * @param list
      *   A strictly ascending list of tuples containing currency symbols and their associated token
      *   amounts
      * @return
      *   A `Value` constructed from the strictly ascending lists
      * @throws scalus.cardano.onchain.RequirementError
      *   If any token amount is zero or any token list is empty
      * @example
      *   {{{
      *   // Successful case - ascending order and non-zero amounts
      *   val validTokens = List.Cons(
      *     (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *   Value.fromStrictlyAscendingListWithNonZeroAmounts(validTokens) // Succeeds
      *
      *   // Error case - contains zero amount
      *   val invalidTokens = List.Cons(
      *     (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(0)), List.Nil)),
      *     List.Nil
      *   )
      *   Value.fromStrictlyAscendingListWithNonZeroAmounts(invalidTokens) // Throws RequirementError
      *   }}}
      * @see
      *   [[fromList]] for a more permissive version that filters invalid entries
      */
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

    /** The currency symbol for ADA, represented as an empty `ByteString`.
      *
      * @example
      *   {{{
      *   Value.adaCurrencySymbol === ByteString.empty
      *   }}}
      */
    val adaCurrencySymbol: CurrencySymbol = ByteString.empty

    /** The token name for ADA, represented as an empty `ByteString`.
      *
      * @example
      *   {{{
      *   Value.adaTokenName === ByteString.empty
      *   }}}
      */
    val adaTokenName: TokenName = ByteString.empty

    /** Tests if two token asset maps contain exactly the same tokens with equal amounts.
      *
      * Compares two maps of token names to amounts, treating absent tokens as having zero amount.
      * The comparison checks if each token has equal amounts in both maps.
      *
      * @param a
      *   First map of token names to amounts
      * @param b
      *   Second map of token names to amounts
      * @return
      *   `true` if both maps contain the same tokens with equal amounts, `false` otherwise
      * @example
      *   {{{
      *   val assets1 = SortedMap.fromList(
      *     List.Cons(
      *       (ByteString.fromString("TOKEN1"), BigInt(100)),
      *       List.Cons(
      *         (ByteString.fromString("TOKEN2"), BigInt(200)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   val assets2 = SortedMap.fromList(
      *     List.Cons(
      *       (ByteString.fromString("TOKEN1"), BigInt(100)),
      *       List.Cons(
      *         (ByteString.fromString("TOKEN2"), BigInt(200)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.equalsAssets(assets1, assets2) === true
      *   }}}
      */
    def equalsAssets(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = checkBinRelTokens(equalsInteger)(a, b)

    /** Tests if two `Value` instances are equal.
      *
      * Compares two `Value` instances by checking if they contain the same currency symbols and
      * tokens with equal amounts, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   `true` if both `Value` instances are equal, `false` otherwise
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(1000000))
      *   Value.eq(value1, value2) === true
      *
      *   val value3 = Value.lovelace(BigInt(500000))
      *   Value.eq(value1, value3) === false
      *   }}}
      */
    def eq(a: Value, b: Value): Boolean = checkBinRel(equalsInteger)(a, b)

    /** Tests if two `Value` instances are not equal.
      *
      * Compares two `Value` instances by checking if they contain different currency symbols or
      * tokens with unequal amounts, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   `true` if the `Value` instances are not equal, `false` otherwise
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(1000000))
      *   Value.nonEq(value1, value2) === false
      *
      *   val value3 = Value.lovelace(BigInt(500000))
      *   Value.nonEq(value1, value3) === true
      *   }}}
      */
    def nonEq(a: Value, b: Value): Boolean = !checkBinRel(equalsInteger)(a, b)

    /** Negates all token amounts in a `Value`, effectively creating the additive inverse.
      *
      * This method traverses the `Value` and negates each token amount by subtracting it from zero.
      *
      * @param v
      *   The `Value` to negate
      * @return
      *   A new `Value` with all token amounts negated
      * @example
      *   {{{
      *   val value1 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   val value2 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(-1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(-100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.negate(value1) === value2
      *   }}}
      */
    def negate(v: Value): Value = v.mapValues { _.mapValues { subtractInteger(0, _) } }

    /** Adds two `Value` instances together, combining their token amounts.
      *
      * This method performs an element-wise addition of the token amounts in both `Value`
      * instances, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   A new `Value` containing the combined token amounts
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.plus(value1, value2) === Value.lovelace(BigInt(1500000))
      *   }}}
      */
    val plus: (a: Value, b: Value) => Value = unionWith(addInteger)

    /** Subtracts the second `Value` from the first, effectively performing element-wise subtraction
      * of token amounts.
      *
      * This method subtracts the token amounts in the second `Value` from those in the first,
      * treating absent tokens as having zero amount.
      *
      * @param a
      *   The `Value` to subtract from
      * @param b
      *   The `Value` to subtract
      * @return
      *   A new `Value` containing the result of the subtraction
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.minus(value1, value2) === Value.lovelace(BigInt(500000))
      *   }}}
      */
    val minus: (a: Value, b: Value) => Value = unionWith(subtractInteger)

    /** Multiplies two `Value` instances together, combining their token amounts.
      *
      * This method performs an element-wise multiplication of the token amounts in both `Value`
      * instances, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   A new `Value` containing the combined token amounts after multiplication
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.multiply(value1, value2) === Value.lovelace(BigInt(500000000000))
      *   }}}
      */
    val multiply: (a: Value, b: Value) => Value = unionWith(multiplyInteger)

    /** Divides the first `Value` by the second, effectively performing element-wise division of
      * token amounts.
      *
      * This method divides the token amounts in the first `Value` by those in the second, treating
      * absent tokens as having zero amount. If a token amount in the second `Value` is zero, it
      * will result in an error.
      *
      * @param a
      *   The `Value` to divide
      * @param b
      *   The `Value` to divide by
      * @return
      *   A new `Value` containing the result of the division
      * @throws ArithmeticException:
      *   / by zero If any token amount in the second `Value` is zero, as division by zero is not
      *   allowed
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.divide(value1, value2) === Value.lovelace(BigInt(2))
      *
      *   Value.divide(value1, Value.zero) // Throws ArithmeticException: / by zero
      *   }}}
      */
    val divide: (a: Value, b: Value) => Value = unionWith(divideInteger)

    /** Converts a `Value` to a debug string representation.
      *
      * Formats the `Value` as a string showing policy IDs and token amounts in a human-readable
      * format. Each currency symbol (policy ID) and its associated tokens are displayed with their
      * hex representations and amounts.
      *
      * @param v
      *   The `Value` to convert to string
      * @return
      *   A formatted string representation of the `Value`
      * @example
      *   {{{
      *   val value = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   // Prints: { policy# -> { #: 1000000 }, policy#ff -> { #544f4b454e: 100 } }
      *   println(Value.debugToString(value))
      *   }}}
      */
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

    /** Implementation of the [[prelude.Eq]] type class for `Value`.
      *
      * Provides equality comparison between two `Value` instances by delegating to [[Value.eq]].
      * Two `Value` instances are considered equal if they contain exactly the same currency symbols
      * and tokens with equal amounts.
      *
      * @example
      *   {{{
      *   val value1 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   val value2 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   value1 === value2 // true, using derived Eq instance
      *   }}}
      */
    given valueEq: Eq[Value] = (a, b) => eq(a, b)

    /** Implementation of the [[prelude.Ord]] type class for `Value`.
      *
      * Compares two `Value` instances by lexicographically comparing their token amounts. Each
      * token amount is compared as follows:
      *   - For tokens present in both values, compares their amounts directly
      *   - For tokens present in only one value, compares that amount against zero
      *
      * @example
      *   {{{
      *   val value1 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   val value2 = Value.fromList(
      *     List.Cons(
      *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(2000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   value1 <=> value2 // returns Order.Less
      *   }}}
      */
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
        /** Extension alias for [[Value.negate]]. */
        inline def unary_- : Value = Value.negate(v)

        /** Extension alias for [[Value.plus]]. */
        inline def +(other: Value): Value = Value.plus(v, other)

        /** Extension alias for [[Value.minus]]. */
        inline def -(other: Value): Value = Value.minus(v, other)

        /** Extension alias for [[Value.multiply]]. */
        inline def *(other: Value): Value = Value.multiply(v, other)

        /** Extension alias for [[Value.divide]]. */
        inline def /(other: Value): Value = Value.divide(v, other)

        /** Extension alias for [[Value.debugToString]]. */
        @Ignore
        inline def showDebug: String = debugToString(v)

        /** Returns the amount of Lovelace in this `Value`.
          *
          * If the `Value` contains no Lovelace, it returns zero.
          *
          * @return
          *   The amount of Lovelace in this `Value`
          * @example
          *   {{{
          *   val value = Value.lovelace(BigInt(1000000))
          *   value.getLovelace === BigInt(1000000)
          *
          *   val emptyValue = Value.zero
          *   emptyValue.getLovelace === BigInt(0)
          *   }}}
          */
        def getLovelace: BigInt = quantityOf(adaCurrencySymbol, adaTokenName)

        /** Checks if this `Value` is zero, meaning it contains no tokens or currency symbols.
          *
          * @return
          *   `true` if the `Value` is empty, `false` otherwise
          * @example
          *   {{{
          *   val value = Value.zero
          *   value.isZero === true
          *
          *   val nonZeroValue = Value.lovelace(BigInt(1000000))
          *   nonZeroValue.isZero === false
          *   }}}
          */
        inline def isZero: Boolean = v.isEmpty

        /** Gets the amount of a specific token in a currency symbol from a `Value`.
          *
          * Returns the token amount for the given currency symbol and token name pair. If either
          * the currency symbol or token name is not found, returns zero.
          *
          * @param cs
          *   The currency symbol to look up
          * @param tn
          *   The token name to look up within that currency symbol
          * @return
          *   The amount of the specified token, or zero if not found
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.quantityOf(Value.adaCurrencySymbol, Value.adaTokenName) === BigInt(1000000)
          *   value.quantityOf(ByteString.fromString("ff"), ByteString.fromString("TOKEN")) === BigInt(100)
          *   value.quantityOf(ByteString.fromString("missing"), ByteString.fromString("TOKEN")) === BigInt(0)
          *   }}}
          */
        def quantityOf(
            cs: CurrencySymbol,
            tn: TokenName
        ): BigInt = v.get(cs) match
            case Option.Some(tokens) => tokens.get(tn).getOrElse(0)
            case Option.None         => 0

        /** Returns a new `Value` with all ADA/Lovelace tokens removed.
          *
          * This method creates a copy of the value with the ADA currency symbol removed,
          * effectively removing all Lovelace tokens while preserving other tokens.
          *
          * @return
          *   A new `Value` without any Lovelace tokens
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   val withoutAda = Value.fromList(
          *     List.Cons(
          *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *       List.Nil
          *     )
          *   )
          *
          *   value.withoutLovelace === withoutAda
          *   }}}
          */
        def withoutLovelace: Value = v.delete(adaCurrencySymbol)

        /** Flattens the `Value` into a list of currency symbol, token name, and amount triples.
          *
          * Converts the nested map structure into a flat list representation where each element
          * contains the currency symbol, token name, and corresponding amount.
          *
          * @return
          *   A flattened list of tuples containing (currencySymbol, tokenName, amount)
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaCurrencySymbol, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.flatten ===
          *   List.Cons(
          *      (Value.adaCurrencySymbol, Value.adaTokenName, BigInt(1000000)),
          *      List.Cons(
          *        (ByteString.fromString("ff"), ByteString.fromString("TOKEN"), BigInt(100)),
          *        List.Nil
          *      )
          *   )
          *   }}}
          */
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
