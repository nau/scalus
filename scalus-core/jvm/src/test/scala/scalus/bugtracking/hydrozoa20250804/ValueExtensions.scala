package scalus.bugtracking.hydrozoa20250804

import scalus.Compile
import scalus.ledger.api.v1.Value.{-, zero}
import scalus.ledger.api.v3.{CurrencySymbol, TokenName, Value}
import scalus.prelude.List.Cons
import scalus.prelude.{fail, require, List, given}

@Compile
object ValueExtensions:
    extension (self: Value)

        def containsCurrencySymbol(cs: CurrencySymbol): Boolean =
            // Split away ada which always comes first
            val List.Cons(_, tokens) = self.toSortedMap.toList: @unchecked
            tokens.map(_._1).contains(cs)

        // Is it useful? get(cs).getOrElse(AssocMap.empty) does the same job
        //        def tokensUnder(cs: CurrencySymbol): AssocMap[TokenName, BigInt] =
        //            // Split away ada which always comes first
        //            val List.Cons(_, tokens) = self.toList: @unchecked
        //            tokens.find(_._1 == cs).map(_._2).getOrElse(AssocMap.empty)

        /** Check - contains only specified amount of same tokens and no other tokens
          * @param cs
          * @param tn
          * @param amount
          * @return
          */
        def containsExactlyOneAsset(
            cs: CurrencySymbol,
            tn: TokenName,
            amount: BigInt
        ): Boolean =
            // Split away ada which always comes first
            val List.Cons(_, tokens) = self.toSortedMap.toList: @unchecked
            tokens match
                case List.Cons((cs_, names), otherSymbols) =>
                    if otherSymbols.isEmpty then
                        if cs_ == cs then
                            names.toList match
                                case List.Cons((tn_, amount_), otherNames) =>
                                    otherNames.isEmpty && tn_ == tn && amount_ == amount
                                case _ => false
                        else false
                    else false
                case _ => false

        /** Returns the only non-ada asset, i.e. a unique token in the value or fails.
          * @return
          */
        def onlyNonAdaAsset: (CurrencySymbol, TokenName, BigInt) =
            // Split away ada which always comes first
            val List.Cons(_, tokens) = self.toSortedMap.toList: @unchecked

            tokens match
                case List.Cons((cs, names), otherSymbols) =>
                    require(
                      otherSymbols.isEmpty,
                      "onlyNonAdaToken: found more than one currency symbol"
                    )
                    names.toList match
                        case List.Cons((tokenName, amount), otherNames) =>
                            require(
                              otherNames.isEmpty,
                              "onlyNonAdaToken: found more than one token name"
                            )
                            (cs, tokenName, amount)
                        // TODO: is it reachable? can the inner Map[TokenName, BigInt] be empty?
                        case List.Nil => fail("onlyNonAdaToken: malformed value")
                case List.Nil => fail("onlyNonAdaToken: no non-ada assets in value")

        // Negate value, useful for burning operations
        def unary_- : Value = Value.zero - self
