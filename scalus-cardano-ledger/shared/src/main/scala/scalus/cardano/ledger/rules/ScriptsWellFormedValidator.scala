package scalus.cardano.ledger
package rules

import scala.collection.View
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.ledger.api.MajorProtocolVersion
import scalus.uplc.{DeBruijnedProgram, DefaultFun, Term}

// It's Babbage.validateScriptsWellFormed in cardano-ledger
object ScriptsWellFormedValidator extends STS.Validator {
    override final type Error = TransactionException.IllFormedScriptsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxo
        val body = event.body.value
        val majorProtocolVersion = MajorProtocolVersion(context.env.params.protocolVersion.major)

        val allWitnessesPlutusScripts = AllResolvedScripts.allWitnessesPlutusScriptsView(event)

        val allOutputs = (body.outputs.view ++ body.collateralReturnOutput).map(_.value)
        val allPlutusScriptsFromAllOutputs =
            for
                output <- allOutputs
                scriptRef <- output.scriptRef
                plutusScript <- scriptRef.script match
                    case plutusScript: PlutusScript => Some(plutusScript)
                    case _                          => None
            yield plutusScript

        val allInvalidWitnessesPlutusScriptHashes = findAllInvalidPlutusScripts(
          allWitnessesPlutusScripts,
          majorProtocolVersion
        )

        val allInvalidPlutusScriptHashesFromAllOutputs = findAllInvalidPlutusScripts(
          allPlutusScriptsFromAllOutputs,
          majorProtocolVersion
        )

        if allInvalidWitnessesPlutusScriptHashes.nonEmpty ||
            allInvalidPlutusScriptHashesFromAllOutputs.nonEmpty
        then
            failure(
              TransactionException.IllFormedScriptsException(
                transactionId,
                allInvalidWitnessesPlutusScriptHashes,
                allInvalidPlutusScriptHashesFromAllOutputs
              )
            )
        else success
    }

    private def findAllInvalidPlutusScripts(
        plutusScripts: View[PlutusScript],
        majorProtocolVersion: MajorProtocolVersion
    ): Set[ScriptHash] = {
        plutusScripts
            .filterNot(isWellFormedPlutusScript(_, majorProtocolVersion))
            .map(_.scriptHash)
            .toSet
    }

    private def isWellFormedPlutusScript(
        plutusScript: PlutusScript,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        val language = plutusScript.language

        if majorProtocolVersion < language.majorProtocolVersion then return false

        val DeBruijnedProgram(_, term) = DeBruijnedProgram.fromCbor(plutusScript.script.bytes)

        val collectedBuiltins = collectBuiltins(term)
        val foundBuiltinsIntroducedIn = findBuiltinsIntroducedIn(language, majorProtocolVersion)

        collectedBuiltins.subsetOf(foundBuiltinsIntroducedIn)
    }

    private def collectBuiltins(term: Term): Set[DefaultFun] = {
        term match
            case Term.Builtin(bn)                         => Set(bn)
            case Term.Var(_) | Term.Const(_) | Term.Error => Set.empty
            case Term.LamAbs(_, body)                     => collectBuiltins(body)
            case Term.Force(body)                         => collectBuiltins(body)
            case Term.Delay(body)                         => collectBuiltins(body)
            case Term.Apply(f, arg) => collectBuiltins(f) ++ collectBuiltins(arg)
            case Term.Constr(_, args) =>
                args.foldLeft(Set.empty[DefaultFun])((acc, x) => acc ++ collectBuiltins(x))
            case Term.Case(arg, cases) =>
                cases.foldLeft(collectBuiltins(arg))((acc, x) => acc ++ collectBuiltins(x))
    }

    private def findBuiltinsIntroducedIn(
        language: Language,
        majorProtocolVersion: MajorProtocolVersion
    ): Set[DefaultFun] = {
        builtinsIntroducedIn.get(language) match
            case Some(majorProtocolVersionBuiltins) =>
                val floorMajorProtocolVersionBuiltins =
                    majorProtocolVersionBuiltins.view.takeWhile {
                        case (currentMajorProtocolVersion, _) =>
                            currentMajorProtocolVersion <= majorProtocolVersion
                    }.lastOption

                floorMajorProtocolVersionBuiltins.map(_._2).getOrElse(Set.empty)
            case None => Set.empty
    }

    private object BuiltinBatches {
        // {- Batches of builtins which were introduced in the same hard fork (but perhaps
        //   not for all LLs): see the Plutus Core specification and
        //   `builtinsIntroducedIn` below.
        // -}
        //
        // {- If any new builtins are introduced after a batch has been deployed on the chain
        //  then a new `batch` object MUST be added to contain them and the
        //  `builtinsIntroducedIn` function must be updated; the contents of batches which
        //  have already been deployed must NOT be altered.  Also, remember to UPDATE THE
        //  TESTS in `Spec.Versions` and `Spec.Data.Versions` when a new batch is added.
        // -}
        //
        // {- It's tempting to try something like `fmap toEnum [0..50]` here, but that's
        //   dangerous because the order of the constructors in DefaultFun doesn't
        //   precisely match the order that the builtins were introduced in.  A safer
        //   alternative would be to use the flat tags, but they're not directly
        //   accessible at the moment.
        // -}
        // -- DO NOT CHANGE THIS.
        val batch1: Set[DefaultFun] =
            Set(
              // Integers
              DefaultFun.AddInteger,
              DefaultFun.SubtractInteger,
              DefaultFun.MultiplyInteger,
              DefaultFun.DivideInteger,
              DefaultFun.QuotientInteger,
              DefaultFun.RemainderInteger,
              DefaultFun.ModInteger,
              DefaultFun.EqualsInteger,
              DefaultFun.LessThanInteger,
              DefaultFun.LessThanEqualsInteger,
              // Bytestrings
              DefaultFun.AppendByteString,
              DefaultFun.ConsByteString,
              DefaultFun.SliceByteString,
              DefaultFun.LengthOfByteString,
              DefaultFun.IndexByteString,
              DefaultFun.EqualsByteString,
              DefaultFun.LessThanByteString,
              DefaultFun.LessThanEqualsByteString,
              // Crypto & hashes
              DefaultFun.Sha2_256,
              DefaultFun.Sha3_256,
              DefaultFun.Blake2b_256,
              DefaultFun.VerifyEd25519Signature,
              // Strings
              DefaultFun.AppendString,
              DefaultFun.EqualsString,
              DefaultFun.EncodeUtf8,
              DefaultFun.DecodeUtf8,
              // Bool / control
              DefaultFun.IfThenElse,
              DefaultFun.ChooseUnit,
              DefaultFun.Trace,
              // Pairs
              DefaultFun.FstPair,
              DefaultFun.SndPair,
              // Lists
              DefaultFun.ChooseList,
              DefaultFun.MkCons,
              DefaultFun.HeadList,
              DefaultFun.TailList,
              DefaultFun.NullList,
              // Data
              DefaultFun.ChooseData,
              DefaultFun.ConstrData,
              DefaultFun.MapData,
              DefaultFun.ListData,
              DefaultFun.IData,
              DefaultFun.BData,
              DefaultFun.UnConstrData,
              DefaultFun.UnMapData,
              DefaultFun.UnListData,
              DefaultFun.UnIData,
              DefaultFun.UnBData,
              DefaultFun.EqualsData,
              // Misc
              DefaultFun.MkPairData,
              DefaultFun.MkNilData,
              DefaultFun.MkNilPairData
            )

        // DO NOT CHANGE THIS.
        val batch2: Set[DefaultFun] =
            Set(DefaultFun.SerialiseData)

        // DO NOT CHANGE THIS.
        val batch3: Set[DefaultFun] =
            Set(
              DefaultFun.VerifyEcdsaSecp256k1Signature,
              DefaultFun.VerifySchnorrSecp256k1Signature
            )

        // -- `cekCase` and `cekConstr` costs come between Batch 3 and Batch 4 in the
        // -- PlutusV3 cost model parameters, although that's irrelevant here.
        //
        // -- batch4, excluding IntegerToByteString and ByteStringToInteger.
        // -- DO NOT CHANGE THIS.
        val batch4a: Set[DefaultFun] =
            Set(
              // BLS12-381
              DefaultFun.Bls12_381_G1_add,
              DefaultFun.Bls12_381_G1_neg,
              DefaultFun.Bls12_381_G1_scalarMul,
              DefaultFun.Bls12_381_G1_equal,
              DefaultFun.Bls12_381_G1_hashToGroup,
              DefaultFun.Bls12_381_G1_compress,
              DefaultFun.Bls12_381_G1_uncompress,
              DefaultFun.Bls12_381_G2_add,
              DefaultFun.Bls12_381_G2_neg,
              DefaultFun.Bls12_381_G2_scalarMul,
              DefaultFun.Bls12_381_G2_equal,
              DefaultFun.Bls12_381_G2_hashToGroup,
              DefaultFun.Bls12_381_G2_compress,
              DefaultFun.Bls12_381_G2_uncompress,
              DefaultFun.Bls12_381_millerLoop,
              DefaultFun.Bls12_381_mulMlResult,
              DefaultFun.Bls12_381_finalVerify,
              // Hashes
              DefaultFun.Keccak_256,
              DefaultFun.Blake2b_224
            )

        // {- batch4b: IntegerToByteString and ByteStringToInteger.  These were enabled in
        // PlutusV3 at PV9, along with batch4a, They were enabled in PlutusV2 at PV10 in
        // #6056 and #6065.  They are available on the chain, but they're prohibitively
        // expensive because the proposal to update the relevant protocol parameters has
        // not (yet) been enacted.  This has left a "gap" in the cost model paramters: for
        // PlutusV3, the parameters for Batch 3 are followed those for 4a, then 4b, but
        // for PlutusV2 those for Batch3 are followed by those for Batch 4a, and those for
        // 4b aren't in use yet.  Since you can't actually use the 4b builtins in PlutusV2
        // at the moment, it's tempting to insert the 4a parameter before the 4b
        // parameters and enable them all at PV11 and with a suitable parameter update.
        // However, if we do do this there's a theoretical risk of turning a phase 2
        // failure into a phase 1 failure: would that be problematic?
        // -}
        // -- DO NOT CHANGE THIS.
        val batch4b: Set[DefaultFun] =
            Set(DefaultFun.IntegerToByteString, DefaultFun.ByteStringToInteger)

        // DO NOT CHANGE THIS.
        val batch4: Set[DefaultFun] = batch4a ++ batch4b

        // DO NOT CHANGE THIS.
        val batch5: Set[DefaultFun] =
            Set(
              DefaultFun.AndByteString,
              DefaultFun.OrByteString,
              DefaultFun.XorByteString,
              DefaultFun.ComplementByteString,
              DefaultFun.ReadBit,
              DefaultFun.WriteBits,
              DefaultFun.ReplicateByte,
              DefaultFun.ShiftByteString,
              DefaultFun.RotateByteString,
              DefaultFun.CountSetBits,
              DefaultFun.FindFirstSetBit,
              DefaultFun.Ripemd_160
            )

        val batch6: Set[DefaultFun] =
            Set(
//              DefaultFun.ExpModInteger,
//              DefaultFun.DropList,
//              DefaultFun.LengthOfArray,
//              DefaultFun.ListToArray,
//              DefaultFun.IndexArray
            )
    }

    private val builtinsIntroducedIn: Map[Language, Map[MajorProtocolVersion, Set[DefaultFun]]] =
        Map(
          Language.PlutusV1 -> Map(
            MajorProtocolVersion.alonzoPV -> BuiltinBatches.batch1,
            MajorProtocolVersion.futurePV -> (
              BuiltinBatches.batch2 ++ BuiltinBatches.batch3 ++
                  BuiltinBatches.batch4 ++ BuiltinBatches.batch5 ++
                  BuiltinBatches.batch6
            )
          ),
          Language.PlutusV2 -> Map(
            MajorProtocolVersion.vasilPV -> (BuiltinBatches.batch1 ++ BuiltinBatches.batch2),
            MajorProtocolVersion.valentinePV -> BuiltinBatches.batch3,
            MajorProtocolVersion.plominPV -> BuiltinBatches.batch4b,
            MajorProtocolVersion.futurePV -> (BuiltinBatches.batch4a ++ BuiltinBatches.batch5 ++ BuiltinBatches.batch6)
          ),
          Language.PlutusV3 -> Map(
            MajorProtocolVersion.changPV -> (
              BuiltinBatches.batch1 ++ BuiltinBatches.batch2 ++
                  BuiltinBatches.batch3 ++ BuiltinBatches.batch4
            ),
            MajorProtocolVersion.plominPV -> BuiltinBatches.batch5,
            MajorProtocolVersion.futurePV -> BuiltinBatches.batch6
          )
        )
}
