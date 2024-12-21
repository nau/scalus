package scalus.sir

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.BLS12_381_G2_Element
import scalus.builtin.BLS12_381_MlResult
import scalus.uplc.DefaultFun
import scalus.uplc.TypeScheme.Type

import scala.util.control.NonFatal

object SIRBuiltins {

    val addInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.AddInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val subtractInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.SubtractInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val multiplyInteger = SIR.Builtin(
      DefaultFun.MultiplyInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val divideInteger = SIR.Builtin(
      DefaultFun.DivideInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val quotientInteger = SIR.Builtin(
      DefaultFun.QuotientInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val remainderInteger = SIR.Builtin(
      DefaultFun.RemainderInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val modInteger = SIR.Builtin(
      DefaultFun.ModInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val equalsInteger = SIR.Builtin(
      DefaultFun.EqualsInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
    )
    val lessThanInteger = SIR.Builtin(
      DefaultFun.LessThanInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
    )
    val lessThanEqualsInteger = SIR.Builtin(
      DefaultFun.LessThanEqualsInteger,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
    )

    // Bytestrings
    val appendByteString = SIR.Builtin(
      DefaultFun.AppendByteString,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val consByteString = SIR.Builtin(
      DefaultFun.ConsByteString,
      SIRType.IntegerPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val sliceByteString = SIR.Builtin(
      DefaultFun.SliceByteString,
      SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val lengthOfByteString = SIR.Builtin(
      DefaultFun.LengthOfByteString,
      SIRType.ByteStringPrimitive ->: SIRType.IntegerPrimitive
    )
    val indexByteString = SIR.Builtin(
      DefaultFun.IndexByteString,
      SIRType.ByteStringPrimitive ->: SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
    )
    val equalsByteString = SIR.Builtin(
      DefaultFun.EqualsByteString,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )
    val lessThanByteString = SIR.Builtin(
      DefaultFun.LessThanByteString,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )
    val lessThanEqualsByteString = SIR.Builtin(
      DefaultFun.LessThanEqualsByteString,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )

    // Cryptography and hashes
    val sha2_256 = SIR.Builtin(
      DefaultFun.Sha2_256,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val sha3_256 = SIR.Builtin(
      DefaultFun.Sha3_256,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val blake2b_256 = SIR.Builtin(
      DefaultFun.Blake2b_256,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    val verifyEd25519Signature = SIR.Builtin(
      DefaultFun.VerifyEd25519Signature,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )
    val verifyEcdsaSecp256k1Signature = SIR.Builtin(
      DefaultFun.VerifyEcdsaSecp256k1Signature,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )
    val verifySchnorrSecp256k1Signature = SIR.Builtin(
      DefaultFun.VerifySchnorrSecp256k1Signature,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
    )

    // Strings
    val appendString = SIR.Builtin(
      DefaultFun.AppendString,
      SIRType.StringPrimitive ->: SIRType.StringPrimitive ->: SIRType.StringPrimitive
    )
    val equalsString = SIR.Builtin(
      DefaultFun.EqualsString,
      SIRType.StringPrimitive ->: SIRType.StringPrimitive ->: SIRType.BooleanPrimitive
    )
    val encodeUtf8 =
        SIR.Builtin(DefaultFun.EncodeUtf8, SIRType.StringPrimitive ->: SIRType.ByteStringPrimitive)
    val decodeUtf8 =
        SIR.Builtin(DefaultFun.DecodeUtf8, SIRType.ByteStringPrimitive ->: SIRType.StringPrimitive)

    // Bool
    val ifThenElse = SIR.Builtin(
      DefaultFun.IfThenElse, {
          val a = SIRType.TypeVar("A", Some("ifThenElse_A".hashCode))
          a :=>> (SIRType.BooleanPrimitive ->: a ->: a ->: a)
      }
    )

    // Unit
    val chooseUnit =
        SIR.Builtin(
          DefaultFun.ChooseUnit, {
              val a = SIRType.TypeVar("A", Some("chooseUnit_A".hashCode))
              a :=>> (SIRType.VoidPrimitive ->: a ->: a)
          }
        )

    // Tracing
    //   TODO: move to SIR construction
    val trace = SIR.Builtin(
      DefaultFun.Trace,
      SIRType.TypeLambda("trace_A", a => SIRType.StringPrimitive ->: a ->: a)
    )

    // Pairs
    val fstPair = SIR.Builtin(
      DefaultFun.FstPair,
      SIRType.TypeLambda2("fstPair_A", "fstPair_B", (a, b) => SIRType.Pair(a, b) ->: a)
    )
    val sndPair = SIR.Builtin(
      DefaultFun.SndPair,
      SIRType.TypeLambda2("sndPair_A", "sndPair_B", (a, b) => SIRType.Pair(a, b) ->: b)
    )

    // Lists
    val chooseList = SIR.Builtin(
      DefaultFun.ChooseList,
      SIRType.TypeLambda2(
        "chooseList_A",
        "chooseList_B",
        (a, b) => SIRType.List(a) ->: b ->: b ->: b
      )
    )
    val mkCons = SIR.Builtin(
      DefaultFun.MkCons,
      SIRType.TypeLambda("mkCons_A", a => a ->: SIRType.List(a) ->: SIRType.List(a))
    )
    val headList =
        SIR.Builtin(
          DefaultFun.HeadList,
          SIRType.TypeLambda("headList_A", a => SIRType.List(a) ->: a)
        )
    val tailList = SIR.Builtin(
      DefaultFun.TailList,
      SIRType.TypeLambda("tailList_A", a => SIRType.List(a) ->: SIRType.List(a))
    )
    val nullList = SIR.Builtin(
      DefaultFun.NullList,
      SIRType.TypeLambda("nullList_A", a => SIRType.List(a) ->: SIRType.BooleanPrimitive)
    )

    // Data
    val chooseData =
        SIR.Builtin(
          DefaultFun.ChooseData,
          SIRType.TypeLambda("chooseData_A", a => SIRType.Data ->: a ->: a ->: a ->: a ->: a ->: a)
        )
    val constrData = SIR.Builtin(
      DefaultFun.ConstrData,
      SIRType.IntegerPrimitive ->: SIRType.List(SIRType.Data) ->: SIRType.Data
    )
    val mapData = SIR.Builtin(
      DefaultFun.MapData,
      SIRType.List(SIRType.Pair(SIRType.Data, SIRType.Data)) ->: SIRType.Data
    )
    val listData =
        SIR.Builtin(DefaultFun.ListData, SIRType.List(SIRType.Data) ->: SIRType.Data)
    val iData = SIR.Builtin(DefaultFun.IData, SIRType.IntegerPrimitive ->: SIRType.Data)
    val bData = SIR.Builtin(DefaultFun.BData, SIRType.BooleanPrimitive ->: SIRType.Data)
    val unConstrData = SIR.Builtin(
      DefaultFun.UnConstrData,
      SIRType.Data ->: SIRType.Pair(SIRType.IntegerPrimitive, SIRType.List(SIRType.Data))
    )
    val unMapData = SIR.Builtin(
      DefaultFun.UnMapData,
      SIRType.Data ->: SIRType.List(SIRType.Pair(SIRType.Data, SIRType.Data))
    )
    val unListData =
        SIR.Builtin(DefaultFun.UnListData, SIRType.Data ->: SIRType.List(SIRType.Data))
    val unIData = SIR.Builtin(DefaultFun.UnIData, SIRType.Data ->: SIRType.IntegerPrimitive)
    val unBData = SIR.Builtin(DefaultFun.UnBData, SIRType.Data ->: SIRType.BooleanPrimitive)
    val equalsData =
        SIR.Builtin(
          DefaultFun.EqualsData,
          SIRType.Data ->: SIRType.Data ->: SIRType.BooleanPrimitive
        )
    val serialiseData =
        SIR.Builtin(DefaultFun.SerialiseData, SIRType.Data ->: SIRType.ByteStringPrimitive)

    //   TODO: think about pair
    val mkPairData =
        SIR.Builtin(
          DefaultFun.MkPairData,
          SIRType.Data ->: SIRType.Data ->: SIRType.Pair(SIRType.Data, SIRType.Data)
        )
    val mkNilData =
        SIR.Builtin(DefaultFun.MkNilData, SIRType.VoidPrimitive ->: SIRType.List(SIRType.Data))
    val mkNilPairData =
        SIR.Builtin(
          DefaultFun.MkNilPairData,
          SIRType.VoidPrimitive ->: SIRType.List(SIRType.Pair(SIRType.Data, SIRType.Data))
        )

    // BLS12_381 operations
    // G1 operations
    val bls12_381_G1_add = SIR.Builtin(
      DefaultFun.Bls12_381_G1_add,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element
    )
    val bls12_381_G1_neg = SIR.Builtin(
      DefaultFun.Bls12_381_G1_neg,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element
    )
    val bls12_381_G1_scalarMul = SIR.Builtin(
      DefaultFun.Bls12_381_G1_scalarMul,
      SIRType.IntegerPrimitive ->: SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element
    )
    val bls12_381_G1_equal = SIR.Builtin(
      DefaultFun.Bls12_381_G1_equal,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element ->: SIRType.BooleanPrimitive
    )
    val bls12_381_G1_hashToGroup = SIR.Builtin(
      DefaultFun.Bls12_381_G1_hashToGroup,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BLS12_381_G1_Element
    )
    val bls12_381_G1_compress = SIR.Builtin(
      DefaultFun.Bls12_381_G1_compress,
      SIRType.BLS12_381_G1_Element ->: SIRType.ByteStringPrimitive
    )
    val bls12_381_G1_uncompress = SIR.Builtin(
      DefaultFun.Bls12_381_G1_uncompress,
      SIRType.ByteStringPrimitive ->: SIRType.BLS12_381_G1_Element
    )

    // G2 operations
    val bls12_381_G2_add = SIR.Builtin(
      DefaultFun.Bls12_381_G2_add,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element
    )
    val bls12_381_G2_neg = SIR.Builtin(
      DefaultFun.Bls12_381_G2_neg,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element
    )
    val bls12_381_G2_scalarMul = SIR.Builtin(
      DefaultFun.Bls12_381_G2_scalarMul,
      SIRType.IntegerPrimitive ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element
    )
    val bls12_381_G2_equal = SIR.Builtin(
      DefaultFun.Bls12_381_G2_equal,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.BooleanPrimitive
    )
    val bls12_381_G2_hashToGroup = SIR.Builtin(
      DefaultFun.Bls12_381_G2_hashToGroup,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive ->: SIRType.BLS12_381_G2_Element
    )
    val bls12_381_G2_compress = SIR.Builtin(
      DefaultFun.Bls12_381_G2_compress,
      SIRType.BLS12_381_G2_Element ->: SIRType.ByteStringPrimitive
    )
    val bls12_381_G2_uncompress = SIR.Builtin(
      DefaultFun.Bls12_381_G2_uncompress,
      SIRType.ByteStringPrimitive ->: SIRType.BLS12_381_G2_Element
    )

    val bls12_381_mulMlResult = SIR.Builtin(
      DefaultFun.Bls12_381_mulMlResult,
      SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult
    )

    // Miller loop
    val bls12_381_millerLoop = SIR.Builtin(
      DefaultFun.Bls12_381_millerLoop,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_MlResult
    )
    // Final verification
    val bls12_381_finalVerify = SIR.Builtin(
      DefaultFun.Bls12_381_finalVerify,
      SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult ->: SIRType.BooleanPrimitive
    )

    // keccak_256
    val keccak_256 = SIR.Builtin(
      DefaultFun.Keccak_256,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )
    // blake
    val blake2b_224 = SIR.Builtin(
      DefaultFun.Blake2b_224,
      SIRType.ByteStringPrimitive ->: SIRType.ByteStringPrimitive
    )

    def fromUplc(uplcFun: DefaultFun): SIR.Builtin =
        uplcFun match
            case DefaultFun.AddInteger                      => addInteger
            case DefaultFun.SubtractInteger                 => subtractInteger
            case DefaultFun.MultiplyInteger                 => multiplyInteger
            case DefaultFun.DivideInteger                   => divideInteger
            case DefaultFun.QuotientInteger                 => quotientInteger
            case DefaultFun.RemainderInteger                => remainderInteger
            case DefaultFun.ModInteger                      => modInteger
            case DefaultFun.EqualsInteger                   => equalsInteger
            case DefaultFun.LessThanInteger                 => lessThanInteger
            case DefaultFun.LessThanEqualsInteger           => lessThanEqualsInteger
            case DefaultFun.AppendByteString                => appendByteString
            case DefaultFun.ConsByteString                  => consByteString
            case DefaultFun.SliceByteString                 => sliceByteString
            case DefaultFun.LengthOfByteString              => lengthOfByteString
            case DefaultFun.IndexByteString                 => indexByteString
            case DefaultFun.EqualsByteString                => equalsByteString
            case DefaultFun.LessThanByteString              => lessThanByteString
            case DefaultFun.LessThanEqualsByteString        => lessThanEqualsByteString
            case DefaultFun.Sha2_256                        => sha2_256
            case DefaultFun.Sha3_256                        => sha3_256
            case DefaultFun.Blake2b_256                     => blake2b_256
            case DefaultFun.VerifyEd25519Signature          => verifyEd25519Signature
            case DefaultFun.VerifyEcdsaSecp256k1Signature   => verifyEcdsaSecp256k1Signature
            case DefaultFun.VerifySchnorrSecp256k1Signature => verifySchnorrSecp256k1Signature
            case DefaultFun.AppendString                    => appendString
            case DefaultFun.EqualsString                    => equalsString
            case DefaultFun.EncodeUtf8                      => encodeUtf8
            case DefaultFun.DecodeUtf8                      => decodeUtf8
            case DefaultFun.IfThenElse                      => ifThenElse
            case DefaultFun.ChooseUnit                      => chooseUnit
            case DefaultFun.Trace                           => trace
            case DefaultFun.FstPair                         => fstPair
            case DefaultFun.SndPair                         => sndPair
            case DefaultFun.ChooseList                      => chooseList
            case DefaultFun.MkCons                          => mkCons
            case DefaultFun.HeadList                        => headList
            case DefaultFun.TailList                        => tailList
            case DefaultFun.NullList                        => nullList
            case DefaultFun.ChooseData                      => chooseData
            case DefaultFun.ConstrData                      => constrData
            case DefaultFun.MapData                         => mapData
            case DefaultFun.ListData                        => listData
            case DefaultFun.IData                           => iData
            case DefaultFun.BData                           => bData
            case DefaultFun.UnConstrData                    => unConstrData
            case DefaultFun.UnMapData                       => unMapData
            case DefaultFun.UnListData                      => unListData
            case DefaultFun.UnIData                         => unIData
            case DefaultFun.UnBData                         => unBData
            case DefaultFun.EqualsData                      => equalsData
            case DefaultFun.SerialiseData                   => serialiseData
            case DefaultFun.MkPairData                      => mkPairData
            case DefaultFun.MkNilData                       => mkNilData
            case DefaultFun.MkNilPairData                   => mkNilPairData
            case DefaultFun.Bls12_381_G1_add                => bls12_381_G1_add
            case DefaultFun.Bls12_381_G1_neg                => bls12_381_G1_neg
            case DefaultFun.Bls12_381_G1_scalarMul          => bls12_381_G1_scalarMul
            case DefaultFun.Bls12_381_G1_equal              => bls12_381_G1_equal
            case DefaultFun.Bls12_381_G1_hashToGroup        => bls12_381_G1_hashToGroup
            case DefaultFun.Bls12_381_G1_compress           => bls12_381_G1_compress
            case DefaultFun.Bls12_381_G1_uncompress         => bls12_381_G1_uncompress
            case DefaultFun.Bls12_381_G2_add                => bls12_381_G2_add
            case DefaultFun.Bls12_381_G2_neg                => bls12_381_G2_neg
            case DefaultFun.Bls12_381_G2_scalarMul          => bls12_381_G2_scalarMul
            case DefaultFun.Bls12_381_G2_equal              => bls12_381_G2_equal
            case DefaultFun.Bls12_381_G2_hashToGroup        => bls12_381_G2_hashToGroup
            case DefaultFun.Bls12_381_G2_compress           => bls12_381_G2_compress
            case DefaultFun.Bls12_381_G2_uncompress         => bls12_381_G2_uncompress
            case DefaultFun.Bls12_381_millerLoop            => bls12_381_millerLoop
            case DefaultFun.Bls12_381_mulMlResult           => bls12_381_mulMlResult
            case DefaultFun.Bls12_381_finalVerify           => bls12_381_finalVerify
            case DefaultFun.Keccak_256                      => keccak_256
            case DefaultFun.Blake2b_224                     => blake2b_224
}
