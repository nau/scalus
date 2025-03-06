package scalus.uplc

enum DefaultFun extends Enum[DefaultFun]:
    // Integers
    case AddInteger
    case SubtractInteger
    case MultiplyInteger
    case DivideInteger
    case QuotientInteger
    case RemainderInteger
    case ModInteger
    case EqualsInteger
    case LessThanInteger
    case LessThanEqualsInteger
    // Bytestrings
    case AppendByteString
    case ConsByteString
    case SliceByteString
    case LengthOfByteString
    case IndexByteString
    case EqualsByteString
    case LessThanByteString
    case LessThanEqualsByteString
    // Cryptography and hashes
    case Sha2_256
    case Sha3_256
    case Blake2b_256
    case VerifyEd25519Signature // formerly verifySignature
    case VerifyEcdsaSecp256k1Signature
    case VerifySchnorrSecp256k1Signature

    // Strings
    case AppendString
    case EqualsString
    case EncodeUtf8
    case DecodeUtf8

    // Bool
    case IfThenElse

    // Unit
    case ChooseUnit

    // Tracing
    case Trace

    // Pairs
    case FstPair

    case SndPair

    // Lists
    case ChooseList
    case MkCons
    case HeadList
    case TailList
    case NullList

    // Data
    // See Note [Pattern matching on built-in types].
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    case ChooseData
    case ConstrData
    case MapData
    case ListData
    case IData
    case BData
    case UnConstrData
    case UnMapData
    case UnListData
    case UnIData
    case UnBData
    case EqualsData
    case SerialiseData

    // Misc monomorphized constructors.
    // We could simply replace those with constants, but we use built-in functions for consistency
    // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    // See note [Representable built-in functions over polymorphic built-in types].
    case MkPairData
    case MkNilData
    case MkNilPairData
    // BLS12_381 operations
    // G1 operations
    case Bls12_381_G1_add
    case Bls12_381_G1_neg
    case Bls12_381_G1_scalarMul
    case Bls12_381_G1_equal
    case Bls12_381_G1_hashToGroup
    case Bls12_381_G1_compress
    case Bls12_381_G1_uncompress

    // G2 operations
    case Bls12_381_G2_add
    case Bls12_381_G2_neg
    case Bls12_381_G2_scalarMul
    case Bls12_381_G2_equal
    case Bls12_381_G2_hashToGroup
    case Bls12_381_G2_compress
    case Bls12_381_G2_uncompress

    // Pairing operations
    case Bls12_381_millerLoop
    case Bls12_381_mulMlResult
    case Bls12_381_finalVerify

    // Hash functions
    case Keccak_256
    case Blake2b_224

    // Conversions
    case IntegerToByteString
    case ByteStringToInteger
    case AndByteString
    case OrByteString
    case XorByteString

given DefaultFunOrdering: Ordering[DefaultFun] with
    def compare(x: DefaultFun, y: DefaultFun): Int = x.ordinal - y.ordinal
