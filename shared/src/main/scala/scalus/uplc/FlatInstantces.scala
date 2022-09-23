package scalus.uplc

import scalus.flat.{DecoderState, EncoderState, Flat}
import scalus.uplc.DefaultFun.*

object FlatInstantces:
  val constantWidth = 4
  val termTagWidth = 4

  given Flat[DefaultFun] with
    import Term.*

    def bitSize(a: DefaultFun): Int = 7

    def encode(a: DefaultFun, encode: EncoderState): Unit =
      val code = a match
        case AddInteger            => 0
        case SubtractInteger       => 1
        case MultiplyInteger       => 2
        case DivideInteger         => 3
        case QuotientInteger       => 4
        case RemainderInteger      => 5
        case ModInteger            => 6
        case EqualsInteger         => 7
        case LessThanInteger       => 8
        case LessThanEqualsInteger => 9

        case AppendByteString         => 10
        case ConsByteString           => 11
        case SliceByteString          => 12
        case LengthOfByteString       => 13
        case IndexByteString          => 14
        case EqualsByteString         => 15
        case LessThanByteString       => 16
        case LessThanEqualsByteString => 17

        case Sha2_256                        => 18
        case Sha3_256                        => 19
        case Blake2b_256                     => 20
        case VerifyEd25519Signature          => 21
        case VerifyEcdsaSecp256k1Signature   => 52
        case VerifySchnorrSecp256k1Signature => 53

        case AppendString => 22
        case EqualsString => 23
        case EncodeUtf8   => 24
        case DecodeUtf8   => 25

        case IfThenElse => 26

        case ChooseUnit => 27

        case Trace => 28

        case FstPair => 29
        case SndPair => 30

        case ChooseList => 31
        case MkCons     => 32
        case HeadList   => 33
        case TailList   => 34
        case NullList   => 35

        case ChooseData    => 36
        case ConstrData    => 37
        case MapData       => 38
        case ListData      => 39
        case IData         => 40
        case BData         => 41
        case UnConstrData  => 42
        case UnMapData     => 43
        case UnListData    => 44
        case UnIData       => 45
        case UnBData       => 46
        case EqualsData    => 47
        case MkPairData    => 48
        case MkNilData     => 49
        case MkNilPairData => 50
        case SerialiseData => 51
      encode.bits(7, code.toByte)

    def decode(decode: DecoderState): DefaultFun =
      decode.bits8(7) match
        case 0  => AddInteger
        case 1  => SubtractInteger
        case 2  => MultiplyInteger
        case 3  => DivideInteger
        case 4  => QuotientInteger
        case 5  => RemainderInteger
        case 6  => ModInteger
        case 7  => EqualsInteger
        case 8  => LessThanInteger
        case 9  => LessThanEqualsInteger
        case 10 => AppendByteString
        case 11 => ConsByteString
        case 12 => SliceByteString
        case 13 => LengthOfByteString
        case 14 => IndexByteString
        case 15 => EqualsByteString
        case 16 => LessThanByteString
        case 17 => LessThanEqualsByteString
        case 18 => Sha2_256
        case 19 => Sha3_256
        case 20 => Blake2b_256
        case 21 => VerifyEd25519Signature
        case 22 => AppendString
        case 23 => EqualsString
        case 24 => EncodeUtf8
        case 25 => DecodeUtf8
        case 26 => IfThenElse
        case 27 => ChooseUnit
        case 28 => Trace
        case 29 => FstPair
        case 30 => SndPair
        case 31 => ChooseList
        case 32 => MkCons
        case 33 => HeadList
        case 34 => TailList
        case 35 => NullList
        case 36 => ChooseData
        case 37 => ConstrData
        case 38 => MapData
        case 39 => ListData
        case 40 => IData
        case 41 => BData
        case 42 => UnConstrData
        case 43 => UnMapData
        case 44 => UnListData
        case 45 => UnIData
        case 46 => UnBData
        case 47 => EqualsData
        case 48 => MkPairData
        case 49 => MkNilData
        case 50 => MkNilPairData
        case 51 => SerialiseData
        case 52 => VerifyEcdsaSecp256k1Signature
        case 53 => VerifySchnorrSecp256k1Signature
        case c  => throw new Exception(s"Invalid builtin function code: $c")

  given Flat[Term] with
    import Term.*

    def bitSize(a: Term): Int = a match
      //      case Var(_)      => constantWidth
      //      case Const(_)    => constantWidth
      case Apply(f, x) => bitSize(f) + bitSize(x)
      //      case LamAbs(x, t) => bitSize(x) + bitSize(t)
      case Force(term) => termTagWidth + bitSize(term)
      case Delay(term) => termTagWidth + bitSize(term)
      case Builtin(bn) => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
      case Error(_)    => termTagWidth

    def encode(a: Term, encode: EncoderState): Unit = ???

    def decode(decode: DecoderState): Term = ???
