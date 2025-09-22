package scalus.uplc

import scalus.builtin
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.uplc.DefaultFun.*

object CommonFlatInstances:
    val constantWidth = 4

    given Flat[builtin.ByteString] with
        val flatArray = summon[Flat[Array[Byte]]]

        def bitSize(a: builtin.ByteString): Int =
            flatArray.bitSize(a.bytes)

        def encode(a: builtin.ByteString, encode: EncoderState): Unit =
            flatArray.encode(a.bytes, encode)

        def decode(decode: DecoderState): builtin.ByteString =
            builtin.ByteString.unsafeFromArray(flatArray.decode(decode))

    def flatForUni(uni: DefaultUni)(using Flat[builtin.Data]): Flat[Any] =
        import DefaultUni.*
        uni match
            case Integer             => summon[Flat[BigInt]].asInstanceOf[Flat[Any]]
            case ByteString          => summon[Flat[builtin.ByteString]].asInstanceOf[Flat[Any]]
            case String              => summon[Flat[String]].asInstanceOf[Flat[Any]]
            case Unit                => summon[Flat[Unit]].asInstanceOf[Flat[Any]]
            case Bool                => summon[Flat[Boolean]].asInstanceOf[Flat[Any]]
            case Data                => summon[Flat[builtin.Data]].asInstanceOf[Flat[Any]]
            case Apply(ProtoList, a) => listFlat(using flatForUni(a)).asInstanceOf[Flat[Any]]
            case Apply(Apply(ProtoPair, a), b) =>
                pairFlat(using flatForUni(a), flatForUni(b)).asInstanceOf[Flat[Any]]
            case _ => throw new Exception(s"Unsupported uni: $uni")

    def encodeUni(uni: DefaultUni): List[Int] =
        uni match
            case DefaultUni.Integer              => List(0)
            case DefaultUni.ByteString           => List(1)
            case DefaultUni.String               => List(2)
            case DefaultUni.Unit                 => List(3)
            case DefaultUni.Bool                 => List(4)
            case DefaultUni.ProtoList            => List(5)
            case DefaultUni.ProtoPair            => List(6)
            case DefaultUni.Apply(uniF, uniA)    => 7 :: encodeUni(uniF) ++ encodeUni(uniA)
            case DefaultUni.Data                 => List(8)
            case DefaultUni.BLS12_381_G1_Element => List(9)
            case DefaultUni.BLS12_381_G2_Element => List(10)
            case DefaultUni.BLS12_381_MlResult   => List(11)

    def decodeUni(state: List[Int]): (DefaultUni, List[Int]) =
        state match
            case 0 :: tail => (DefaultUni.Integer, tail)
            case 1 :: tail => (DefaultUni.ByteString, tail)
            case 2 :: tail => (DefaultUni.String, tail)
            case 3 :: tail => (DefaultUni.Unit, tail)
            case 4 :: tail => (DefaultUni.Bool, tail)
            case 5 :: tail => (DefaultUni.ProtoList, tail)
            case 6 :: tail => (DefaultUni.ProtoPair, tail)
            case 7 :: tail =>
                val (uniF, tail1) = decodeUni(tail)
                val (uniA, tail2) = decodeUni(tail1)
                (DefaultUni.Apply(uniF, uniA), tail2)
            case 8 :: tail  => (DefaultUni.Data, tail)
            case 9 :: tail  => (DefaultUni.BLS12_381_G1_Element, tail)
            case 10 :: tail => (DefaultUni.BLS12_381_G2_Element, tail)
            case 11 :: tail => (DefaultUni.BLS12_381_MlResult, tail)
            case _          => throw new Exception(s"Invalid uni: $state")

    given Flat[DefaultFun] with
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

                case Sha2_256               => 18
                case Sha3_256               => 19
                case Blake2b_256            => 20
                case VerifyEd25519Signature => 21

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

                case ChooseData                      => 36
                case ConstrData                      => 37
                case MapData                         => 38
                case ListData                        => 39
                case IData                           => 40
                case BData                           => 41
                case UnConstrData                    => 42
                case UnMapData                       => 43
                case UnListData                      => 44
                case UnIData                         => 45
                case UnBData                         => 46
                case EqualsData                      => 47
                case MkPairData                      => 48
                case MkNilData                       => 49
                case MkNilPairData                   => 50
                case SerialiseData                   => 51
                case VerifyEcdsaSecp256k1Signature   => 52
                case VerifySchnorrSecp256k1Signature => 53
                case Bls12_381_G1_add                => 54
                case Bls12_381_G1_neg                => 55
                case Bls12_381_G1_scalarMul          => 56
                case Bls12_381_G1_equal              => 57
                case Bls12_381_G1_compress           => 58
                case Bls12_381_G1_uncompress         => 59
                case Bls12_381_G1_hashToGroup        => 60
                case Bls12_381_G2_add                => 61
                case Bls12_381_G2_neg                => 62
                case Bls12_381_G2_scalarMul          => 63
                case Bls12_381_G2_equal              => 64
                case Bls12_381_G2_compress           => 65
                case Bls12_381_G2_uncompress         => 66
                case Bls12_381_G2_hashToGroup        => 67
                case Bls12_381_millerLoop            => 68
                case Bls12_381_mulMlResult           => 69
                case Bls12_381_finalVerify           => 70
                case Keccak_256                      => 71
                case Blake2b_224                     => 72

                case IntegerToByteString  => 73
                case ByteStringToInteger  => 74
                case AndByteString        => 75
                case OrByteString         => 76
                case XorByteString        => 77
                case ComplementByteString => 78
                case ReadBit              => 79
                case WriteBits            => 80
                case ReplicateByte        => 81

                case ShiftByteString  => 82
                case RotateByteString => 83
                case CountSetBits     => 84
                case FindFirstSetBit  => 85
                case Ripemd_160       => 86

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
                case 54 => Bls12_381_G1_add
                case 55 => Bls12_381_G1_neg
                case 56 => Bls12_381_G1_scalarMul
                case 57 => Bls12_381_G1_equal
                case 58 => Bls12_381_G1_compress
                case 59 => Bls12_381_G1_uncompress
                case 60 => Bls12_381_G1_hashToGroup
                case 61 => Bls12_381_G2_add
                case 62 => Bls12_381_G2_neg
                case 63 => Bls12_381_G2_scalarMul
                case 64 => Bls12_381_G2_equal
                case 65 => Bls12_381_G2_compress
                case 66 => Bls12_381_G2_uncompress
                case 67 => Bls12_381_G2_hashToGroup
                case 68 => Bls12_381_millerLoop
                case 69 => Bls12_381_mulMlResult
                case 70 => Bls12_381_finalVerify
                case 71 => Keccak_256
                case 72 => Blake2b_224
                case 73 => IntegerToByteString
                case 74 => ByteStringToInteger
                case 75 => AndByteString
                case 76 => OrByteString
                case 77 => XorByteString
                case 78 => ComplementByteString
                case 79 => ReadBit
                case 80 => WriteBits
                case 81 => ReplicateByte
                case 82 => ShiftByteString
                case 83 => RotateByteString
                case 84 => CountSetBits
                case 85 => FindFirstSetBit
                case 86 => Ripemd_160
                case c  => throw new Exception(s"Invalid builtin function code: $c")

    def flatConstant(using Flat[builtin.Data]): Flat[Constant] = new Flat[Constant]:

        val constantTypeTagFlat = new Flat[Int]:
            def bitSize(a: Int): Int = constantWidth

            def encode(a: Int, encode: EncoderState): Unit = encode.bits(constantWidth, a.toByte)

            def decode(decode: DecoderState): Int = decode.bits8(constantWidth)

        def bitSize(a: Constant): Int =
            val uniSize = encodeUni(
              a.tpe
            ).length * (1 + constantWidth) + 1 // List Cons (1 bit) + constant + List Nil (1 bit)
            val valueSize = flatForUni(a.tpe).bitSize(Constant.toValue(a))
            val retval = uniSize + valueSize
            retval

        def encode(a: Constant, encoder: EncoderState): Unit =
            val tags = encodeUni(a.tpe)
            listFlat[Int](using constantTypeTagFlat).encode(tags, encoder)
            flatForUni(a.tpe).encode(Constant.toValue(a), encoder)

        def decode(decoder: DecoderState): Constant =
            val tags = listFlat[Int](using constantTypeTagFlat).decode(decoder)
            val (tpe, _) = decodeUni(tags)
            val uniDecoder = flatForUni(tpe)
            val decoded = uniDecoder.decode(decoder)
            val result = Constant.fromValue(tpe, decoded)
            result
