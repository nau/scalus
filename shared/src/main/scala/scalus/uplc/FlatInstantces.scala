package scalus.uplc

import io.bullet.borer.{Cbor, Decoder, Encoder}
import scalus.flat
import scalus.flat.{DecoderState, EncoderState, Flat, Natural, given}
import scalus.uplc.CommonFlatInstances.*
import scalus.uplc.CommonFlatInstances.given
import scalus.builtin.{PlutusDataCborDecoder, PlutusDataCborEncoder}

object FlatInstantces:
    val termTagWidth = 4

    given Flat[Data] with
        implicit val plutusDataCborEncoder: Encoder[Data] = PlutusDataCborEncoder
        implicit val plutusDataCborDecoder: Decoder[Data] = PlutusDataCborDecoder

        def bitSize(a: Data): Int = summon[Flat[Array[Byte]]].bitSize(Cbor.encode(a).toByteArray)

        def encode(a: Data, encode: EncoderState): Unit =
            flat.encode(Cbor.encode(a).toByteArray, encode)

        def decode(decode: DecoderState): Data =
            val bytes = summon[Flat[Array[Byte]]].decode(decode)
            Cbor.decode(bytes).to[Data].value

    given Flat[Term] with
        import Term.*

        def bitSize(a: Term): Int = a match
            case Var(name) =>
                // in Plutus See Note [Index (Word64) (de)serialized through Natural]
                termTagWidth + summon[Flat[Natural]].bitSize(Natural(BigInt(name.index)))
            case Const(c)     => termTagWidth + flatConstant.bitSize(c)
            case Apply(f, x)  => termTagWidth + bitSize(f) + bitSize(x)
            case LamAbs(x, t) => termTagWidth + bitSize(t)
            case Force(term)  => termTagWidth + bitSize(term)
            case Delay(term)  => termTagWidth + bitSize(term)
            case Builtin(bn)  => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error        => termTagWidth

        def encode(a: Term, enc: EncoderState): Unit =
            a match
                case Term.Var(name) =>
                    enc.bits(termTagWidth, 0)
                    summon[Flat[Natural]].encode(Natural(BigInt(name.index)), enc)
                case Term.Delay(term) =>
                    enc.bits(termTagWidth, 1)
                    encode(term, enc)
                case Term.LamAbs(name, term) =>
                    enc.bits(termTagWidth, 2)
                    encode(term, enc)
                case Term.Apply(f, arg) =>
                    enc.bits(termTagWidth, 3)
                    encode(f, enc); encode(arg, enc)
                case Term.Const(const) =>
                    enc.bits(termTagWidth, 4)
                    flatConstant.encode(const, enc)
                case Term.Force(term) =>
                    enc.bits(termTagWidth, 5)
                    encode(term, enc)
                case Term.Error =>
                    enc.bits(termTagWidth, 6)
                case Term.Builtin(bn) =>
                    enc.bits(termTagWidth, 7)
                    flat.encode(bn, enc)

        def decode(decoder: DecoderState): Term =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case 0 =>
                    val index = summon[Flat[Natural]].decode(decoder).n.toInt
                    val name = s"i$index"
                    Term.Var(NamedDeBruijn(name, index))
                case 1 =>
                    val term = decode(decoder)
                    Term.Delay(term)
                case 2 =>
                    val term = decode(decoder)
                    // in plutus-core it's super-duper over complicated, but it all boils down to this
                    // https://github.com/input-output-hk/plutus/blob/a56c96598b4b25c9e28215214d25189331087244/plutus-core/plutus-core/src/PlutusCore/Flat.hs#L357
                    Term.LamAbs("i0", term)
                case 3 =>
                    val f = decode(decoder)
                    val arg = decode(decoder)
                    Term.Apply(f, arg)
                case 4 =>
                    Term.Const(flatConstant.decode(decoder))
                case 5 =>
                    val term = decode(decoder)
                    Term.Force(term)
                case 6 =>
                    Term.Error
                case 7 =>
                    Term.Builtin(flat.decode(decoder))

    given Flat[DeBruijnedProgram] with
        val fn = summon[Flat[Natural]]
        def bitSize(a: DeBruijnedProgram): Int =
            fn.bitSize(Natural(BigInt(a.version._1))) +
                fn.bitSize(Natural(BigInt(a.version._2))) +
                fn.bitSize(Natural(BigInt(a.version._3))) +
                summon[Flat[Term]].bitSize(a.term)

        def encode(a: DeBruijnedProgram, enc: EncoderState): Unit =
            fn.encode(Natural(BigInt(a.version._1)), enc)
            fn.encode(Natural(BigInt(a.version._2)), enc)
            fn.encode(Natural(BigInt(a.version._3)), enc)
            flat.encode(a.term, enc)

        def decode(decoder: DecoderState): DeBruijnedProgram =
            val v1 = fn.decode(decoder).n.toInt
            val v2 = fn.decode(decoder).n.toInt
            val v3 = fn.decode(decoder).n.toInt
            val term = flat.decode[Term](decoder)
            DeBruijnedProgram((v1, v2, v3), term)
