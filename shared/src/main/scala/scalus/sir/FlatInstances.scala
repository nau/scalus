package scalus.flat

import scalus.builtin
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.given
import scalus.sir.Binding
import scalus.sir.Case
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Module
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.uplc.CommonFlatInstances.*
import scalus.uplc.CommonFlatInstances.given
import scalus.builtin.Data
import scalus.uplc.DefaultFun

object FlatInstantces:
    val termTagWidth = 4

    given Flat[Data] with
        private val width = 3

        def bitSize(a: Data): Int = a match
            case Data.Constr(constr, args) =>
                width + summon[Flat[Long]].bitSize(constr) + summon[Flat[List[Data]]].bitSize(args)
            case Data.Map(values) =>
                width + summom[Flat[List[(Data,Data)]]].bitSize(values)
            case Data.List(values) => width + summon[Flat[List[Data]]].bitSize(values)
            case Data.I(value)     => width + summon[Flat[BigInt]].bitSize(value)
            case Data.B(value)     => width + summon[Flat[builtin.ByteString]].bitSize(value)

        def encode(a: Data, enc: EncoderState): Unit =
            a match
                case Data.Constr(constr, args) =>
                    enc.bits(width, 0)
                    summon[Flat[Long]].encode(constr, enc)
                    summon[Flat[List[Data]]].encode(args, enc)
                case Data.Map(values) =>
                    enc.bits(width, 1)
                    summon[Flat[List[(Data, Data)]].encode(values, enc)
                case Data.List(values) =>
                    enc.bits(width, 2)
                    summon[Flat[List[Data]]].encode(values, enc)
                case Data.I(value) =>
                    enc.bits(width, 3)
                    summon[Flat[BigInt]].encode(value, enc)
                case Data.B(value) =>
                    enc.bits(width, 4)
                    summon[Flat[builtin.ByteString]].encode(value, enc)

        def decode(decode: DecoderState): Data =
            decode.bits8(width) match
                case 0 =>
                    val constr = summon[Flat[Long]].decode(decode)
                    val args = summon[Flat[List[Data]]].decode(decode)
                    Data.Constr(constr, args)
                case 1 =>
                    val values = summon[Flat[List[(Data, Data)]]].decode(decode)
                    Data.Map(values)
                case 2 =>
                    val values = summon[Flat[List[Data]]].decode(decode)
                    Data.List(values)
                case 3 =>
                    val value = summon[Flat[BigInt]].decode(decode)
                    Data.I(value)
                case 4 =>
                    val value = summon[Flat[builtin.ByteString]].decode(decode)
                    Data.B(value)
                case c => throw new Exception(s"Invalid data code: $c")

    given Flat[Recursivity] with
        def bitSize(a: Recursivity): Int = 1

        def encode(a: Recursivity, encode: EncoderState): Unit =
            encode.bits(1, if a == Recursivity.Rec then 1 else 0)

        def decode(decode: DecoderState): Recursivity =
            if decode.bits8(1) == 1 then Recursivity.Rec else Recursivity.NonRec

    given Flat[Binding] with
        def bitSize(a: Binding): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val termSize = summon[Flat[SIR]].bitSize(a.value)
            nameSize + termSize
        def encode(a: Binding, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[SIR]].encode(a.value, encode)
        def decode(decode: DecoderState): Binding =
            val name = summon[Flat[String]].decode(decode)
            val term = summon[Flat[SIR]].decode(decode)
            Binding(name, term)

    given Flat[ConstrDecl] with
        def bitSize(a: ConstrDecl): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val paramsSize = summon[Flat[List[String]]].bitSize(a.params)
            nameSize + paramsSize
        def encode(a: ConstrDecl, encode: EncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[List[String]]].encode(a.params, encode)
        }
        def decode(decode: DecoderState): ConstrDecl = {
            val name = summon[Flat[String]].decode(decode)
            val params = summon[Flat[List[String]]].decode(decode)
            ConstrDecl(name, params)
        }

    given Flat[DataDecl] with
        def bitSize(a: DataDecl): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val constrSize = listFlat[ConstrDecl].bitSize(a.constructors)
            nameSize + constrSize
        def encode(a: DataDecl, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)
            listFlat[ConstrDecl].encode(a.constructors, encode)
        def decode(decode: DecoderState): DataDecl =
            val name = summon[Flat[String]].decode(decode)
            val constr = listFlat[ConstrDecl].decode(decode)
            DataDecl(name, constr)

    given Flat[Case] with {
        def bitSize(a: Case): Int =
            val constrSize = summon[Flat[ConstrDecl]].bitSize(a.constr)
            val bindings = summon[Flat[List[String]]].bitSize(a.bindings)
            val bodySize = summon[Flat[SIR]].bitSize(a.body)
            constrSize + bindings + bodySize
        def encode(a: Case, encode: EncoderState): Unit = {
            summon[Flat[ConstrDecl]].encode(a.constr, encode)
            summon[Flat[List[String]]].encode(a.bindings, encode)
            summon[Flat[SIR]].encode(a.body, encode)
        }
        def decode(decode: DecoderState): Case = {
            val constr = summon[Flat[ConstrDecl]].decode(decode)
            val bindings = summon[Flat[List[String]]].decode(decode)
            val body = summon[Flat[SIR]].decode(decode)
            Case(constr, bindings, body)
        }
    }

    given Flat[SIR] with
        import SIR.*

        def bitSize(a: SIR): Int = a match
            case Var(name) =>
                termTagWidth + summon[Flat[String]].bitSize(name)
            case ExternalVar(modName, name) =>
                termTagWidth + summon[Flat[String]].bitSize(modName) + summon[Flat[String]].bitSize(
                  name
                )
            case Let(rec, binds, body) =>
                termTagWidth + summon[Flat[Recursivity]].bitSize(rec) + listFlat[Binding].bitSize(
                  binds
                ) + bitSize(body)
            case LamAbs(x, t)        => termTagWidth + summon[Flat[String]].bitSize(x) + bitSize(t)
            case Apply(f, x)         => termTagWidth + bitSize(f) + bitSize(x)
            case Const(c)            => termTagWidth + flatConstant.bitSize(c)
            case And(x, y)           => termTagWidth + bitSize(x) + bitSize(y)
            case Or(x, y)            => termTagWidth + bitSize(x) + bitSize(y)
            case Not(x)              => termTagWidth + bitSize(x)
            case IfThenElse(c, t, f) => termTagWidth + bitSize(c) + bitSize(t) + bitSize(f)
            case Builtin(bn)         => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error(msg)          => termTagWidth + summon[Flat[String]].bitSize(msg)
            case Decl(data, term) =>
                termTagWidth + summon[Flat[DataDecl]].bitSize(data) + bitSize(term)
            case Constr(name, data, args) =>
                termTagWidth + summon[Flat[String]].bitSize(name) + summon[Flat[DataDecl]].bitSize(
                  data
                ) + listFlat[SIR].bitSize(args)
            case Match(scrutinee, cases) =>
                termTagWidth + bitSize(scrutinee) + listFlat[Case].bitSize(cases)

        def encode(a: SIR, enc: EncoderState): Unit =
            a match
                case Var(name) =>
                    enc.bits(termTagWidth, 0)
                    summon[Flat[String]].encode(name, enc)
                case Let(rec, binds, body) =>
                    enc.bits(termTagWidth, 1)
                    summon[Flat[Recursivity]].encode(rec, enc)
                    listFlat[Binding].encode(binds, enc)
                    encode(body, enc)
                case LamAbs(x, t) =>
                    enc.bits(termTagWidth, 2)
                    summon[Flat[String]].encode(x, enc)
                    encode(t, enc)
                case Apply(f, x) =>
                    enc.bits(termTagWidth, 3)
                    encode(f, enc)
                    encode(x, enc)
                case Const(c) =>
                    enc.bits(termTagWidth, 4)
                    flatConstant.encode(c, enc)
                case IfThenElse(c, t, f) =>
                    enc.bits(termTagWidth, 5)
                    encode(c, enc)
                    encode(t, enc)
                    encode(f, enc)
                case Builtin(bn) =>
                    enc.bits(termTagWidth, 6)
                    summon[Flat[DefaultFun]].encode(bn, enc)
                case Error(msg) =>
                    enc.bits(termTagWidth, 7)
                    summon[Flat[String]].encode(msg, enc)
                case Decl(data, term) =>
                    enc.bits(termTagWidth, 8)
                    summon[Flat[DataDecl]].encode(data, enc)
                    encode(term, enc)
                case Constr(name, data, args) =>
                    enc.bits(termTagWidth, 9)
                    summon[Flat[String]].encode(name, enc)
                    summon[Flat[DataDecl]].encode(data, enc)
                    listFlat[SIR].encode(args, enc)
                case Match(scrutinee, cases) =>
                    enc.bits(termTagWidth, 10)
                    encode(scrutinee, enc)
                    listFlat[Case].encode(cases, enc)
                case ExternalVar(modName, name) =>
                    enc.bits(termTagWidth, 11)
                    summon[Flat[String]].encode(modName, enc)
                    summon[Flat[String]].encode(name, enc)
                case And(x, y) =>
                    enc.bits(termTagWidth, 12)
                    encode(x, enc)
                    encode(y, enc)
                case Or(x, y) =>
                    enc.bits(termTagWidth, 13)
                    encode(x, enc)
                    encode(y, enc)
                case Not(x) =>
                    enc.bits(termTagWidth, 14)
                    encode(x, enc)

        def decode(decoder: DecoderState): SIR =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case 0 =>
                    val name = summon[Flat[String]].decode(decoder)
                    Var(name)
                case 1 =>
                    val rec = summon[Flat[Recursivity]].decode(decoder)
                    val binds = listFlat[Binding].decode(decoder)
                    val body = decode(decoder)
                    Let(rec, binds, body)
                case 2 =>
                    val x = summon[Flat[String]].decode(decoder)
                    val t = decode(decoder)
                    LamAbs(x, t)
                case 3 =>
                    val f = decode(decoder)
                    val x = decode(decoder)
                    Apply(f, x)
                case 4 =>
                    val c = flatConstant.decode(decoder)
                    Const(c)
                case 5 =>
                    val c = decode(decoder)
                    val t = decode(decoder)
                    val f = decode(decoder)
                    IfThenElse(c, t, f)
                case 6 =>
                    val bn = summon[Flat[DefaultFun]].decode(decoder)
                    Builtin(bn)
                case 7 =>
                    val msg = summon[Flat[String]].decode(decoder)
                    Error(msg)
                case 8 =>
                    val data = summon[Flat[DataDecl]].decode(decoder)
                    val term = decode(decoder)
                    Decl(data, term)
                case 9 =>
                    val name = summon[Flat[String]].decode(decoder)
                    val data = summon[Flat[DataDecl]].decode(decoder)
                    val args = listFlat[SIR].decode(decoder)
                    Constr(name, data, args)
                case 10 =>
                    val scrutinee = decode(decoder)
                    val cases = listFlat[Case].decode(decoder)
                    Match(scrutinee, cases)
                case 11 =>
                    val modName = summon[Flat[String]].decode(decoder)
                    val name = summon[Flat[String]].decode(decoder)
                    ExternalVar(modName, name)
                case 12 =>
                    val x = decode(decoder)
                    val y = decode(decoder)
                    And(x, y)
                case 13 =>
                    val x = decode(decoder)
                    val y = decode(decoder)
                    Or(x, y)
                case 14 =>
                    val x = decode(decoder)
                    Not(x)

    given Flat[Module] with
        def bitSize(a: Module): Int = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].bitSize(version) +
                    summon[Flat[List[Binding]]].bitSize(defs)
        def encode(a: Module, enc: EncoderState): Unit = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].encode(version, enc)
                summon[Flat[List[Binding]]].encode(defs, enc)
        def decode(decoder: DecoderState): Module =
            val version = summon[Flat[(Int, Int)]].decode(decoder)
            val defs = summon[Flat[List[Binding]]].decode(decoder)
            Module(version, defs)
