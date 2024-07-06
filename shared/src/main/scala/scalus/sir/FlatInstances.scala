package scalus.flat

import scalus.builtin
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.given
import scalus.sir.{Binding, ConstrDecl, DataDecl, Module, Recursivity, SIR, SIRBuiltins, SIRDef, SIRExpr, SIRType, SIRVarStorage, TypeBinding}
import scalus.sir.SIR.Case
import scalus.uplc.CommonFlatInstances.*
import scalus.uplc.CommonFlatInstances.given
import scalus.builtin.Data
import scalus.uplc.DefaultFun
import scalus.utils.*

object FlatInstantces:
    val termTagWidth = 4

    final val tagDecl = 0x08

    given Flat[Data] with
        private val width = 3

        def bitSize(a: Data, hashConsed: HashConsed.Map): Int = a match
            case Data.Constr(constr, args) =>
                width + summon[Flat[Long]].bitSize(constr, hashConsed) + args.map(bitSize(_,hashConsed)).sum
            case Data.Map(values) =>
                width + values.map { case (k, v) => bitSize(k,hashConsed) + bitSize(v,hashConsed) }.sum
            case Data.List(values) => width + values.map(bitSize(_,hashConsed)).sum
            case Data.I(value)     => width + summon[Flat[BigInt]].bitSize(value,hashConsed)
            case Data.B(value)     => width + summon[Flat[builtin.ByteString]].bitSize(value,hashConsed)

        def encode(a: Data, enc: EncoderState): Unit =
            a match
                case Data.Constr(constr, args) =>
                    enc.bits(width, 0)
                    summon[Flat[Long]].encode(constr, enc)
                    args.foreach(a => encode(a, enc))
                case Data.Map(values) =>
                    enc.bits(width, 1)
                    values.foreach { case (k, v) => encode(k, enc); encode(v, enc) }
                case Data.List(values) =>
                    enc.bits(width, 2)
                    values.foreach(a => encode(a, enc))
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
        def bitSize(a: Recursivity, hashConsed: HashConsed.Map): Int = 1

        def encode(a: Recursivity, encode: EncoderState): Unit =
            encode.bits(1, if a == Recursivity.Rec then 1 else 0)

        def decode(decode: DecoderState): Recursivity =
            if decode.bits8(1) == 1 then Recursivity.Rec else Recursivity.NonRec


    given flatBinding: Flat[Binding] with
        def bitSize(a: Binding, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name, hashConsed)
            val termSize = summon[Flat[SIR]].bitSize(a.value, hashConsed)
            nameSize + termSize
        def encode(a: Binding, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[SIR]].encode(a.value, encode)
        def decode(decode: DecoderState): Binding =
            val name = summon[Flat[String]].decode(decode)
            val term = summon[Flat[SIRExpr]].decode(decode)
            Binding(name, term)

    given flatConstrDecl:  Flat[ConstrDecl] with
        def bitSize(a: ConstrDecl, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name, hashConsed)
            val storageTypeSize = summon[Flat[SIRVarStorage]].bitSize(a.storageType, hashConsed)
            val paramsSize = summon[Flat[List[TypeBinding]]].bitSize(a.params, hashConsed)
            val typeParamsSize = summon[Flat[List[SIRType.TypeVar]]].bitSize(a.typeParams, hashConsed)
            nameSize +  storageTypeSize + paramsSize + typeParamsSize

        def encode(a: ConstrDecl, encode: EncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[SIRVarStorage]].encode(a.storageType, encode)
            summon[Flat[List[TypeBinding]]].encode(a.params, encode)
            summon[Flat[List[SIRType.TypeVar]]].encode(a.typeParams, encode)
            summon[Flat[List[SIRType]]].encode(a.parentTypeArgs, encode)
            // TODO: chek and resolve forward references to a
            val _ = encode.hashConsed.putRef(a)
        }

        def decode(decode: DecoderState): ConstrDecl = {
            val name = summon[Flat[String]].decode(decode)
            val storageType  = summon[Flat[SIRVarStorage]].decode(decode)
            val params = summon[Flat[List[TypeBinding]]].decode(decode)
            val typeParams = summon[Flat[List[SIRType.TypeVar]]].decode(decode)
            val parentTypeArgs = summon[Flat[List[SIRType]]].decode(decode)
            ConstrDecl(name, storageType, params, typeParams, parentTypeArgs)
        }

    given flatDataDecl: Flat[DataDecl] with
        def bitSize(a: DataDecl, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name, hashConsed)
            val constrSize = listFlat[ConstrDecl].bitSize(a.constructors, hashConsed)
            val typeParamsSize = listFlat[SIRType.TypeVar].bitSize(a.typeParams, hashConsed)
            nameSize + constrSize + typeParamsSize
        def encode(a: DataDecl, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)
            listFlat[ConstrDecl].encode(a.constructors, encode)
            listFlat[SIRType.TypeVar].encode(a.typeParams, encode)
            encode.hashConsed.putRef(a)
        def decode(decode: DecoderState): DataDecl =
            val name = summon[Flat[String]].decode(decode)
            val constr = listFlat[ConstrDecl].decode(decode)
            val typeParams = listFlat[SIRType.TypeVar].decode(decode)
            DataDecl(name, constr,typeParams)

    given flatCase: Flat[SIR.Case] with {
        def bitSize(a: SIR.Case, hashConsed: HashConsed.Map): Int =
            val constrSize = summon[Flat[ConstrDecl]].bitSize(a.constr, hashConsed)
            val bindings = summon[Flat[List[String]]].bitSize(a.bindings, hashConsed)
            val typeBindings = summon[Flat[List[SIRType]]].bitSize(a.typeBindings, hashConsed)
            val bodySize = summon[Flat[SIR]].bitSize(a.body, hashConsed)
            constrSize + bindings + typeBindings + bodySize
        def encode(a: SIR.Case, encode: EncoderState): Unit = {
            summon[Flat[ConstrDecl]].encode(a.constr, encode)
            summon[Flat[List[String]]].encode(a.bindings, encode)
            summon[Flat[List[SIRType]]].encode(a.typeBindings, encode)
            summon[Flat[SIRExpr]].encode(a.body, encode)
        }
        def decode(decode: DecoderState): SIR.Case = {
            val constr = summon[Flat[ConstrDecl]].decode(decode)
            val bindings = summon[Flat[List[String]]].decode(decode)
            val typeBindings = summon[Flat[List[SIRType]]].decode(decode)
            val body = summon[Flat[SIRExpr]].decode(decode)
            SIR.Case(constr, bindings, typeBindings, body)
        }
    }

    //  sealed trait Tree[A]
    //  case class Leaf[A](a:A) extends Tree[A]
    //  case class Node(left: Tree[A], right: Tree[A])
    
    //
    //  DataDecl(
    //     "Tree"
    //     List(
    //        ConsDecl("Leaf",  List(TypeBinding("a", SIRType.TypeVar("A")))), List(TypeVar(Ä"))),
    //        ConsDecl("Node", List(TypeBinding("left", SIRType.SumCaseClass(DataDecl(***), List(TypeVar[A]))
    //                         List(TypeBinding("right", SIRType.SumCaseClass(DataDecl(***), .List(TypeVar("A")))))
    //                         )
    
    given Flat[SIRType] with

        final val tagWidth = 4

        val tagPrimitiveByteString = 0x00
        val tagPrimitiveInteger = 0x01
        val tagPrimitiveString = 0x02
        val tagPrimitiveBoolean = 0x03
        val tagPrimitiveUnit = 0x04
        val tagPrimitiveData = 0x05
        val tagCaseClass = 0x06
        val tagSumCaseClass = 0x07
        val tagFun = 0x08
        val tagTypeVar = 0x09
        val tagTypeLambda = 0x0A
        val tagHashConsedType = 0x0B
        val tagHashCondedDataDecl = 0x0C
        val tagHashCondedConstrDecl = 0x0D

        //

        override def bitSize(a: SIRType, hashConsed: HashConsed.Map): Int =
            a match
                case _: SIRType.Primitive[?] => tagWidth
                case SIRType.Data => tagWidth
                case SIRType.CaseClass(constrDecl, typeParams) =>
                    hashConsed.lookup(a) match
                        case Some(ref) =>
                            tagWidth + summon[Flat[HashConsed.Ref]].bitSize(ref, hashConsed)
                        case None =>
                            hashConsed.lookup(constrDecl) match
                                case Some(ref) =>
                                    tagWidth + summon[Flat[HashConsed.Ref]].bitSize(ref, hashConsed)
                                case None =>
                                    val constrSize = summon[Flat[ConstrDecl]].bitSize(constrDecl, hashConsed)
                                    tagWidth + constrSize
                case SIRType.SumCaseClass(dataDecl, typeParams) =>
                    hashConsed.lookup(a) match
                        case Some(ref) =>
                            tagWidth + summon[Flat[HashConsed.Ref]].bitSize(ref, hashConsed)
                        case None =>
                            hashConsed.lookup(dataDecl) match
                                case Some(ref) =>
                                    tagWidth + summon[Flat[HashConsed.Ref]].bitSize(ref, hashConsed)
                                case None =>
                                    val dataDeclSize = summon[Flat[DataDecl]].bitSize(dataDecl, hashConsed)
                                    tagWidth + dataDeclSize

        override def encode(a: SIRType, encode: EncoderState): Unit = ???

        override def decode(decode: DecoderState): SIRType = ???
                                    
                                    


    given Flat[TypeBinding] with

        override def bitSize(a: TypeBinding, hashCons: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name, hashCons)
            val tpSize = summon[Flat[SIRType]].bitSize(a.tp, hashCons)
            nameSize + tpSize

        override def encode(a: TypeBinding, encode: EncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[SIRType]].encode(a.tp, encode)
        }

        override def decode(decode: DecoderState): TypeBinding = {
            val name = summon[Flat[String]].decode(decode)
            val tp = summon[Flat[SIRType]].decode(decode)
            TypeBinding(name, tp)
        }

    given Flat[SIRType.TypeVar] with
        override def bitSize(a: SIRType.TypeVar, hashCons: HashConsed.Map): Int =
            summon[Flat[String]].bitSize(a.name, hashCons)

        override def encode(a: SIRType.TypeVar, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)

        override def decode(decode: DecoderState): SIRType.TypeVar =
            val name = summon[Flat[String]].decode(decode)
            SIRType.TypeVar(name)

    given Flat[SIRExpr] with
        import SIR.*

        final val tagVar = 0x00
        final val tagLet = 0x01
        final val tagLamAbs = 0x02
        final val tagApply = 0x03
        final val tagConst = 0x04
        final val tagIfThenElse = 0x05
        final val tagBuiltin = 0x06
        final val tagError = 0x07
        final val tagConstr = 0x09
        final val tagMatch = 0x0A
        final val tagExternalVar = 0x0B
        final val tagAnd = 0x0C
        final val tagOr = 0x0D
        final val tagNot = 0x0E

        def bitSize(a: SIRExpr, hashCons: HashConsed.Map): Int = a match
            case Var(name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(name, hashCons)
            case ExternalVar(modName, name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(modName, hashCons)
                             + summon[Flat[String]].bitSize(name, hashCons)
                             + summon[Flat[SIRType]].bitSize(tp, hashCons)
            case Let(rec, binds, body) =>
                termTagWidth + summon[Flat[Recursivity]].bitSize(rec, hashCons) +
                    listFlat[Binding].bitSize(binds, hashCons) + bitSize(body, hashCons)
            case LamAbs(x, t)        =>
                termTagWidth + bitSize(x,hashCons) + bitSize(t, hashCons)
            case Apply(f, x, tp)     =>
                termTagWidth + bitSize(f,hashCons) + bitSize(x, hashCons) + summon[Flat[SIRType]].bitSize(tp, hashCons)
            case Const(c, tp)        => termTagWidth + flatConstant.bitSize(c, hashCons) + summon[Flat[SIRType]].bitSize(tp, hashCons)
            case And(x, y)           => termTagWidth + bitSize(x, hashCons) + bitSize(y, hashCons)
            case Or(x, y)            => termTagWidth + bitSize(x, hashCons) + bitSize(y, hashCons)
            case Not(x)              => termTagWidth + bitSize(x, hashCons)
            case IfThenElse(c, t, f, tp) =>
                termTagWidth + bitSize(c, hashCons) + bitSize(t, hashCons) + bitSize(f, hashCons) +
                    summon[Flat[SIRType]].bitSize(tp, hashCons)
            case Builtin(bn, tp)     => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn, hashCons)
            case Error(msg)          => termTagWidth + summon[Flat[String]].bitSize(msg, hashCons)
            case Constr(name, data, args) =>
                termTagWidth + summon[Flat[String]].bitSize(name, hashCons)
                              + summon[Flat[DataDecl]].bitSize(data, hashCons)
                              + listFlat[SIR].bitSize(args, hashCons)
            case Match(scrutinee, cases, tp) =>
                termTagWidth + bitSize(scrutinee, hashCons) + listFlat[Case].bitSize(cases, hashCons)


        def encode(a: SIRExpr, enc: EncoderState): Unit =
            a match
                case v@Var(name, tp) =>
                    enc.bits(termTagWidth, tagVar)
                    summon[Flat[SIR.Var]].encode(v,enc)
                case Let(rec, binds, body) =>
                    enc.bits(termTagWidth, tagLet)
                    summon[Flat[Recursivity]].encode(rec, enc)
                    listFlat[Binding].encode(binds, enc)
                    encode(body, enc)
                case LamAbs(x, t) =>
                    enc.bits(termTagWidth, tagLamAbs)
                    summon[Flat[SIR.Var]].encode(x, enc)
                    encode(t, enc)
                case Apply(f, x, tp) =>
                    enc.bits(termTagWidth, tagApply)
                    encode(f, enc)
                    encode(x, enc)
                    summon[Flat[SIRType]].encode(tp, enc)
                case cn@Const(c, tp) =>
                    enc.bits(termTagWidth, tagConst)
                    summon[Flat[SIR.Const]].encode(cn, enc)
                case IfThenElse(c, t, f, tp) =>
                    enc.bits(termTagWidth, tagIfThenElse)
                    encode(c, enc)
                    encode(t, enc)
                    encode(f, enc)
                    summon[Flat[SIRType]].encode(tp, enc)
                case Builtin(bn, tp) =>
                    enc.bits(termTagWidth, tagBuiltin)
                    summon[Flat[DefaultFun]].encode(bn, enc)
                case Error(msg) =>
                    enc.bits(termTagWidth, tagError)
                    summon[Flat[String]].encode(msg, enc)
                case Constr(name, data, args) =>
                    enc.bits(termTagWidth, tagConstr)
                    summon[Flat[String]].encode(name, enc)
                    summon[Flat[DataDecl]].encode(data, enc)
                    listFlat[SIR].encode(args, enc)
                case Match(scrutinee, cases, tp) =>
                    enc.bits(termTagWidth, tagMatch)
                    encode(scrutinee, enc)
                    listFlat[Case].encode(cases, enc)
                    summon[Flat[SIRType]].encode(tp, enc)
                case ExternalVar(modName, name, tp) =>
                    enc.bits(termTagWidth, tagExternalVar)
                    summon[Flat[String]].encode(modName, enc)
                    summon[Flat[String]].encode(name, enc)
                    summon[Flat[SIRType]].encode(tp, enc)
                case And(x, y) =>
                    enc.bits(termTagWidth, tagAnd)
                    encode(x, enc)
                    encode(y, enc)
                case Or(x, y) =>
                    enc.bits(termTagWidth, tagOr)
                    encode(x, enc)
                    encode(y, enc)
                case Not(x) =>
                    enc.bits(termTagWidth, tagNot)
                    encode(x, enc)

        def decode(decoder: DecoderState): SIRExpr =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case `tagVar` =>
                    summon[Flat[SIR.Var]].decode(decoder)
                case `tagLet` =>
                    val rec = summon[Flat[Recursivity]].decode(decoder)
                    val binds = listFlat[Binding].decode(decoder)
                    val body = decode(decoder)
                    Let(rec, binds, body)
                case `tagLamAbs` =>
                    val x = summon[Flat[Var]].decode(decoder)
                    val t = decode(decoder)
                    LamAbs(x, t)
                case `tagApply` =>
                    val f = decode(decoder)
                    val x = decode(decoder)
                    val tp = summon[Flat[SIRType]].decode(decoder)
                    Apply(f, x, tp)
                case `tagConst` =>
                    summon[Flat[SIR.Const]].decode(decoder)
                case `tagIfThenElse` =>
                    val c = decode(decoder)
                    val t = decode(decoder)
                    val f = decode(decoder)
                    val tp = summon[Flat[SIRType]].decode(decoder)
                    IfThenElse(c, t, f, tp)
                case `tagBuiltin` =>
                    val bn = summon[Flat[DefaultFun]].decode(decoder)
                    SIRBuiltins.fromUplc(bn)
                case `tagError` =>
                    val msg = summon[Flat[String]].decode(decoder)
                    Error(msg)
                case `tagConstr` =>
                    val name = summon[Flat[String]].decode(decoder)
                    val data = summon[Flat[DataDecl]].decode(decoder)
                    val args = listFlat[SIRExpr].decode(decoder)
                    Constr(name, data, args)
                case `tagMatch` =>
                    val scrutinee = decode(decoder)
                    val cases = listFlat[Case].decode(decoder)
                    val tp = summon[Flat[SIRType]].decode(decoder)
                    Match(scrutinee, cases, tp)
                case `tagExternalVar` =>
                    val modName = summon[Flat[String]].decode(decoder)
                    val name = summon[Flat[String]].decode(decoder)
                    val tp = summon[Flat[SIRType]].decode(decoder)
                    ExternalVar(modName, name, tp)
                case `tagAnd` =>
                    val x = decode(decoder)
                    val y = decode(decoder)
                    And(x, y)
                case `tagOr` =>
                    val x = decode(decoder)
                    val y = decode(decoder)
                    Or(x, y)
                case `tagNot` =>
                    val x = decode(decoder)
                    Not(x)

    given Flat[SIR.Var] with
        
        override def bitSize(a: SIR.Var, hashCons: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name, hashCons)
            val tpSize = summon[Flat[SIRType]].bitSize(a.tp, hashCons)
            nameSize + tpSize

        override def encode(a: SIR.Var, encode: EncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode)
            summon[Flat[SIRType]].encode(a.tp, encode)

        override def decode(decode: DecoderState): SIR.Var =
            val name = summon[Flat[String]].decode(decode)
            val tp = summon[Flat[SIRType]].decode(decode)
            SIR.Var(name, tp)
            
            
    
    given Flat[SIRVarStorage] with
        override def bitSize(a: SIRVarStorage, hashConsed: HashConsed.Map): Int =
            summon[Flat[Boolean]].bitSize(a == SIRVarStorage.Data, hashConsed)

        override def decode(decode: DecoderState): SIRVarStorage =
            if summon[Flat[Boolean]].decode(decode) then
                SIRVarStorage.Data
            else SIRVarStorage.LocalUPLC

        override def encode(a: SIRVarStorage, encode: EncoderState): Unit =
            summon[Flat[Boolean]].encode(a == SIRVarStorage.Data, encode)

    given Flat[SIR.Const] with

        override def bitSize(a: SIR.Const, hashCons: HashConsed.Map): Int =
            val constSize = flatConstant.bitSize(a.uplcConst, hashCons)
            val tpSize = summon[Flat[SIRType]].bitSize(a.tp, hashCons)
            tpSize + constSize

        override def encode(a: SIR.Const, encode: EncoderState): Unit =
            flatConstant.encode(a.uplcConst, encode)
            summon[Flat[SIRType]].encode(a.tp, encode)

        override def decode(decode: DecoderState): SIR.Const =
            val uplcConst = flatConstant.decode(decode)
            val tp = summon[Flat[SIRType]].decode(decode)
            SIR.Const(uplcConst, tp)


    given Flat[SIRDef] with



        override def bitSize(a: SIRDef, hashCons: HashConsed.Map): Int =
            a match
                case SIR.Decl(data, term) =>
                  termTagWidth + summon[Flat[DataDecl]].bitSize(data, hashCons) +
                                 summon[Flat[SIR]].bitSize(term, hashCons)


        override def encode(a: SIRDef, encode: EncoderState): Unit =
            a match
                case SIR.Decl(data, term) =>
                    encode.bits(termTagWidth, tagDecl)
                    summon[Flat[DataDecl]].encode(data, encode)
                    summon[Flat[SIR]].encode(term, encode)

        override def decode(decoder: DecoderState): SIRDef =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case `tagDecl` =>
                    val data = summon[Flat[DataDecl]].decode(decoder)
                    val term = summon[Flat[SIR]].decode(decoder)
                    SIR.Decl(data, term)


    given Flat[SIR] with

        override def bitSize(a: SIR, hashCons: HashConsed.Map): Int =
            a match
                case sdf: SIRDef =>
                    summon[Flat[SIRDef]].bitSize(sdf, hashCons)
                case sexpr: SIRExpr =>
                    summon[Flat[SIRExpr]].bitSize(sexpr, hashCons)

        override def encode(a: SIR, encode: EncoderState): Unit =
            a match
                case sdf: SIRDef =>
                    summon[Flat[SIRDef]].encode(sdf, encode)
                case sexpr: SIRExpr =>
                    summon[Flat[SIRExpr]].encode(sexpr, encode)

        override def decode(decode: DecoderState): SIR =
            val tag = decode.lookupBits8(termTagWidth)
            if tag == tagDecl then
                summon[Flat[SIRDef]].decode(decode)
            else summon[Flat[SIRExpr]].decode(decode)

    given Flat[HashConsed.Ref] with
        def bitSize(a: HashConsed.Ref, hashConsed: HashConsed.Map): Int =
            summon[Flat[Int]].bitSize(a.hc, hashConsed)+summon[Flat[Int]].bitSize(a.i, hashConsed)

        def encode(a: HashConsed.Ref, encode: EncoderState): Unit =
            summon[Flat[Int]].encode(a.hc, encode)
            summon[Flat[Int]].encode(a.i, encode)

        def decode(decoder: DecoderState): HashConsed.Ref =
            val hc = summon[Flat[Int]].decode(decoder)
            val i = summon[Flat[Int]].decode(decoder)
            HashConsed.Ref(hc,i)

    given Flat[Module] with
        def bitSize(a: Module, hs: HashConsed.Map): Int = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].bitSize(version, hs) +
                    summon[Flat[List[Binding]]].bitSize(defs, hs)
        def encode(a: Module, enc: EncoderState): Unit = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].encode(version, enc)
                summon[Flat[List[Binding]]].encode(defs, enc)
        def decode(decoder: DecoderState): Module =
            val version = summon[Flat[(Int, Int)]].decode(decoder)
            val defs = summon[Flat[List[Binding]]].decode(decoder)
            Module(version, defs)
