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

        def bitSize(a: Data): Int = a match
            case Data.Constr(constr, args) =>
                width + summon[Flat[Long]].bitSize(constr) + args.map(bitSize(_)).sum
            case Data.Map(values) =>
                width + values.map { case (k, v) => bitSize(k) + bitSize(v) }.sum
            case Data.List(values) => width + values.map(bitSize).sum
            case Data.I(value)     => width + summon[Flat[BigInt]].bitSize(value)
            case Data.B(value)     => width + summon[Flat[builtin.ByteString]].bitSize(value)

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
        def bitSize(a: Recursivity): Int = 1

        def encode(a: Recursivity, encode: EncoderState): Unit =
            encode.bits(1, if a == Recursivity.Rec then 1 else 0)

        def decode(decode: DecoderState): Recursivity =
            if decode.bits8(1) == 1 then Recursivity.Rec else Recursivity.NonRec


    given flatBinding: HashConsedFlat[Binding] with
        def bitSizeHC(a: Binding, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val termSize = summon[HashConsedFlat[SIR]].bitSizeHC(a.value, hashConsed)
            nameSize + termSize
        def encodeHC(a: Binding, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[HashConsedFlat[SIR]].encodeHC(a.value, encode)
        def decodeHC(decode: HashConsedDecoderState): Binding =
            val name = summon[Flat[String]].decode(decode.decode)
            val term = summon[HashConsedFlat[SIRExpr]].decodeHC(decode)
            Binding(name, term)

    given flatConstrDecl:  HashConsedFlat[ConstrDecl] with
        def bitSizeHC(a: ConstrDecl, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val storageTypeSize = summon[Flat[SIRVarStorage]].bitSize(a.storageType)
            val paramsSize = summon[HashConsedFlat[List[TypeBinding]]].bitSizeHC(a.params, hashConsed)
            val typeParamsSize = summon[HashConsedFlat[List[SIRType.TypeVar]]].bitSizeHC(a.typeParams, hashConsed)
            nameSize +  storageTypeSize + paramsSize + typeParamsSize

        def encodeHC(a: ConstrDecl, encode: HashConsedEncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[SIRVarStorage]].encode(a.storageType, encode.encode)
            summon[HashConsedFlat[List[TypeBinding]]].encodeHC(a.params, encode)
            summon[HashConsedFlat[List[SIRType.TypeVar]]].encodeHC(a.typeParams, encode)
            summon[HashConsedFlat[List[SIRType]]].encodeHC(a.parentTypeArgs, encode)
            // TODO: chek and resolve forward references to a
            val _ = encode.hashConsed.putRef(a)
        }

        def decodeHC(decode: HashConsedDecoderState): ConstrDecl = {
            val name = summon[Flat[String]].decode(decode.decode)
            val storageType  = summon[Flat[SIRVarStorage]].decode(decode.decode)
            val params = summon[HashConsedFlat[List[TypeBinding]]].decodeHC(decode)
            val typeParams = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
            val parentTypeArgs = summon[HashConsedFlat[List[SIRType]]].decodeHC(decode)
            ConstrDecl(name, storageType, params, typeParams, parentTypeArgs)
        }

    given flatDataDecl: HashConsedFlat[DataDecl] with
        def bitSizeHC(a: DataDecl, hashConsed: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val constrSize = HashConsedFlat.listHashConsedFlat[ConstrDecl].bitSizeHC(a.constructors, hashConsed)
            val typeParamsSize = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].bitSizeHC(a.typeParams, hashConsed)
            nameSize + constrSize + typeParamsSize
        def encodeHC(a: DataDecl, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            HashConsedFlat.listHashConsedFlat[ConstrDecl].encodeHC(a.constructors, encode)
            HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].encodeHC(a.typeParams, encode)
            encode.hashConsed.putRef(a)
        def decodeHC(decode: HashConsedDecoderState): DataDecl =
            val name = summon[Flat[String]].decode(decode.decode)
            val constr = HashConsedFlat.listHashConsedFlat[ConstrDecl].decodeHC(decode)
            val typeParams = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].decodeHC(decode)
            DataDecl(name, constr, typeParams)

    given flatCase: HashConsedFlat[SIR.Case] with {
        def bitSizeHC(a: SIR.Case, hashConsed: HashConsed.Map): Int =
            val constrSize = summon[HashConsedFlat[ConstrDecl]].bitSizeHC(a.constr, hashConsed)
            val bindings = summon[Flat[List[String]]].bitSize(a.bindings)
            val typeBindings = summon[HashConsedFlat[List[SIRType]]].bitSizeHC(a.typeBindings, hashConsed)
            val bodySize = summon[HashConsedFlat[SIR]].bitSizeHC(a.body, hashConsed)
            constrSize + bindings + typeBindings + bodySize
        def encodeHC(a: SIR.Case, encode: HashConsedEncoderState): Unit = {
            summon[HashConsedFlat[ConstrDecl]].encodeHC(a.constr, encode)
            summon[Flat[List[String]]].encode(a.bindings, encode.encode)
            summon[HashConsedFlat[List[SIRType]]].encodeHC(a.typeBindings, encode)
            summon[HashConsedFlat[SIRExpr]].encodeHC(a.body, encode)
        }
        def decodeHC(decode: HashConsedDecoderState): SIR.Case = {
            val constr = summon[HashConsedFlat[ConstrDecl]].decodeHC(decode)
            val bindings = summon[Flat[List[String]]].decode(decode.decode)
            val typeBindings = summon[HashConsedFlat[List[SIRType]]].decodeHC(decode)
            val body = summon[HashConsedFlat[SIRExpr]].decodeHC(decode)
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
    
    given HashConsedFlat[SIRType] with

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

        override def bitSizeHC(a: SIRType, hashConsed: HashConsed.Map): Int =
            a match
                case _: SIRType.Primitive[?] => tagWidth
                case SIRType.Data => tagWidth
                case SIRType.CaseClass(constrDecl, typeParams) =>
                    hashConsed.lookup(a) match
                        case Some(ref) =>
                            tagWidth + summon[HashConsedFlat[HashConsed.Ref]].bitSizeHC(ref, hashConsed)
                        case None =>
                            hashConsed.lookup(constrDecl) match
                                case Some(ref) =>
                                    tagWidth + summon[HashConsedFlat[HashConsed.Ref]].bitSizeHC(ref, hashConsed)
                                case None =>
                                    val constrSize = summon[HashConsedFlat[ConstrDecl]].bitSizeHC(constrDecl, hashConsed)
                                    tagWidth + constrSize
                case SIRType.SumCaseClass(dataDecl, typeParams) =>
                    hashConsed.lookup(a) match
                        case Some(ref) =>
                            tagWidth + summon[Flat[HashConsed.Ref]].bitSizeHC(ref, hashConsed)
                        case None =>
                            hashConsed.lookup(dataDecl) match
                                case Some(ref) =>
                                    tagWidth + summon[Flat[HashConsed.Ref]].bitSizeHC(ref, hashConsed)
                                case None =>
                                    val dataDeclSize = summon[Flat[DataDecl]].bitSizeHC(dataDecl, hashConsed)
                                    tagWidth + dataDeclSize

        override def encodeHC(a: SIRType, encode: HashConsedEncoderState): Unit = ???

        override def decodeHC(decode: HashConsedDecoderState): SIRType = ???
                                    
                                    

    given HashConsedFlat[TypeBinding] with

        override def bitSizeHC(a: TypeBinding, hashCons: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val tpSize = summon[HashConsedFlat[SIRType]].bitSizeHC(a.tp, hashCons)
            nameSize + tpSize

        override def encodeHC(a: TypeBinding, encode: HashConsedEncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[HashConsedFlat[SIRType]].encodeHC(a.tp, encode)
        }

        override def decodeHC(decode: HashConsedDecoderState): TypeBinding = {
            val name = summon[Flat[String]].decode(decode.decode)
            val tp = summon[HashConsedFlat[SIRType]].decodeHC(decode)
            TypeBinding(name, tp)
        }

    given HashConsedFlat[SIRType.TypeVar] with
        override def bitSizeHC(a: SIRType.TypeVar, hashCons: HashConsed.Map): Int =
            summon[Flat[String]].bitSize(a.name)

        override def encodeHC(a: SIRType.TypeVar, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)

        override def decodeHC(decode: HashConsedDecoderState): SIRType.TypeVar =
            val name = summon[Flat[String]].decode(decode.decode)
            SIRType.TypeVar(name)

    given HashConsedFlat[SIRExpr] with
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

        def bitSizeHC(a: SIRExpr, hashCons: HashConsed.Map): Int = a match
            case Var(name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(name)
            case ExternalVar(modName, name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(modName)
                             + summon[Flat[String]].bitSize(name)
                             + summon[Flat[SIRType]].bitSizeHC(tp, hashCons)
            case Let(rec, binds, body) =>
                termTagWidth + summon[Flat[Recursivity]].bitSize(rec) +
                    summon[HashConsedFlat[List[Binding]]].bitSizeHC(binds, hashCons) + bitSizeHC(body, hashCons)
            case LamAbs(x, t)        =>
                termTagWidth + bitSizeHC(x,hashCons) + bitSizeHC(t, hashCons)
            case Apply(f, x, tp)     =>
                termTagWidth + bitSizeHC(f,hashCons) + bitSizeHC(x, hashCons) +
                    summon[HashConsedFlat[SIRType]].bitSizeHC(tp, hashCons)
            case Const(c, tp)        => termTagWidth + flatConstant.bitSize(c) +
                                                        summon[HashConsedFlat[SIRType]].bitSizeHC(tp, hashCons)
            case And(x, y)           => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Or(x, y)            => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Not(x)              => termTagWidth + bitSizeHC(x, hashCons)
            case IfThenElse(c, t, f, tp) =>
                termTagWidth + bitSizeHC(c, hashCons) + bitSizeHC(t, hashCons) + bitSizeHC(f, hashCons) +
                    summon[HashConsedFlat[SIRType]].bitSizeHC(tp, hashCons)
            case Builtin(bn, tp)     => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error(msg, cause)   => termTagWidth + summon[Flat[String]].bitSize(msg)
            case Constr(name, data, args) =>
                termTagWidth + summon[Flat[String]].bitSize(name)
                              + summon[HashConsedFlat[DataDecl]].bitSizeHC(data, hashCons)
                              + summon[HashConsedFlat[List[SIR]]].bitSizeHC(args, hashCons)
            case Match(scrutinee, cases, tp) =>
                termTagWidth + bitSizeHC(scrutinee, hashCons)
                    + summon[HashConsedFlat[List[Case]]].bitSizeHC(cases, hashCons)


        def encodeHC(a: SIRExpr, enc: HashConsedEncoderState): Unit =
            a match
                case v@Var(name, tp) =>
                    enc.encode.bits(termTagWidth, tagVar)
                    summon[HashConsedFlat[SIR.Var]].encodeHC(v,enc)
                case Let(rec, binds, body) =>
                    enc.encode.bits(termTagWidth, tagLet)
                    summon[Flat[Recursivity]].encode(rec, enc.encode)
                    summon[HashConsedFlat[List[Binding]]].encodeHC(binds, enc)
                    encodeHC(body, enc)
                case LamAbs(x, t) =>
                    enc.encode.bits(termTagWidth, tagLamAbs)
                    summon[HashConsedFlat[SIR.Var]].encodeHC(x, enc)
                    encodeHC(t, enc)
                case Apply(f, x, tp) =>
                    enc.encode.bits(termTagWidth, tagApply)
                    encodeHC(f, enc)
                    encodeHC(x, enc)
                    summon[HashConsedFlat[SIRType]].encodeHC(tp, enc)
                case cn@Const(c, tp) =>
                    enc.encode.bits(termTagWidth, tagConst)
                    summon[HashConsedFlat[SIR.Const]].encodeHC(cn, enc)
                case IfThenElse(c, t, f, tp) =>
                    enc.encode.bits(termTagWidth, tagIfThenElse)
                    encodeHC(c, enc)
                    encodeHC(t, enc)
                    encodeHC(f, enc)
                    summon[HashConsedFlat[SIRType]].encodeHC(tp, enc)
                case Builtin(bn, tp) =>
                    enc.encode.bits(termTagWidth, tagBuiltin)
                    summon[Flat[DefaultFun]].encode(bn, enc.encode)
                case Error(msg, _) =>
                    enc.encode.bits(termTagWidth, tagError)
                    summon[Flat[String]].encode(msg, enc.encode)
                case Constr(name, data, args) =>
                    enc.encode.bits(termTagWidth, tagConstr)
                    summon[Flat[String]].encode(name, enc.encode)
                    summon[HashConsedFlat[DataDecl]].encodeHC(data, enc)
                    summon[HashConsedFlat[List[SIR]]].encodeHC(args, enc)
                case Match(scrutinee, cases, tp) =>
                    enc.encode.bits(termTagWidth, tagMatch)
                    encodeHC(scrutinee, enc)
                    summon[HashConsedFlat[List[Case]]].encodeHC(cases, enc)
                    summon[HashConsedFlat[SIRType]].encodeHC(tp, enc)
                case ExternalVar(modName, name, tp) =>
                    enc.encode.bits(termTagWidth, tagExternalVar)
                    summon[Flat[String]].encode(modName, enc.encode)
                    summon[Flat[String]].encode(name, enc.encode)
                    summon[HashConsedFlat[SIRType]].encodeHC(tp, enc)
                case And(x, y) =>
                    enc.encode.bits(termTagWidth, tagAnd)
                    encodeHC(x, enc)
                    encodeHC(y, enc)
                case Or(x, y) =>
                    enc.encode.bits(termTagWidth, tagOr)
                    encodeHC(x, enc)
                    encodeHC(y, enc)
                case Not(x) =>
                    enc.encode.bits(termTagWidth, tagNot)
                    encodeHC(x, enc)

        def decodeHC(decoder: HashConsedDecoderState): SIRExpr =
            val tag = decoder.decode.bits8(termTagWidth)
            tag match
                case `tagVar` =>
                    summon[HashConsedFlat[SIR.Var]].decodeHC(decoder)
                case `tagLet` =>
                    val rec = summon[Flat[Recursivity]].decode(decoder.decode)
                    val binds = summon[HashConsedFlat[List[Binding]]].decodeHC(decoder)
                    val body = decodeHC(decoder)
                    Let(rec, binds, body)
                case `tagLamAbs` =>
                    val x = summon[HashConsedFlat[Var]].decodeHC(decoder)
                    val t = decodeHC(decoder)
                    LamAbs(x, t)
                case `tagApply` =>
                    val f = decodeHC(decoder)
                    val x = decodeHC(decoder)
                    val tp = summon[HashConsedFlat[SIRType]].decodeHC(decoder)
                    Apply(f, x, tp)
                case `tagConst` =>
                    summon[HashConsedFlat[SIR.Const]].decodeHC(decoder)
                case `tagIfThenElse` =>
                    val c = decodeHC(decoder)
                    val t = decodeHC(decoder)
                    val f = decodeHC(decoder)
                    val tp = summon[HashConsedFlat[SIRType]].decodeHC(decoder)
                    IfThenElse(c, t, f, tp)
                case `tagBuiltin` =>
                    val bn = summon[Flat[DefaultFun]].decode(decoder.decode)
                    SIRBuiltins.fromUplc(bn)
                case `tagError` =>
                    val msg = summon[Flat[String]].decode(decoder.decode)
                    Error(msg)
                case `tagConstr` =>
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val data = summon[HashConsedFlat[DataDecl]].decodeHC(decoder)
                    val args = summon[HashConsedFlat[List[SIRExpr]]].decodeHC(decoder)
                    Constr(name, data, args)
                case `tagMatch` =>
                    val scrutinee = decodeHC(decoder)
                    val cases = summon[HashConsedFlat[List[Case]]].decodeHC(decoder)
                    val tp = summon[HashConsedFlat[SIRType]].decodeHC(decoder)
                    Match(scrutinee, cases, tp)
                case `tagExternalVar` =>
                    val modName = summon[Flat[String]].decode(decoder.decode)
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val tp = summon[HashConsedFlat[SIRType]].decodeHC(decoder)
                    ExternalVar(modName, name, tp)
                case `tagAnd` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    And(x, y)
                case `tagOr` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    Or(x, y)
                case `tagNot` =>
                    val x = decodeHC(decoder)
                    Not(x)

    given HashConsedFlat[SIR.Var] with
        
        override def bitSizeHC(a: SIR.Var, hashCons: HashConsed.Map): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val tpSize = summon[HashConsedFlat[SIRType]].bitSizeHC(a.tp, hashCons)
            nameSize + tpSize

        override def encodeHC(a: SIR.Var, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[HashConsedFlat[SIRType]].encodeHC(a.tp, encode)

        override def decodeHC(decode: HashConsedDecoderState): SIR.Var =
            val name = summon[Flat[String]].decode(decode.decode)
            val tp = summon[HashConsedFlat[SIRType]].decodeHC(decode)
            SIR.Var(name, tp)
            
            
    
    given Flat[SIRVarStorage] with
        override def bitSize(a: SIRVarStorage): Int =
            summon[Flat[Boolean]].bitSize(a == SIRVarStorage.Data)

        override def decode(decode: DecoderState): SIRVarStorage =
            if summon[Flat[Boolean]].decode(decode) then
                SIRVarStorage.Data
            else SIRVarStorage.LocalUPLC

        override def encode(a: SIRVarStorage, encode: EncoderState): Unit =
            summon[Flat[Boolean]].encode(a == SIRVarStorage.Data, encode)

    given HashConsedFlat[SIR.Const] with

        override def bitSizeHC(a: SIR.Const, hashCons: HashConsed.Map): Int =
            val constSize = flatConstant.bitSize(a.uplcConst)
            val tpSize = summon[HashConsedFlat[SIRType]].bitSizeHC(a.tp, hashCons)
            tpSize + constSize

        override def encodeHC(a: SIR.Const, encode: HashConsedEncoderState): Unit =
            flatConstant.encode(a.uplcConst, encode.encode)
            summon[HashConsedFlat[SIRType]].encodeHC(a.tp, encode)

        override def decodeHC(decode: HashConsedDecoderState): SIR.Const =
            val uplcConst = flatConstant.decode(decode.decode)
            val tp = summon[HashConsedFlat[SIRType]].decodeHC(decode)
            SIR.Const(uplcConst, tp)


    given HashConsedFlat[SIRDef] with

        override def bitSizeHC(a: SIRDef, hashCons: HashConsed.Map): Int =
            a match
                case SIR.Decl(data, term) =>
                  termTagWidth + summon[HashConsedFlat[DataDecl]].bitSizeHC(data, hashCons) +
                                 summon[HashConsedFlat[SIR]].bitSizeHC(term, hashCons)

        override def encodeHC(a: SIRDef, encode: HashConsedEncoderState): Unit =
            a match
                case SIR.Decl(data, term) =>
                    encode.encode.bits(termTagWidth, tagDecl)
                    summon[HashConsedFlat[DataDecl]].encodeHC(data, encode)
                    summon[HashConsedFlat[SIR]].encodeHC(term, encode)

        override def decodeHC(decoder: HashConsedDecoderState): SIRDef =
            val tag = decoder.decode.bits8(termTagWidth)
            tag match
                case `tagDecl` =>
                    val data = summon[HashConsedFlat[DataDecl]].decodeHC(decoder)
                    val term = summon[HashConsedFlat[SIR]].decodeHC(decoder)
                    SIR.Decl(data, term)


    given HashConsedFlat[SIR] with

        override def bitSizeHC(a: SIR, hashCons: HashConsed.Map): Int =
            a match
                case sdf: SIRDef =>
                    summon[HashConsedFlat[SIRDef]].bitSizeHC(sdf, hashCons)
                case sexpr: SIRExpr =>
                    summon[HashConsedFlat[SIRExpr]].bitSizeHC(sexpr, hashCons)

        override def encodeHC(a: SIR, encode: HashConsedEncoderState): Unit =
            a match
                case sdf: SIRDef =>
                    summon[HashConsedFlat[SIRDef]].encodeHC(sdf, encode)
                case sexpr: SIRExpr =>
                    summon[HashConsedFlat[SIRExpr]].encodeHC(sexpr, encode)

        override def decodeHC(decode: HashConsedDecoderState): SIR =
            val tag = decode.decode.lookupBits8(termTagWidth)
            if tag == tagDecl then
                summon[HashConsedFlat[SIRDef]].decodeHC(decode)
            else summon[HashConsedFlat[SIRExpr]].decodeHC(decode)

    given HashConsedFlat[HashConsed.Ref] with
        def bitSizeHC(a: HashConsed.Ref, hashConsed: HashConsed.Map): Int =
            summon[Flat[Int]].bitSize(a.hc)+summon[Flat[Int]].bitSize(a.i)

        def encodeHC(a: HashConsed.Ref, encode: HashConsedEncoderState): Unit =
            summon[Flat[Int]].encode(a.hc, encode.encode)
            summon[Flat[Int]].encode(a.i, encode.encode)

        def decodeHC(decoder: HashConsedDecoderState): HashConsed.Ref =
            val hc = summon[Flat[Int]].decode(decoder.decode)
            val i = summon[Flat[Int]].decode(decoder.decode)
            HashConsed.Ref(hc,i)

    given HashConsedFlat[Module] with
        def bitSizeHC(a: Module, hs: HashConsed.Map): Int = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].bitSize(version) +
                    summon[HashConsedFlat[List[Binding]]].bitSizeHC(defs, hs)
        def encodeHC(a: Module, enc: HashConsedEncoderState): Unit = a match
            case Module(version, defs) =>
                summon[Flat[(Int, Int)]].encode(version, enc.encode)
                summon[HashConsedFlat[List[Binding]]].encode(defs, enc.encode)
        def decodeHC(decoder: HashConsedDecoderState): Module =
            val version = summon[Flat[(Int, Int)]].decode(decoder.decode)
            val defs = summon[HashConsedFlat[List[Binding]]].decodeHC(decoder)
            Module(version, defs)
