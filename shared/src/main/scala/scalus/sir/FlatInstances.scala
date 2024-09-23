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
import scalus.utils.HashConsedRead.Tag

import java.util.concurrent.atomic.AtomicReference

object FlatInstantces:
    //val termTagWidth = 4

    final val hashConsTagSIRType = HashConsedRead.tag(0x01)
    final val hashConsTagDecl = HashConsedRead.tag(0x02)
    final val hashConsTagConstr = HashConsedRead.tag(0x03)


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

        def bitSizeHC(a: Binding, hashConsed: HashConsedRead.State): Int =
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

    given flatConstrDecl:  HashConsedNonRecReferenceFlat[ConstrDecl] with
        
        def tag = hashConsTagConstr

        def bitSizeHCNew(a: ConstrDecl, hashConsed: HashConsedRead.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val storageTypeSize = summon[Flat[SIRVarStorage]].bitSize(a.storageType)
            val paramsSize = summon[HashConsedFlat[List[TypeBinding]]].bitSizeHC(a.params, hashConsed)
            val typeParamsSize = summon[HashConsedFlat[List[SIRType.TypeVar]]].bitSizeHC(a.typeParams, hashConsed)
            nameSize +  storageTypeSize + paramsSize + typeParamsSize

        def encodeHCNew(a: ConstrDecl, encode: HashConsedEncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[SIRVarStorage]].encode(a.storageType, encode.encode)
            summon[HashConsedFlat[List[TypeBinding]]].encodeHC(a.params, encode)
            summon[HashConsedFlat[List[SIRType.TypeVar]]].encodeHC(a.typeParams, encode)
            summon[HashConsedFlat[List[SIRType]]].encodeHC(a.parentTypeArgs, encode)
        }

        def decodeHCNew(decode: HashConsedDecoderState): ConstrDecl = {
            val name = summon[Flat[String]].decode(decode.decode)
            val storageType  = summon[Flat[SIRVarStorage]].decode(decode.decode)
            val params = summon[HashConsedFlat[List[TypeBinding]]].decodeHC(decode)
            val typeParams = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
            val parentTypeArgs = summon[HashConsedFlat[List[SIRType]]].decodeHC(decode)
            ConstrDecl(name, storageType, params, typeParams, parentTypeArgs)
        }




    given flatDataDeclRef: HashConsedMutRefFlat[DataDecl] with
        
        def tag = hashConsTagDecl

        def bitSizeHCNew(a: DataDecl, encoderState: HashConsedRead.State): Int =
              val nameSize = summon[Flat[String]].bitSize(a.name)
              val constrSize = HashConsedFlat.listHashConsedFlat[ConstrDecl].bitSizeHC(a.constructors, encoderState)
              val typeParamsSize = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].bitSizeHC(a.typeParams, encoderState)
              nameSize + constrSize + typeParamsSize

        def encodeHCNew(a: DataDecl, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            HashConsedFlat.listHashConsedFlat[ConstrDecl].encodeHC(a.constructors, encode)
            HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].encodeHC(a.typeParams, encode)

        def decodeHCNew(decode: HashConsedDecoderState): DataDecl =
            val name = summon[Flat[String]].decode(decode.decode)
            val constr = HashConsedFlat.listHashConsedFlat[ConstrDecl].decodeHC(decode)
            val typeParams = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].decodeHC(decode)
            DataDecl(name, constr, typeParams)

    given flatCase: HashConsedFlat[SIR.Case] with {

        def bitSizeHC(a: SIR.Case, hashConsed: HashConsedRead.State): Int =
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

        //  Types
        val tagPrimitiveByteString: Byte = 0x00
        val tagPrimitiveInteger:Byte = 0x01
        val tagPrimitiveString: Byte = 0x02
        val tagPrimitiveBoolean: Byte = 0x03
        val tagPrimitiveVoid: Byte = 0x04
        val tagPrimitiveData: Byte = 0x05
        val tagCaseClass: Byte = 0x06
        val tagSumCaseClass: Byte = 0x07
        val tagFun: Byte = 0x08
        val tagTypeVar: Byte = 0x09
        val tagTypeLambda: Byte = 0x0A
        val tagTypeFreeUnificator: Byte = 0x0B
        val tagTypeProxy: Byte = 0x0C
        val tagTypeError: Byte = 0x0D
        val tagTypeNothing: Byte = 0x0E

        //

        override def bitSizeHC(a: SIRType, hashConsed: HashConsedRead.State): Int =
            val retval = a match
                case _: SIRType.Primitive[?] => tagWidth
                case SIRType.Data => tagWidth
                case cc: SIRType.CaseClass =>
                    tagWidth + summon[HashConsedReferencedFlat[SIRType.CaseClass, SIRType]].bitSizeHC(cc, hashConsed)
                case scc:SIRType.SumCaseClass =>
                    tagWidth + summon[HashConsedReferencedFlat[SIRType.SumCaseClass, SIRType]].bitSizeHC(scc, hashConsed)
                case fun: SIRType.Fun =>
                    tagWidth + summon[HashConsedReferencedFlat[SIRType.Fun, SIRType]].bitSizeHC(fun, hashConsed)
                case SIRType.TypeVar(name, optId) =>
                    tagWidth + summon[HashConsedFlat[SIRType.TypeVar]].bitSizeHC(a.asInstanceOf[SIRType.TypeVar], hashConsed)
                case SIRType.TypeLambda(params, body) =>
                    tagWidth + summon[HashConsedFlat[List[SIRType.TypeVar]]].bitSizeHC(params, hashConsed) +
                        bitSizeHC(body, hashConsed)
                case SIRType.FreeUnificator =>
                    tagWidth
                case SIRType.TypeProxy(ref) =>
                    if (ref == null) 
                        throw new IllegalStateException("TypeProxy id is null, looks lise save or restore is invalid")
                    println(s"encode proxy, ref=${ref}")
                    tagWidth + PlainIntFlat.bitSize(ref.hashCode())
                case err:SIRType.TypeError =>
                    tagWidth + summon[Flat[String]].bitSize(err.msg)
                case SIRType.TypeNothing => tagWidth
            println(s"bisSizeHC($a)=${retval}")
            retval


        override def encodeHC(a: SIRType, encode: HashConsedEncoderState): Unit =
            val startPos = encode.encode.bitPosition()
            a match
                case SIRType.ByteStringPrimitive => encode.encode.bits(tagWidth, tagPrimitiveByteString)
                case SIRType.IntegerPrimitive => encode.encode.bits(tagWidth, tagPrimitiveInteger)
                case SIRType.StringPrimitive => encode.encode.bits(tagWidth, tagPrimitiveString)
                case SIRType.BooleanPrimitive => encode.encode.bits(tagWidth, tagPrimitiveBoolean)
                case SIRType.VoidPrimitive => encode.encode.bits(tagWidth, tagPrimitiveVoid)
                case SIRType.Data => encode.encode.bits(tagWidth, tagPrimitiveData)
                case cc: SIRType.CaseClass => 
                    encode.encode.bits(tagWidth, tagCaseClass)
                    summon[HashConsedReferencedFlat[SIRType.CaseClass, SIRType.TypeProxy, SIRType]].encodeHC(cc, encode)
                case scc: SIRType.SumCaseClass =>
                    encode.encode.bits(tagWidth, tagSumCaseClass)
                    summon[HashConsedReferencedFlat[SIRType.SumCaseClass, SIRType.TypeProxy, SIRType]].encodeHC(scc, encode)
                case fun@SIRType.Fun(from, to) =>
                    encode.encode.bits(tagWidth, tagFun)
                    summon[HashConsedReferencedFlat[SIRType.Fun, SIRType.TypeProxy, SIRType]].encodeHC(fun, encode)
                case tv: SIRType.TypeVar =>
                    encode.encode.bits(tagWidth, tagTypeVar)
                    summon[HashConsedFlat[SIRType.TypeVar]].encodeHC(tv, encode)
                case SIRType.TypeLambda(params, body) =>
                    encode.encode.bits(tagWidth, tagTypeLambda)
                    summon[HashConsedFlat[List[SIRType.TypeVar]]].encodeHC(params, encode)
                    encodeHC(body, encode)
                case SIRType.FreeUnificator =>
                    encode.encode.bits(tagWidth, tagTypeFreeUnificator)
                case SIRType.TypeProxy(ref) =>
                    encode.encode.bits(tagWidth, tagTypeProxy)
                    val ihc = ref.hashCode()
                    PlainIntFlat.encode(ihc, encode.encode)
                case err: SIRType.TypeError =>
                    encode.encode.bits(tagWidth, tagTypeError)
                    summon[Flat[String]].encode(err.msg, encode.encode)
                case SIRType.TypeNothing =>
                    encode.encode.bits(tagWidth, tagTypeNothing)
            val endPos = encode.encode.bitPosition()
            println(s"encode $a,  size=${endPos-startPos}")
                    
                

        override def decodeHC(decode: HashConsedDecoderState): SIRType =
            val tag: Byte = decode.decode.bits8(tagWidth)
            tag match 
                case `tagPrimitiveByteString` => SIRType.ByteStringPrimitive
                case `tagPrimitiveInteger` => SIRType.IntegerPrimitive
                case `tagPrimitiveString` => SIRType.StringPrimitive
                case `tagPrimitiveBoolean` => SIRType.BooleanPrimitive
                case `tagPrimitiveVoid` => SIRType.VoidPrimitive
                case `tagPrimitiveData` => SIRType.Data
                case `tagCaseClass` => summon[HashConsedReferencedFlat[SIRType.CaseClass]].decodeHC(decode)
                case `tagSumCaseClass` => summon[HashConsedReferencedFlat[SIRType.SumCaseClass]].decodeHC(decode)
                case `tagFun` =>
                    summon[HashConsedReferencedFlat[SIRType.Fun]].decodeHC(decode)
                case `tagTypeVar` => summon[HashConsedFlat[SIRType.TypeVar]].decodeHC(decode)
                case `tagTypeLambda` =>
                    val params = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
                    val body = decodeHC(decode)
                    SIRType.TypeLambda(params, body)
                case `tagTypeFreeUnificator` => SIRType.FreeUnificator
                case `tagTypeProxy` =>
                    //val ihc = summon[Flat[Int]].decode(decode.decode)
                    val ihc = PlainIntFlat.decode(decode.decode)
                    decode.hashConsed.lookupValue(ihc, hashConsTagSIRType) match
                        case None => 
                            val retval = new SIRType.TypeProxy(null)
                            decode.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, hashConsTagSIRType, 
                                List((a: AnyRef) => retval.ref = a.asInstanceOf[SIRType]))
                            )
                            // TODO: create something like check step, to ensure all proxies are resolved.
                            retval
                        case Some(a) => a.asInstanceOf[SIRType]
                case `tagTypeError` =>
                    val msg = summon[Flat[String]].decode(decode.decode)
                    SIRType.TypeError(msg, null)
                case `tagTypeNothing` => SIRType.TypeNothing
                case _ => throw new IllegalStateException(s"Invalid SIRType tag: $tag")
                


    given HashConsedFlat[TypeBinding] with

        override def bitSizeHC(a: TypeBinding, hashCons: HashConsedRead.State): Int =
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

        override def bitSizeHC(a: SIRType.TypeVar, hashCons: HashConsedRead.State): Int =
            summon[Flat[String]].bitSize(a.name) + summon[Flat[Long]].bitSize(a.optId.getOrElse(0))

        override def encodeHC(a: SIRType.TypeVar, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[Long]].encode(a.optId.getOrElse(0L), encode.encode)

        override def decodeHC(decode: HashConsedDecoderState): SIRType.TypeVar =
            val name = summon[Flat[String]].decode(decode.decode)
            val optId = summon[Flat[Long]].decode(decode.decode) match
                case 0 => None
                case id => Some(id)
            SIRType.TypeVar(name, optId)

    trait HashConsedReferencedSIRTypeFlat[A <: SIRType] extends HashConsedReferencedFlat[A, SIRType.TypeProxy, SIRType] {

        override def tag: Tag = hashConsTagSIRType

        override def makeProxy: Proxy = new SIRType.TypeProxy(null)

        override def setRef(proxy: Proxy, a: SIRType): Unit =
            proxy.ref = a

    }

    given HashConsedReferencedSIRTypeFlat[SIRType.CaseClass] with

        override def tag: Tag = hashConsTagSIRType

        override def bitSizeHCNew(a: SIRType.CaseClass, encode: HashConsedRead.State): Int =
            val constrDeclSize = summon[HashConsedFlat[ConstrDecl]].bitSizeHC(a.constrDecl, encode)
            val typeArgsSize = summon[HashConsedFlat[List[SIRType]]].bitSizeHC(a.typeArgs, encode)
            constrDeclSize + typeArgsSize

        override def encodeHCNew(a: SIRType.CaseClass, encode: HashConsedEncoderState): Unit =
            summon[HashConsedFlat[ConstrDecl]].encodeHC(a.constrDecl, encode)
            summon[HashConsedFlat[List[SIRType]]].encodeHC(a.typeArgs, encode)

        override def decodeHCNew(decode: HashConsedDecoderState): SIRType.CaseClass =
            val constrDecl = summon[HashConsedFlat[ConstrDecl]].decodeHC(decode)
            val typeArgs = summon[HashConsedFlat[List[SIRType]]].decodeHC(decode)
            SIRType.CaseClass(constrDecl, typeArgs)


    given HashConsedReferencedSIRTyoeFlat[SIRType.SumCaseClass] with

        override def bitSizeHCNew(a: SIRType.SumCaseClass, encode: HashConsedRead.State): Int =
            val declSize = summon[HashConsedFlat[DataDecl]].bitSizeHC(a.decl, encode)
            val typeArgsSize = summon[HashConsedFlat[List[SIRType]]].bitSizeHC(a.typeArgs, encode)
            declSize + typeArgsSize

        override def encodeHCNew(a: SIRType.SumCaseClass, encode: HashConsedEncoderState): Unit =
            summon[HashConsedFlat[DataDecl]].encodeHC(a.decl, encode)
            summon[HashConsedFlat[List[SIRType]]].encodeHC(a.typeArgs, encode)

        override def decodeHCNew(decode: HashConsedDecoderState): SIRType.SumCaseClass =
            val decl = summon[HashConsedFlat[DataDecl]].decodeHC(decode)
            val typeArgs = summon[HashConsedFlat[List[SIRType]]].decodeHC(decode)
            SIRType.SumCaseClass(decl, typeArgs)

    given HashConsedReferencedSIRTypeFlat[SIRType.Fun] with

        override def bitSizeHCNew(a: SIRType.Fun, encode: HashConsedRead.State): Int =
            val fromSize = summon[HashConsedFlat[SIRType]].bitSizeHC(a.in, encode)
            val toSize = summon[HashConsedFlat[SIRType]].bitSizeHC(a.out, encode)
            fromSize + toSize

        override def encodeHCNew(a: SIRType.Fun, encode: HashConsedEncoderState): Unit =
            summon[HashConsedFlat[SIRType]].encodeHC(a.in, encode)
            summon[HashConsedFlat[SIRType]].encodeHC(a.out, encode)

        override def decodeHCNew(decode: HashConsedDecoderState): SIRType.Fun =
            val from = summon[HashConsedFlat[SIRType]].decodeHC(decode)
            val to = summon[HashConsedFlat[SIRType]].decodeHC(decode)
            SIRType.Fun(from, to)
            
            

    given HashConsedFlat[SIRExpr] with
        import SIR.*

        final val termTagWidth = 4

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

        def bitSizeHC(a: SIRExpr, hashCons: HashConsedRead.State): Int = a match
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
        
        override def bitSizeHC(a: SIR.Var, hashCons: HashConsedRead.State): Int =
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

        override def bitSizeHC(a: SIR.Const, hashCons: HashConsedRead.State): Int =
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

        override def bitSizeHC(a: SIRDef, hashCons: HashConsedRead.State): Int =
            a match
                case SIR.Decl(data, term) =>
                  summon[HashConsedFlat[DataDecl]].bitSizeHC(data, hashCons) +
                      summon[HashConsedFlat[SIR]].bitSizeHC(term, hashCons)

        override def encodeHC(a: SIRDef, encode: HashConsedEncoderState): Unit =
            a match
                case SIR.Decl(data, term) =>
                    summon[HashConsedFlat[DataDecl]].encodeHC(data, encode)
                    summon[HashConsedFlat[SIR]].encodeHC(term, encode)

        override def decodeHC(decoder: HashConsedDecoderState): SIRDef =
             val data = summon[HashConsedFlat[DataDecl]].decodeHC(decoder)
             val term = summon[HashConsedFlat[SIR]].decodeHC(decoder)
             SIR.Decl(data, term)


    given HashConsedFlat[SIR] with

        final val tagSIRDef = 0x00
        final val tagSIRExpr = 0x01

        final val sirTagWidth = 1

        override def bitSizeHC(a: SIR, hashCons: HashConsedRead.State): Int =
            a match
                case sdf: SIRDef =>
                    1 + summon[HashConsedFlat[SIRDef]].bitSizeHC(sdf, hashCons)
                case sexpr: SIRExpr =>
                    1 + summon[HashConsedFlat[SIRExpr]].bitSizeHC(sexpr, hashCons)

        override def encodeHC(a: SIR, encode: HashConsedEncoderState): Unit =
            a match
                case sdf: SIRDef =>
                    encode.encode.bits(sirTagWidth, tagSIRDef)
                    summon[HashConsedFlat[SIRDef]].encodeHC(sdf, encode)
                case sexpr: SIRExpr =>
                    encode.encode.bits(sirTagWidth, tagSIRExpr)
                    summon[HashConsedFlat[SIRExpr]].encodeHC(sexpr, encode)

        override def decodeHC(decode: HashConsedDecoderState): SIR =
            decode.decode.bits8(1) match
                case `tagSIRDef` =>
                    summon[HashConsedFlat[SIRDef]].decodeHC(decode)
                case `tagSIRExpr` =>
                    summon[HashConsedFlat[SIRExpr]].decodeHC(decode)


    given HashConsedFlat[Module] with
        def bitSizeHC(a: Module, hs: HashConsedRead.State): Int = a match
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
