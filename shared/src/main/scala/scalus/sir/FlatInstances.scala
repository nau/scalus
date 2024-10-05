package scalus.flat

import scalus.{builtin, flat}
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
    //val termTagWidth = 4

    final val hashConsTagSIRType = HashConsed.tag(0x01)
    final val hashConsTagDecl = HashConsed.tag(0x02)
    final val hashConsTagConstr = HashConsed.tag(0x03)
    final val hashConsTagBinging = HashConsed.tag(0x04)
    final val hashConsTagCase = HashConsed.tag(0x05)
    final val hashConsTagLet = HashConsed.tag(0x06)
    final val hashConsTagSIRExpr = HashConsed.tag(0x07)
    final val hashConsTagTypeBinding = HashConsed.tag(0x08)


    given Flat[Data] with
        private val width = 3

        def bitSize(a: Data): Int = a match
            case Data.Constr(constr, args) =>
                width + summon[Flat[Long]].bitSize(constr) + summon[Flat[List[Data]]].bitSize(args)
            case Data.Map(values) =>
                width + summon[Flat[List[(Data,Data)]]].bitSize(values)
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
                    summon[Flat[List[(Data, Data)]]].encode(values, enc)
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


    case class BindingSerializedRepr(name: String, value: HashConsedRef[SIRExpr]) extends HashConsedRef[Binding] :

        override def isComplete(hashConsed: HashConsed.State): Boolean = value.isComplete(hashConsed)

        override def finValue(hashConsed: HashConsed.State): Binding = {
            val restoredValue = value.finValue(hashConsed)
            Binding(name, restoredValue)
        }

    end BindingSerializedRepr

    object BindingFlat extends HashConsedReprFlat[Binding, BindingSerializedRepr]:

        def tag = hashConsTagBinging

        override def toRepr(a: Binding): BindingSerializedRepr = {
            val value = HashConsed.MutRef.fromData(a.value)
            BindingSerializedRepr(a.name, value)
        }

        def bitSizeHC(a: Binding, hashConsed: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val termSize = SIRExprHashConsedFlat.bitSizeHC(a.value, hashConsed)
            nameSize + termSize

        def encodeHC(a: Binding, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            SIRExprHashConsedFlat.encodeHC(a.value, encode)

        def decodeHC(decode: HashConsedDecoderState): BindingSerializedRepr =
            val name = summon[Flat[String]].decode(decode.decode)
            val termRef = SIRExprHashConsedFlat.decodeHC(decode)
            BindingSerializedRepr(name, termRef)

    end BindingFlat


    object ConstrDeclFlat extends HashConsedMutRefReprFlat[ConstrDecl]:
        
        def tag = hashConsTagConstr

        override def toRepr(a: ConstrDecl): HashConsedRef[ConstrDecl] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHCNew(a: ConstrDecl, hashConsed: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val storageTypeSize = summon[Flat[SIRVarStorage]].bitSize(a.storageType)
            val paramsSize = HashConsedReprFlat.listRepr(TypeBindingFlat).bitSizeHC(a.params, hashConsed)
            val typeParamsSize = summon[HashConsedFlat[List[SIRType.TypeVar]]].bitSizeHC(a.typeParams, hashConsed)
            val parentTypeArgsSize = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.parentTypeArgs, hashConsed)
            nameSize +  storageTypeSize + paramsSize + typeParamsSize + parentTypeArgsSize

        def encodeHCNew(a: ConstrDecl, encode: HashConsedEncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[SIRVarStorage]].encode(a.storageType, encode.encode)
            HashConsedReprFlat.listRepr(TypeBindingFlat).encodeHC(a.params, encode)
            summon[HashConsedFlat[List[SIRType.TypeVar]]].encodeHC(a.typeParams, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.parentTypeArgs, encode)
        }

        def decodeHCNew(decode: HashConsedDecoderState): HashConsedRef[ConstrDecl] = {
            val name = summon[Flat[String]].decode(decode.decode)
            val storageType  = summon[Flat[SIRVarStorage]].decode(decode.decode)
            val params = HashConsedReprFlat.listRepr(TypeBindingFlat).decodeHC(decode)
            val typeParams = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
            val parentTypeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            HashConsedRef.deferred(
                hs => params.isComplete(hs) && parentTypeArgs.isComplete(hs),
                hs => ConstrDecl(name, storageType, params.finValue(hs), typeParams, parentTypeArgs.finValue(hs))
            )
        }


    object DataDeclFlat extends HashConsedMutRefReprFlat[DataDecl]:
        
        def tag = hashConsTagDecl

        override def toRepr(a: DataDecl): HashConsedRef[DataDecl] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHCNew(a: DataDecl, encoderState: HashConsed.State): Int =
              val nameSize = summon[Flat[String]].bitSize(a.name)
              val constrSize = HashConsedReprFlat.listRepr(ConstrDeclFlat).bitSizeHC(a.constructors, encoderState)
              val typeParamsSize = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].bitSizeHC(a.typeParams, encoderState)
              nameSize + constrSize + typeParamsSize

        def encodeHCNew(a: DataDecl, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            HashConsedReprFlat.listRepr(ConstrDeclFlat).encodeHC(a.constructors, encode)
            HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].encodeHC(a.typeParams, encode)

        def decodeHCNew(decode: HashConsedDecoderState): HashConsedRef[DataDecl] =
            val name = summon[Flat[String]].decode(decode.decode)
            val constr = HashConsedReprFlat.listRepr(ConstrDeclFlat).decodeHC(decode)
            val typeParams = HashConsedFlat.listHashConsedFlat[SIRType.TypeVar].decodeHC(decode)
            HashConsedRef.deferred(
                hs => constr.isComplete(hs),
                hs => DataDecl(name, constr.finValue(hs), typeParams)
            )

    end DataDeclFlat



    object SIRTypeCaseClassFlat extends HashConsedMutRefReprFlat[SIRType.CaseClass] {

        override def tag = hashConsTagSIRType

        override def toRepr(a: SIRType.CaseClass): HashConsedRef[SIRType.CaseClass] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHCNew(a: SIRType.CaseClass, hashConsed: HashConsed.State): Int = {
            val constrSize = ConstrDeclFlat.bitSizeHC(a.constrDecl, hashConsed)
            val typeArgsSize = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.typeArgs, hashConsed)
            constrSize + typeArgsSize
        }

        override def encodeHCNew(a: SIRType.CaseClass, encode: HashConsedEncoderState): Unit = {
            ConstrDeclFlat.encodeHC(a.constrDecl, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.typeArgs, encode)
        }

        override def decodeHCNew(decode: HashConsedDecoderState): HashConsedRef[SIRType.CaseClass] = {
            val constrDecl = ConstrDeclFlat.decodeHC(decode)
            val typeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            HashConsedRef.deferred(
                hs => constrDecl.isComplete(hs) && typeArgs.isComplete(hs),
                hs => SIRType.CaseClass(constrDecl.finValue(hs), typeArgs.finValue(hs))
            )
        }


    }
    
    object SIRTypeHashConsedFlat extends HashConsedReprFlat[SIRType, HashConsedRef[SIRType]]:

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
        def tag = hashConsTagSIRType

        override def toRepr(a: SIRType): HashConsedRef[SIRType] = {
            HashConsedRef.fromData(a)
        }

        override def bitSizeHC(a: SIRType, hashConsed: HashConsed.State): Int =
            val retval = a match
                case _: SIRType.Primitive[?] => tagWidth
                case SIRType.Data => tagWidth
                case cc: SIRType.CaseClass =>
                    tagWidth + SIRTypeCaseClassFlat.bitSizeHC(cc, hashConsed)
                case scc:SIRType.SumCaseClass =>
                    tagWidth + SIRTypeSumCaseClassFlat.bitSizeHC(scc, hashConsed)
                case fun: SIRType.Fun =>
                    val fromSize = SIRTypeHashConsedFlat.bitSizeHC(fun.in, hashConsed)
                    val toSize = SIRTypeHashConsedFlat.bitSizeHC(fun.out, hashConsed)
                    fromSize + toSize
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
                    hashConsed.lookup(ref.hashCode(), tag) match
                        case None => tagWidth + PlainIntFlat.bitSize(ref.hashCode()) + bitSizeHC(ref, hashConsed)
                        case Some(_) => tagWidth + PlainIntFlat.bitSize(ref.hashCode())
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
                    SIRTypeCaseClassFlat.encodeHC(cc, encode)
                case scc: SIRType.SumCaseClass =>
                    encode.encode.bits(tagWidth, tagSumCaseClass)
                    SIRTypeSumCaseClassFlat.encodeHC(scc, encode)
                case fun@SIRType.Fun(from, to) =>
                    encode.encode.bits(tagWidth, tagFun)
                    SIRTypeHashConsedFlat.encodeHC(from, encode)
                    SIRTypeHashConsedFlat.encodeHC(to, encode)
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
                    encode.hashConsed.lookup(ihc, hashConsTagSIRType) match
                        case None =>
                            encode.hashConsed.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                            encodeHC(ref, encode)
                            encode.hashConsed.setRef(ihc, tag, HashConsedRef.fromData(ref))
                        case Some(_) =>
                case err: SIRType.TypeError =>
                    encode.encode.bits(tagWidth, tagTypeError)
                    summon[Flat[String]].encode(err.msg, encode.encode)
                case SIRType.TypeNothing =>
                    encode.encode.bits(tagWidth, tagTypeNothing)
            val endPos = encode.encode.bitPosition()
            println(s"encode $a,  size=${endPos-startPos}")
                    
                

        override def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIRType] =
            val tag: Byte = decode.decode.bits8(tagWidth)
            tag match 
                case `tagPrimitiveByteString` => HashConsedRef.fromData(SIRType.ByteStringPrimitive)
                case `tagPrimitiveInteger` => HashConsedRef.fromData(SIRType.IntegerPrimitive)
                case `tagPrimitiveString` => HashConsedRef.fromData(SIRType.StringPrimitive)
                case `tagPrimitiveBoolean` => HashConsedRef.fromData(SIRType.BooleanPrimitive)
                case `tagPrimitiveVoid` => HashConsedRef.fromData(SIRType.VoidPrimitive)
                case `tagPrimitiveData` => HashConsedRef.fromData(SIRType.Data)
                case `tagCaseClass` =>
                    SIRTypeCaseClassFlat.decodeHC(decode)
                case `tagSumCaseClass` =>
                    SIRTypeSumCaseClassFlat.decodeHC(decode)
                case `tagFun` =>
                    val from = decodeHC(decode)
                    val to = decodeHC(decode)
                    HashConsedRef.deferred(
                        hs => from.isComplete(hs) && to.isComplete(hs),
                        hs => SIRType.Fun(from.finValue(hs), to.finValue(hs))
                    )
                case `tagTypeVar` =>
                    HashConsedRef.fromData(summon[HashConsedFlat[SIRType.TypeVar]].decodeHC(decode))
                case `tagTypeLambda` =>
                    val params = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
                    val body = decodeHC(decode)
                    HashConsedRef.deferred(
                        hs => body.isComplete(hs),
                        hs => SIRType.TypeLambda(params, body.finValue(hs))
                    )
                case `tagTypeFreeUnificator` => HashConsedRef.fromData(SIRType.FreeUnificator)
                case `tagTypeProxy` =>
                    //val ihc = summon[Flat[Int]].decode(decode.decode)
                    val ihc = PlainIntFlat.decode(decode.decode)
                    decode.hashConsed.lookup(ihc, hashConsTagSIRType) match
                        case None =>
                            var ref: HashConsedRef[SIRType]|Null = null
                            decode.hashConsed.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, hashConsTagSIRType,
                                List((a: HashConsedRef[?]) =>
                                    ref = a.asInstanceOf[HashConsedRef[SIRType]])
                            ))
                            new HashConsedRef[SIRType]:
                                override def isComplete(hashConsed: HashConsed.State): Boolean =
                                    ref != null  && ref.isComplete(hashConsed)

                                override def finValue(hashConsed: HashConsed.State): SIRType = {
                                    if (ref == null)
                                        throw new IllegalStateException("TypeProxy is not resolved")
                                    SIRType.TypeProxy(ref.finValue(hashConsed))
                                }
                        case Some(Left(fw)) =>
                            var ref: HashConsedRef[SIRType] | Null = null
                            fw.addAction((a: HashConsedRef[?]) =>
                                ref = a.asInstanceOf[HashConsedRef[SIRType]]
                            )
                            new HashConsedRef[SIRType] {
                                override def isComplete(hashConsed: HashConsed.State): Boolean =
                                    ref != null && ref.isComplete(hashConsed)
                                override def finValue(hashConsed: HashConsed.State): SIRType = {
                                    if (ref == null)
                                        throw new IllegalStateException("TypeProxy is not resolved")
                                    SIRType.TypeProxy(ref.finValue(hashConsed))
                                }
                            }
                        case Some(Right(a)) =>
                            a.asInstanceOf[HashConsedRef[SIRType.TypeProxy]]
                case `tagTypeError` =>
                    val msg = summon[Flat[String]].decode(decode.decode)
                    HashConsedRef.fromData(SIRType.TypeError(msg, null))
                case `tagTypeNothing` => HashConsedRef.fromData(SIRType.TypeNothing)
                case _ => throw new IllegalStateException(s"Invalid SIRType tag: $tag")
                


    object TypeBindingFlat extends HashConsedReprFlat[TypeBinding,HashConsedRef[TypeBinding]]:

        def tag = hashConsTagTypeBinding

        override def toRepr(a: TypeBinding): HashConsedRef[TypeBinding] = {
            HashConsedRef.fromData(a)
        }

        override def bitSizeHC(a: TypeBinding, hashCons: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val tpSize = SIRTypeHashConsedFlat.bitSizeHC(a.tp, hashCons)
            nameSize + tpSize

        override def encodeHC(a: TypeBinding, encode: HashConsedEncoderState): Unit = {
            summon[Flat[String]].encode(a.name, encode.encode)
            SIRTypeHashConsedFlat.encodeHC(a.tp, encode)
        }

        override def decodeHC(decode: HashConsedDecoderState): HashConsedRef[TypeBinding] = {
            val name = summon[Flat[String]].decode(decode.decode)
            val tp = SIRTypeHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
                hs => tp.isComplete(hs),
                hs => TypeBinding(name, tp.finValue(hs))
            )
        }

    end TypeBindingFlat


    given HashConsedFlat[SIRType.TypeVar] with

        override def bitSizeHC(a: SIRType.TypeVar, hashCons: HashConsed.State): Int =
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





    object SIRTypeSumCaseClassFlat extends HashConsedMutRefReprFlat[SIRType.SumCaseClass] {

        override def tag = hashConsTagSIRType

        override def toRepr(a: SIRType.SumCaseClass): HashConsedRef[SIRType.SumCaseClass] = {
            HashConsedRef.fromData(a)
        }


        override def bitSizeHCNew(a: SIRType.SumCaseClass, state: HashConsed.State): Int =
            val declSize = DataDeclFlat.bitSizeHC(a.decl, state)
            val typeArgsSize = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.typeArgs, state)
            declSize + typeArgsSize

        override def encodeHCNew(a: SIRType.SumCaseClass, encode: HashConsedEncoderState): Unit =
            DataDeclFlat.encodeHC(a.decl, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.typeArgs, encode)

        override def decodeHCNew(decode: HashConsedDecoderState): HashConsedRef[SIRType.SumCaseClass] =
            val declReprRef = DataDeclFlat.decodeHC(decode)
            val typeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            HashConsedRef.deferred(
                hs => declReprRef.isComplete(hs) && typeArgs.isComplete(hs),
                hs => SIRType.SumCaseClass(declReprRef.finValue(hs), typeArgs.finValue(hs))
            )

    }


    object SIRLetHashConsedFlat extends HashConsedReprFlat[SIR.Let, HashConsedRef[SIR.Let]] {

        override def toRepr(a: SIR.Let): HashConsedRef[SIR.Let] =
            HashConsedRef.fromData(a)

        def bitSizeHC(a: SIR.Let, hashConsed: HashConsed.State): Int =
            val recSize = summon[Flat[Recursivity]].bitSize(a.recursivity)
            val bindingsSize = HashConsedReprFlat.listRepr(BindingFlat).bitSizeHC(a.bindings, hashConsed)
            val bodySize = SIRExprHashConsedFlat.bitSizeHC(a.body, hashConsed)
            recSize + bindingsSize + bodySize

        def encodeHC(a: SIR.Let, encode: HashConsedEncoderState): Unit = {
            summon[Flat[Recursivity]].encode(a.recursivity, encode.encode)
            HashConsedReprFlat.listRepr(BindingFlat).encodeHC(a.bindings, encode)
            SIRExprHashConsedFlat.encodeHC(a.body, encode)
        }

        def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Let] = {
            val rec = summon[Flat[Recursivity]].decode(decode.decode)
            val bindings = HashConsedReprFlat.listRepr(BindingFlat).decodeHC(decode)
            val body = SIRExprHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
                hs => bindings.isComplete(hs) && body.isComplete(hs),
                hs => SIR.Let(rec, bindings.finValue(hs), body.finValue(hs))
            )
        }
    }


    object SIRCaseHashConsedFlat extends HashConsedReprFlat[SIR.Case, HashConsedRef[SIR.Case]] {

        override def toRepr(a: SIR.Case): HashConsedRef[SIR.Case] =
            HashConsedRef.fromData(a)

        def bitSizeHC(a: SIR.Case, hashConsed: HashConsed.State): Int =
            val constrSize = ConstrDeclFlat.bitSizeHC(a.constr, hashConsed)
            val bindings = summon[Flat[List[String]]].bitSize(a.bindings)
            val typeBindings = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.typeBindings, hashConsed)
            val bodySize = SIRExprHashConsedFlat.bitSizeHC(a.body, hashConsed)
            constrSize + bindings + typeBindings + bodySize

        def encodeHC(a: SIR.Case, encode: HashConsedEncoderState): Unit = {
            ConstrDeclFlat.encodeHC(a.constr, encode)
            summon[Flat[List[String]]].encode(a.bindings, encode.encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.typeBindings, encode)
            SIRExprHashConsedFlat.encodeHC(a.body, encode)
        }

        def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Case] = {
            val constr = ConstrDeclFlat.decodeHC(decode)
            val bindings = summon[Flat[List[String]]].decode(decode.decode)
            val typeBindings = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            val body = SIRExprHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
                hs => constr.isComplete(hs) && typeBindings.isComplete(hs) && body.isComplete(hs),
                hs => SIR.Case(constr.finValue(hs), bindings, typeBindings.finValue(hs), body.finValue(hs))
            )
        }
    }

    

    object SIRExprHashConsedFlat extends HashConsedReprFlat[SIRExpr, HashConsedRef[SIRExpr]]:
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

        override def toRepr(a: SIRExpr): HashConsedRef[SIRExpr] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHC(a: SIRExpr, hashCons: HashConsed.State): Int = a match
            case SIR.Var(name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(name) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case SIR.ExternalVar(modName, name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(modName)
                             + summon[Flat[String]].bitSize(name)
                             + SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case aLet: SIR.Let =>
                SIRLetHashConsedFlat.bitSizeHC(aLet, hashCons)
            case LamAbs(x, t)        =>
                termTagWidth + bitSizeHC(x,hashCons) + bitSizeHC(t, hashCons)
            case Apply(f, x, tp)     =>
                termTagWidth + bitSizeHC(f,hashCons) + bitSizeHC(x, hashCons) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case aConst: Const        => termTagWidth + SIRConstHashConsedFlat.bitSizeHC(aConst, hashCons)
            case And(x, y)           => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Or(x, y)            => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Not(x)              => termTagWidth + bitSizeHC(x, hashCons)
            case IfThenElse(c, t, f, tp) =>
                termTagWidth + bitSizeHC(c, hashCons) + bitSizeHC(t, hashCons) + bitSizeHC(f, hashCons) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case Builtin(bn, tp)     => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error(msg, cause)   => termTagWidth + summon[Flat[String]].bitSize(msg)
            case Constr(name, data, args) =>
                termTagWidth + summon[Flat[String]].bitSize(name)
                              + DataDeclFlat.bitSizeHC(data, hashCons)
                              + HashConsedReprFlat.listRepr(SIRExprHashConsedFlat).bitSizeHC(args, hashCons)
            case Match(scrutinee, cases, tp) =>
                termTagWidth + bitSizeHC(scrutinee, hashCons)
                    + HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).bitSizeHC(cases, hashCons)


        def encodeHC(a: SIRExpr, enc: HashConsedEncoderState): Unit =
            a match
                case v@Var(name, tp) =>
                    enc.encode.bits(termTagWidth, tagVar)
                    summon[Flat[String]].encode(name, enc.encode)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case aLet: SIR.Let =>
                    enc.encode.bits(termTagWidth, tagLet)
                    SIRLetHashConsedFlat.encodeHC(aLet, enc)
                case LamAbs(x, t) =>
                    enc.encode.bits(termTagWidth, tagLamAbs)
                    SIRVarHashConsedFlat.encodeHC(x, enc)
                    encodeHC(t, enc)
                case Apply(f, x, tp) =>
                    enc.encode.bits(termTagWidth, tagApply)
                    encodeHC(f, enc)
                    encodeHC(x, enc)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case cn@Const(_, _) =>
                    enc.encode.bits(termTagWidth, tagConst)
                    SIRConstHashConsedFlat.encodeHC(cn, enc)
                case IfThenElse(c, t, f, tp) =>
                    enc.encode.bits(termTagWidth, tagIfThenElse)
                    encodeHC(c, enc)
                    encodeHC(t, enc)
                    encodeHC(f, enc)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case Builtin(bn, tp) =>
                    enc.encode.bits(termTagWidth, tagBuiltin)
                    summon[Flat[DefaultFun]].encode(bn, enc.encode)
                case Error(msg, _) =>
                    enc.encode.bits(termTagWidth, tagError)
                    summon[Flat[String]].encode(msg, enc.encode)
                case Constr(name, data, args) =>
                    enc.encode.bits(termTagWidth, tagConstr)
                    summon[Flat[String]].encode(name, enc.encode)
                    DataDeclFlat.encodeHC(data, enc)
                    HashConsedReprFlat.listRepr(SIRHashConsedFlat).encodeHC(args, enc)
                case Match(scrutinee, cases, tp) =>
                    enc.encode.bits(termTagWidth, tagMatch)
                    encodeHC(scrutinee, enc)
                    HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).encodeHC(cases, enc)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case ExternalVar(modName, name, tp) =>
                    enc.encode.bits(termTagWidth, tagExternalVar)
                    summon[Flat[String]].encode(modName, enc.encode)
                    summon[Flat[String]].encode(name, enc.encode)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
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

        def decodeHC(decoder: HashConsedDecoderState): HashConsedRef[SIRExpr] =
            val tag = decoder.decode.bits8(termTagWidth)
            tag match
                case `tagVar` =>
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => tp.isComplete(hs),
                        hs => Var(name, tp.finValue(hs))
                    )
                case `tagLet` =>
                    SIRLetHashConsedFlat.decodeHC(decoder)
                case `tagLamAbs` =>
                    val x = SIRVarHashConsedFlat.decodeHC(decoder)
                    val t = decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => x.isComplete(hs) && t.isComplete(hs),
                        hs => LamAbs(x.finValue(hs), t.finValue(hs))
                    )
                case `tagApply` =>
                    val f = decodeHC(decoder)
                    val x = decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => f.isComplete(hs) && x.isComplete(hs) && tp.isComplete(hs),
                        hs => Apply(f.finValue(hs), x.finValue(hs), tp.finValue(hs))
                    )
                case `tagConst` =>
                    SIRConstHashConsedFlat.decodeHC(decoder)
                case `tagIfThenElse` =>
                    val c = decodeHC(decoder)
                    val t = decodeHC(decoder)
                    val f = decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => c.isComplete(hs) && t.isComplete(hs) && f.isComplete(hs) && tp.isComplete(hs),
                        hs => IfThenElse(c.finValue(hs), t.finValue(hs), f.finValue(hs), tp.finValue(hs))
                    )
                case `tagBuiltin` =>
                    val bn = summon[Flat[DefaultFun]].decode(decoder.decode)
                    HashConsedRef.fromData(SIRBuiltins.fromUplc(bn))
                case `tagError` =>
                    val msg = summon[Flat[String]].decode(decoder.decode)
                    HashConsedRef.fromData(Error(msg))
                case `tagConstr` =>
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val data = DataDeclFlat.decodeHC(decoder)
                    val args = HashConsedReprFlat.listRepr(SIRExprHashConsedFlat).decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => data.isComplete(hs) && args.isComplete(hs),
                        hs => Constr(name, data.finValue(hs), args.finValue(hs))
                    )
                case `tagMatch` =>
                    val scrutinee = decodeHC(decoder)
                    val cases =  HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => scrutinee.isComplete(hs) && cases.isComplete(hs) && tp.isComplete(hs),
                        hs => Match(scrutinee.finValue(hs), cases.finValue(hs), tp.finValue(hs))
                    )
                case `tagExternalVar` =>
                    val modName = summon[Flat[String]].decode(decoder.decode)
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => tp.isComplete(hs),
                        hs => ExternalVar(modName, name, tp.finValue(hs))
                    )
                case `tagAnd` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => x.isComplete(hs) && y.isComplete(hs),
                        hs => And(x.finValue(hs), y.finValue(hs))
                    )
                case `tagOr` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => x.isComplete(hs) && y.isComplete(hs),
                        hs => Or(x.finValue(hs), y.finValue(hs))
                    )
                case `tagNot` =>
                    val x = decodeHC(decoder)
                    HashConsedRef.deferred(
                        hs => x.isComplete(hs),
                        hs => Not(x.finValue(hs))
                    )

                    
    end SIRExprHashConsedFlat                

    object SIRVarHashConsedFlat extends HashConsedReprFlat[SIR.Var, HashConsedRef[SIR.Var]]:

        override def toRepr(a: SIR.Var): HashConsedRef[SIR.Var] =
            HashConsedRef.fromData(a)
        
        override def bitSizeHC(a: SIR.Var, hashCons: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val tpSize = SIRTypeHashConsedFlat.bitSizeHC(a.tp, hashCons)
            nameSize + tpSize

        override def encodeHC(a: SIR.Var, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            SIRTypeHashConsedFlat.encodeHC(a.tp, encode)

        override def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Var] =
            val name = summon[Flat[String]].decode(decode.decode)
            val tp = SIRTypeHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
                hs => tp.isComplete(hs),
                hs => SIR.Var(name, tp.finValue(hs))
            )
            
    
    given Flat[SIRVarStorage] with
        override def bitSize(a: SIRVarStorage): Int =
            summon[Flat[Boolean]].bitSize(a == SIRVarStorage.Data)

        override def decode(decode: DecoderState): SIRVarStorage =
            if summon[Flat[Boolean]].decode(decode) then
                SIRVarStorage.Data
            else SIRVarStorage.LocalUPLC

        override def encode(a: SIRVarStorage, encode: EncoderState): Unit =
            summon[Flat[Boolean]].encode(a == SIRVarStorage.Data, encode)

    object SIRConstHashConsedFlat extends HashConsedReprFlat[SIR.Const, HashConsedRef[SIR.Const]]:

        override def toRepr(a: SIR.Const): HashConsedRef[SIR.Const] =
            HashConsedRef.fromData(a)

        override def bitSizeHC(a: SIR.Const, hashCons: HashConsed.State): Int =
            val constSize = flatConstant.bitSize(a.uplcConst)
            val tpSize = SIRTypeHashConsedFlat.bitSizeHC(a.tp, hashCons)
            tpSize + constSize

        override def encodeHC(a: SIR.Const, encode: HashConsedEncoderState): Unit =
            flatConstant.encode(a.uplcConst, encode.encode)
            SIRTypeHashConsedFlat.encodeHC(a.tp, encode)

        override def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Const] =
            val uplcConst = flatConstant.decode(decode.decode)
            val tp = SIRTypeHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
                hs => tp.isComplete(hs),
                hs => SIR.Const(uplcConst, tp.finValue(hs))
            )


    object SIRDeclHashConsedFlat extends HashConsedReprFlat[SIR.Decl, HashConsedRef[SIR.Decl]]:

        override def toRepr(a: SIR.Decl): HashConsedRef[SIR.Decl] =
            HashConsedRef.fromData(a)

        override def bitSizeHC(a: SIR.Decl, hashCons: HashConsed.State): Int =
                DataDeclFlat.bitSizeHC(a.data, hashCons) +
                   SIRHashConsedFlat.bitSizeHC(a.term, hashCons)

        override def encodeHC(a: SIR.Decl, encode: HashConsedEncoderState): Unit =
            DataDeclFlat.encodeHC(a.data, encode)
            SIRHashConsedFlat.encodeHC(a.term, encode)

        override def decodeHC(decoder: HashConsedDecoderState): HashConsedRef[SIR.Decl] =
             val data = DataDeclFlat.decodeHC(decoder)
             val term = SIRHashConsedFlat.decodeHC(decoder)
             new HashConsedRef[SIR.Decl] {
                 override def isComplete(hashConsed: HashConsed.State): Boolean =
                     data.isComplete(hashConsed) && term.isComplete(hashConsed)
                 override def finValue(hashConsed: HashConsed.State): SIR.Decl = {
                     SIR.Decl(data.finValue(hashConsed), term.finValue(hashConsed))
                 }
             }


    object SIRHashConsedFlat extends HashConsedReprFlat[SIR, HashConsedRef[SIR]]:

        final val tagSIRDef = 0x00
        final val tagSIRExpr = 0x01

        final val sirTagWidth = 1

        override def toRepr(a: SIR): HashConsedRef[SIR] =
            HashConsedRef.fromData(a)

        override def bitSizeHC(a: SIR, hashCons: HashConsed.State): Int =
            a match
                case sdf: SIRDef =>
                    val aSize = sdf match
                        case dcl: SIR.Decl => SIRDeclHashConsedFlat.bitSizeHC(dcl, hashCons)
                        case _ =>
                            throw new IllegalStateException(s"Unsupported SIRDef: $sdf")
                    1 + aSize
                case sexpr: SIRExpr =>
                    1 + SIRExprHashConsedFlat.bitSizeHC(sexpr, hashCons)

        override def encodeHC(a: SIR, encode: HashConsedEncoderState): Unit =
            a match
                case sdf: SIRDef =>
                    encode.encode.bits(sirTagWidth, tagSIRDef)
                    sdf match
                        case dcl: SIR.Decl => SIRDeclHashConsedFlat.encodeHC(dcl, encode)
                        case _ =>
                            throw new IllegalStateException(s"Unsupported SIRDef: $sdf")
                case sexpr: SIRExpr =>
                    encode.encode.bits(sirTagWidth, tagSIRExpr)
                    SIRExprHashConsedFlat.encodeHC(sexpr, encode)

        override def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR] =
            decode.decode.bits8(1) match
                case `tagSIRDef` =>
                    SIRDeclHashConsedFlat.decodeHC(decode)
                case `tagSIRExpr` =>
                    SIRExprHashConsedFlat.decodeHC(decode)

    end SIRHashConsedFlat

    class ModuleSerializedRef(val version: (Int, Int), val defs: HashConsedRef[List[Binding]]) extends HashConsedRef[Module] {

        override def isComplete(hs: HashConsed.State): Boolean = defs.isComplete(hs)

        override def finValue(hc: HashConsed.State): Module = {
            Module(version, defs.finValue(hc))
        }

    }

    object ModuleHashSetReprFlat extends HashConsedReprFlat[Module, ModuleSerializedRef]:

        override def toRepr(a: Module): ModuleSerializedRef =
            new ModuleSerializedRef(a.version, HashConsedRef.fromData(a.defs))

        override def bitSizeHC(a: Module, hs: HashConsed.State): Int =
            summon[Flat[(Int, Int)]].bitSize(a.version) +
                HashConsedReprFlat.listRepr(BindingFlat).bitSizeHC(a.defs, hs)

        override def encodeHC(a: Module, enc: HashConsedEncoderState): Unit =
            summon[Flat[(Int, Int)]].encode(a.version, enc.encode)
            HashConsedReprFlat.listRepr(BindingFlat).encodeHC(a.defs, enc)

        override def decodeHC(decoder: HashConsedDecoderState): ModuleSerializedRef =
            val version = summon[Flat[(Int, Int)]].decode(decoder.decode)
            val defs = HashConsedReprFlat.listRepr(BindingFlat).decodeHC(decoder)
            ModuleSerializedRef(version, defs)

    given HashConsedFlat[Module] with
        def bitSizeHC(a: Module, hs: HashConsed.State): Int =
            println(s"before bitSizeHC Module ${a.version}, head-name = ${a.defs.head.name}")
            val retval = ModuleHashSetReprFlat.bitSizeHC(a, hs)
            println(s"after bitSizeHC module ${a.version}, heed=name = ${a.defs.head.name} retval =${retval}")
            retval

        def encodeHC(a: Module, enc: HashConsedEncoderState): Unit =
            println(s"before encoding Module ${a.version}, head-name = ${a.defs.head.name}")
            ModuleHashSetReprFlat.encodeHC(a, enc)
            println(s"after encoding Module , count=${enc.encode.bitPosition()}")

        def decodeHC(decoder: HashConsedDecoderState): Module =
            // here we know that Module is the whole data, so decondign is finished
            println(s"before decoding Module")
            val repr = ModuleHashSetReprFlat.decodeHC(decoder)
            decoder.runFinCallbacks()
            val retval = repr.finValue(decoder.hashConsed)
            println(s"after decoding Module, head-name = ${retval.defs.head.name}")
            retval

