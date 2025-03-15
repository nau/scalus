package scalus.flat

import scala.util.control.NonFatal
import scalus.{builtin, flat}
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.given
import scalus.sir.{Binding, ConstrDecl, DataDecl, Module, Recursivity, SIR, SIRBuiltins, SIRType, SIRVarStorage, TypeBinding}
import scalus.uplc.CommonFlatInstances.*
import scalus.uplc.CommonFlatInstances.given
import scalus.builtin.Data
import scalus.sir.SIR.Pattern
import scalus.uplc.DefaultFun
import scalus.utils.*
import scalus.utils.HashConsed.CachedTaggedRef

object FlatInstantces:
    // val termTagWidth = 4

    final val hashConsTagSIRType = HashConsed.tag(0x01)
    final val hashConsTagDecl = HashConsed.tag(0x02)
    final val hashConsTagConstr = HashConsed.tag(0x03)
    final val hashConsTagBinging = HashConsed.tag(0x04)
    final val hashConsTagCase = HashConsed.tag(0x05)
    final val hashConsTagLet = HashConsed.tag(0x06)
    final val hashConsTagSIR = HashConsed.tag(0x07)
    final val hashConsTagTypeBinding = HashConsed.tag(0x08)

    given Flat[Data] with
        private val width = 3

        def bitSize(a: Data): Int = a match
            case Data.Constr(constr, args) =>
                width + summon[Flat[Long]].bitSize(constr) + summon[Flat[List[Data]]].bitSize(args)
            case Data.Map(values) =>
                width + summon[Flat[List[(Data, Data)]]].bitSize(values)
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

    object BindingFlat extends HashConsedReprFlat[Binding, HashConsedRef[Binding]]:

        def tag = hashConsTagBinging

        override def toRepr(a: Binding): HashConsedRef[Binding] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHC(a: Binding, hashConsed: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val termSize = SIRHashConsedFlat.bitSizeHC(a.value, hashConsed)
            val retval = nameSize + termSize
            retval

        def encodeHC(a: Binding, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            SIRHashConsedFlat.encodeHC(a.value, encode)

        def decodeHC(decode: HashConsedDecoderState): HashConsedRef[Binding] =
            val name = summon[Flat[String]].decode(decode.decode)
            val termRef = SIRHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
              hs => termRef.isComplete(hs),
              (hs, level, parents) => Binding(name, termRef.finValue(hs, level, parents))
            )

    end BindingFlat

    object ConstrDeclFlat extends HashConsedMutRefReprFlat[ConstrDecl, HashConsedRef[ConstrDecl]]:

        def tag = hashConsTagConstr

        override def toRepr(a: ConstrDecl): HashConsedRef[ConstrDecl] = {
            HashConsedRef.fromData(a)
        }

        override def saFromForward(
            state: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): HashConsedRef[ConstrDecl] = {
            HashConsedRef.fromForward[ConstrDecl](state, fw.ihc, fw.tag)
        }

        override def saFromRef(ref: HashConsedRef[ConstrDecl]) = ref

        def bitSizeHCNew(a: ConstrDecl, hashConsed: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val storageTypeSize = summon[Flat[SIRVarStorage]].bitSize(a.storageType)
            val paramsSize =
                HashConsedReprFlat.listRepr(TypeBindingFlat).bitSizeHC(a.params, hashConsed)
            val typeParamsSize =
                summon[HashConsedFlat[List[SIRType.TypeVar]]].bitSizeHC(a.typeParams, hashConsed)
            val parentTypeArgsSize = HashConsedReprFlat
                .listRepr(SIRTypeHashConsedFlat)
                .bitSizeHC(a.parentTypeArgs, hashConsed)
            nameSize + storageTypeSize + paramsSize + typeParamsSize + parentTypeArgsSize

        def encodeHCNew(a: ConstrDecl, encode: HashConsedEncoderState): Unit = {
            val debug = encode.debug
            if (debug) then println(s"ConstrDecl = ${a}")
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[SIRVarStorage]].encode(a.storageType, encode.encode)
            HashConsedReprFlat.listRepr(TypeBindingFlat).encodeHC(a.params, encode)
            summon[HashConsedFlat[List[SIRType.TypeVar]]].encodeHC(a.typeParams, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.parentTypeArgs, encode)
        }

        def decodeHCNew(decode: HashConsedDecoderState): HashConsedRef[ConstrDecl] = {
            val name = summon[Flat[String]].decode(decode.decode)
            val storageType = summon[Flat[SIRVarStorage]].decode(decode.decode)
            val params = HashConsedReprFlat.listRepr(TypeBindingFlat).decodeHC(decode)
            val typeParams = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
            val parentTypeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            // HashConsedRef.deferred(
            //    hs => params.isComplete(hs) && parentTypeArgs.isComplete(hs),
            //    (hs, level, parents) =>
            //        ConstrDecl(name, storageType, params.finValue(hs, level, parents), typeParams,
            //            parentTypeArgs.finValue(hs, level, parents))
            // )
            HashConsedRef.deferred(
              hs => params.isComplete(hs),
              (hs, level, parents) =>
                  try
                      ConstrDecl(
                        name,
                        storageType,
                        params.finValue(hs, level, parents),
                        typeParams,
                        parentTypeArgs.finValue(hs, level, parents)
                      )
                  catch
                      case scala.util.control.NonFatal(ex) =>
                          println(s"can't decodeHCNew ConstrDecl $name")
                          println(s"check where is loop:")
                          try {
                              val unused = params.finValue(hs, level, parents)
                          } catch {
                              case scala.util.control.NonFatal(ex1) =>
                                  println(s"in params, params=${params}")
                                  throw ex1
                          }
                          try {
                              val unused = parentTypeArgs.finValue(hs, level, parents)
                          } catch
                              case scala.util.control.NonFatal(ex2) =>
                                  println("in parentTypeArgs")
                                  throw ex2

                          throw ex
            )
        }

    object DataDeclFlat extends HashConsedMutRefReprFlat[DataDecl, HashConsedRef[DataDecl]]:

        def tag = hashConsTagDecl

        override def toRepr(a: DataDecl): HashConsedRef[DataDecl] = {
            HashConsedRef.fromData(a)
        }

        override def saFromForward(
            state: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): HashConsedRef[DataDecl] =
            HashConsedRef.fromForward(state, fw.ihc, fw.tag)

        override def saFromRef(ref: HashConsedRef[DataDecl]): HashConsedRef[DataDecl] =
            ref

        def bitSizeHCNew(a: DataDecl, encoderState: HashConsed.State): Int =
            val nameSize = summon[Flat[String]].bitSize(a.name)
            val constrSize =
                HashConsedReprFlat.listRepr(ConstrDeclFlat).bitSizeHC(a.constructors, encoderState)
            val typeParamsSize = HashConsedFlat
                .listHashConsedFlat[SIRType.TypeVar]
                .bitSizeHC(a.typeParams, encoderState)
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
              (hs, level, parents) =>
                  try DataDecl(name, constr.finValue(hs, level, parents), typeParams)
                  catch
                      case scala.util.control.NonFatal(ex) =>
                          println(s"Can't decode DataDecl $name")
                          throw ex;
            )

    end DataDeclFlat

    object SIRTypeCaseClassFlat extends HashConsedMutRefReprFlat[SIRType, SIRTypeHashConsedRef] {

        override def tag = hashConsTagSIRType

        override def toRepr(a: SIRType): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromData(a)
        }

        override def saFromForward(
            state: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromForward(state, fw)
        }

        override def saFromRef(ref: HashConsedRef[SIRType]): SIRTypeHashConsedRef =
            SIRTypeHashConsedRef.fromRef(ref)

        def bitSizeHCNew(inA: SIRType, hashConsed: HashConsed.State): Int = {
            val a = inA match
                case cc: SIRType.CaseClass => cc
                case _ => throw IllegalStateException(s"expected SIRType.CaseClass we have $inA")
            val constrSize = ConstrDeclFlat.bitSizeHC(a.constrDecl, hashConsed)
            val typeArgsSize =
                HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.typeArgs, hashConsed)
            val parentBitSize =
                if (a.parent.isEmpty) then 1
                else 1 + SIRTypeHashConsedFlat.bitSizeHC(a.parent.get, hashConsed)
            constrSize + typeArgsSize + parentBitSize
        }

        override def encodeHCNew(inA: SIRType, encode: HashConsedEncoderState): Unit = {
            val a = inA match
                case cc: SIRType.CaseClass => cc
                case _ => throw IllegalStateException(s"expected SIRType.CaseClass we have $inA")
            ConstrDeclFlat.encodeHC(a.constrDecl, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.typeArgs, encode)
            encode.encode.bits(1, if a.parent.isEmpty then 0 else 1)
            a.parent.foreach(SIRTypeHashConsedFlat.encodeHC(_, encode))
        }

        override def decodeHCNew(
            decode: HashConsedDecoderState
        ): SIRTypeHashConsedRef = {
            val constrDecl = ConstrDeclFlat.decodeHC(decode)
            val typeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            val optParentRef = decode.decode.bits8(1) match
                case 0 => None
                case _ => Some(SIRTypeHashConsedFlat.decodeHC(decode))
            SIRTypeHashConsedRef.deferred(
              hs => constrDecl.isComplete(hs) && typeArgs.isComplete(hs),
              (hs, level, parents) =>
                  SIRType.CaseClass(
                    constrDecl.finValue(hs, level, parents),
                    typeArgs.finValue(hs, level, parents),
                    optParentRef.map(_.finValue(hs, level, parents))
                  )
            )
        }

    }

    //  when we have recursive dependencies,  we just install proxy
    class SIRTypeHashConsedRef(
        opComplete: HashConsed.State => Boolean,
        opFinValue: (HashConsed.State, Int, HSRIdentityHashMap) => SIRType,
        initResult: SIRType | Null = null
    ) extends HashConsedRef[SIRType] {

        var result: SIRType | Null = initResult
        var proxies: List[SIRType.TypeProxy] = Nil

        override def isComplete(hashConsed: HashConsed.State): Boolean =
            if result eq null then opComplete(hashConsed) else true

        override def finValue(
            hashConsed: HashConsed.State,
            level: Int,
            parents: HSRIdentityHashMap
        ): SIRType = {
            if result eq null then
                if !(parents.get(this) eq null) then
                    // loop detected.
                    val proxy = SIRType.TypeProxy(null)
                    proxies = proxy :: proxies
                    proxy
                else
                    parents.put(this, this)
                    val retval = opFinValue(hashConsed, level + 1, parents)
                    parents.remove(this)
                    val unproxied = retval match
                        case SIRType.TypeProxy(ref) => ref
                        case _                      => retval
                    proxies.foreach(p => p.ref = unproxied)
                    result = retval
                    retval
            else result
        }

    }

    object SIRTypeHashConsedRef {
        def fromData(a: SIRType) = new SIRTypeHashConsedRef((s) => true, (s, l, p) => a, a)
        def deferred(
            opComplete: HashConsed.State => Boolean,
            opFinValue: (HashConsed.State, Int, HSRIdentityHashMap) => SIRType
        ) =
            new SIRTypeHashConsedRef(opComplete, opFinValue, null)

        def fromForward(
            hs0: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): SIRTypeHashConsedRef =
            val retval = SIRTypeHashConsedRef.deferred(
              hs => hs.lookup(fw.ihc, fw.tag).map(_.isRight).getOrElse(false),
              (hs, l, ps) => {
                  hs.lookup(fw.ihc, fw.tag) match
                      case Some(Right(sa)) =>
                          sa.asInstanceOf[SIRTypeHashConsedRef].finValue(hs, l + 1, ps)
                      case _ =>
                          throw IllegalStateException("Reference in finValue is not set")
              }
            )
            retval

        def fromRef(ref: HashConsedRef[SIRType]): SIRTypeHashConsedRef =
            ref match
                case sthcr: SIRTypeHashConsedRef => sthcr
                case HashConsed.ConstRef(data)   => SIRTypeHashConsedRef.fromData(data)
                case ctr: CachedTaggedRef[?] if ctr.ref.isInstanceOf[SIRTypeHashConsedRef] =>
                    ctr.ref.asInstanceOf[SIRTypeHashConsedRef]
                case _ =>
                    SIRTypeHashConsedRef.deferred(
                      hs => ref.isComplete(hs),
                      (hs, l, p) => ref.finValue(hs, l + 1, p)
                    )

    }

    object SIRTypeHashConsedFlat extends HashConsedReprFlat[SIRType, SIRTypeHashConsedRef]:

        final val tagWidth = 5

        //  Types
        val tagPrimitiveByteString: Byte = 0x00
        val tagPrimitiveInteger: Byte = 0x01
        val tagPrimitiveString: Byte = 0x02
        val tagPrimitiveBoolean: Byte = 0x03
        val tagPrimitiveVoid: Byte = 0x04
        val tagPrimitiveData: Byte = 0x05
        val tagCaseClass: Byte = 0x06
        val tagSumCaseClass: Byte = 0x07
        val tagFun: Byte = 0x08
        val tagTypeVar: Byte = 0x09
        val tagTypeLambda: Byte = 0x0a
        val tagTypeFreeUnificator: Byte = 0x0b
        val tagTypeProxy: Byte = 0x0c
        // val tagTypeError: Byte = 0x0d -- free.
        val tagTypeNothing: Byte = 0x0e
        val tagNonCaseModule: Byte = 0x0f
        val tagBls12_381_G1_Element: Byte = 0x10
        val tagBls12_381_G2_Element: Byte = 0x11
        val tagBls12_381_MlResult: Byte = 0x12

        //
        def tag = hashConsTagSIRType

        override def toRepr(a: SIRType): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromData(a)
        }

        override def bitSizeHC(a: SIRType, hashConsed: HashConsed.State): Int =
            // println(s"SIRTypeHashConsedFlat.bisSizeHC start ${a.hashCode()} ${a}")
            var mute = true
            val retval = a match
                case _: SIRType.Primitive =>
                    tagWidth
                case cc: SIRType.CaseClass =>
                    tagWidth + SIRTypeCaseClassFlat.bitSizeHC(cc, hashConsed)
                case scc: SIRType.SumCaseClass =>
                    tagWidth + SIRTypeSumCaseClassFlat.bitSizeHC(scc, hashConsed)
                case fun: SIRType.Fun =>
                    val fromSize = SIRTypeHashConsedFlat.bitSizeHC(fun.in, hashConsed)
                    val toSize = SIRTypeHashConsedFlat.bitSizeHC(fun.out, hashConsed)
                    tagWidth + fromSize + toSize
                case aType: SIRType.TypeVar =>
                    mute = true
                    tagWidth +
                        summon[HashConsedFlat[SIRType.TypeVar]].bitSizeHC(aType, hashConsed)
                case SIRType.TypeLambda(params, body) =>
                    tagWidth + summon[HashConsedFlat[List[SIRType.TypeVar]]]
                        .bitSizeHC(params, hashConsed) +
                        bitSizeHC(body, hashConsed)
                case SIRType.FreeUnificator =>
                    mute = true
                    tagWidth
                case tp: SIRType.TypeProxy =>
                    tagWidth + SIRTypeTypeProxyFlat.bitSizeHC(tp, hashConsed)
                case a: SIRType.TypeNonCaseModule =>
                    tagWidth + SIRTypeNonCaseModuleFlat.bitSizeHC(a, hashConsed)
                case SIRType.TypeNothing => tagWidth

            if !mute then
                println(s"SIRTypeHashConsedFlat.bisSizeHC end ${a.hashCode()} $a =${retval}")
            retval

        override def encodeHC(a: SIRType, encode: HashConsedEncoderState): Unit =
            // println(s"SIRTypeHashConsedFlat.encodeHC:start ${a.hashCode()} $a, pos=${encode.encode.bitPosition()}")
            var mute = false
            val startPos = encode.encode.bitPosition()
            a match
                case SIRType.ByteString =>
                    encode.encode.bits(tagWidth, tagPrimitiveByteString)
                case SIRType.Integer =>
                    encode.encode.bits(tagWidth, tagPrimitiveInteger)
                case SIRType.String =>
                    encode.encode.bits(tagWidth, tagPrimitiveString)
                case SIRType.Boolean =>
                    encode.encode.bits(tagWidth, tagPrimitiveBoolean)
                case SIRType.Unit =>
                    encode.encode.bits(tagWidth, tagPrimitiveVoid)
                case SIRType.Data =>
                    encode.encode.bits(tagWidth, tagPrimitiveData)
                case cc: SIRType.CaseClass =>
                    encode.encode.bits(tagWidth, tagCaseClass)
                    SIRTypeCaseClassFlat.encodeHC(cc, encode)
                case scc: SIRType.SumCaseClass =>
                    encode.encode.bits(tagWidth, tagSumCaseClass)
                    SIRTypeSumCaseClassFlat.encodeHC(scc, encode)
                case fun @ SIRType.Fun(from, to) =>
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
                case tp: SIRType.TypeProxy =>
                    encode.encode.bits(tagWidth, tagTypeProxy)
                    SIRTypeTypeProxyFlat.encodeHC(tp, encode)
                case a: SIRType.TypeNonCaseModule =>
                    encode.encode.bits(tagWidth, tagNonCaseModule)
                    SIRTypeNonCaseModuleFlat.encodeHC(a, encode)
                case SIRType.TypeNothing =>
                    encode.encode.bits(tagWidth, tagTypeNothing)
                case SIRType.BLS12_381_G1_Element =>
                    encode.encode.bits(tagWidth, tagBls12_381_G1_Element)
                case SIRType.BLS12_381_G2_Element =>
                    encode.encode.bits(tagWidth, tagBls12_381_G2_Element)
                case SIRType.BLS12_381_MlResult =>
                    encode.encode.bits(tagWidth, tagBls12_381_MlResult)
            // if !mute then
            //    //val endPos = encode.encode.bitPosition()
            //    //println(s"SIRTypeHashConsedFlat.encode ${a.hashCode()} $a,  size=${endPos-startPos}")

        override def decodeHC(decode: HashConsedDecoderState): SIRTypeHashConsedRef =
            val ctag = decode.decode.bits8(tagWidth)
            // println(s"SIRTypeHashConsedFlat.decodeHC: ctag=${ctag}, pos=${decode.decode.currPtr*8 + decode.decode.usedBits}")
            ctag match
                case `tagPrimitiveByteString` => SIRTypeHashConsedRef.fromData(SIRType.ByteString)
                case `tagPrimitiveInteger`    => SIRTypeHashConsedRef.fromData(SIRType.Integer)
                case `tagPrimitiveString`     => SIRTypeHashConsedRef.fromData(SIRType.String)
                case `tagPrimitiveBoolean`    => SIRTypeHashConsedRef.fromData(SIRType.Boolean)
                case `tagPrimitiveVoid`       => SIRTypeHashConsedRef.fromData(SIRType.Unit)
                case `tagPrimitiveData`       => SIRTypeHashConsedRef.fromData(SIRType.Data)
                case `tagCaseClass` =>
                    SIRTypeCaseClassFlat.decodeHC(decode)
                case `tagSumCaseClass` =>
                    SIRTypeSumCaseClassFlat.decodeHC(decode)
                case `tagFun` =>
                    val from = decodeHC(decode)
                    val to = decodeHC(decode)
                    SIRTypeHashConsedRef.deferred(
                      hs => from.isComplete(hs) && to.isComplete(hs),
                      (hs, level, parents) =>
                          SIRType.Fun(
                            from.finValue(hs, level, parents),
                            to.finValue(hs, level, parents)
                          )
                    )
                case `tagTypeVar` =>
                    SIRTypeHashConsedRef.fromData(
                      summon[HashConsedFlat[SIRType.TypeVar]].decodeHC(decode)
                    )
                case `tagTypeLambda` =>
                    val params = summon[HashConsedFlat[List[SIRType.TypeVar]]].decodeHC(decode)
                    val body = decodeHC(decode)
                    SIRTypeHashConsedRef.deferred(
                      hs => body.isComplete(hs),
                      (hs, level, parents) =>
                          SIRType.TypeLambda(params, body.finValue(hs, level, parents))
                    )
                case `tagTypeFreeUnificator` =>
                    SIRTypeHashConsedRef.fromData(SIRType.FreeUnificator)
                case `tagTypeProxy` =>
                    SIRTypeTypeProxyFlat.decodeHC(decode)
                case `tagTypeNothing` => SIRTypeHashConsedRef.fromData(SIRType.TypeNothing)
                case `tagNonCaseModule` =>
                    SIRTypeNonCaseModuleFlat.decodeHC(decode)
                case `tagBls12_381_G1_Element` =>
                    SIRTypeHashConsedRef.fromData(SIRType.BLS12_381_G1_Element)
                case `tagBls12_381_G2_Element` =>
                    SIRTypeHashConsedRef.fromData(SIRType.BLS12_381_G2_Element)
                case `tagBls12_381_MlResult` =>
                    SIRTypeHashConsedRef.fromData(SIRType.BLS12_381_MlResult)
                case _ => throw new IllegalStateException(s"Invalid SIRType tag: $tag")

    object TypeBindingFlat extends HashConsedReprFlat[TypeBinding, HashConsedRef[TypeBinding]]:

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
              (hs, l, p) =>
                  try TypeBinding(name, tp.finValue(hs, l, p))
                  catch
                      case NonFatal(ex) =>
                          println(s"Can;t decopde TypeBinging $name")
                          throw ex
            )
        }

    end TypeBindingFlat

    given HashConsedFlat[SIRType.TypeVar] with

        override def bitSizeHC(a: SIRType.TypeVar, hashCons: HashConsed.State): Int =
            summon[Flat[String]].bitSize(a.name) + summon[Flat[Long]].bitSize(a.optId.getOrElse(0L))

        override def encodeHC(a: SIRType.TypeVar, encode: HashConsedEncoderState): Unit =
            summon[Flat[String]].encode(a.name, encode.encode)
            summon[Flat[Long]].encode(a.optId.getOrElse(0L), encode.encode)

        override def decodeHC(decode: HashConsedDecoderState): SIRType.TypeVar =
            val name = summon[Flat[String]].decode(decode.decode)
            val optId = summon[Flat[Long]].decode(decode.decode) match
                case 0  => None
                case id => Some(id)
            SIRType.TypeVar(name, optId)

    object SIRTypeSumCaseClassFlat extends HashConsedMutRefReprFlat[SIRType, SIRTypeHashConsedRef] {

        override def tag = hashConsTagSIRType

        override def toRepr(a: SIRType): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromData(a)
        }

        override def saFromForward(
            state: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): SIRTypeHashConsedRef =
            SIRTypeHashConsedRef.fromForward(state, fw)

        override def saFromRef(ref: HashConsedRef[SIRType]): SIRTypeHashConsedRef =
            SIRTypeHashConsedRef.fromRef(ref)

        override def bitSizeHCNew(inA: SIRType, state: HashConsed.State): Int =
            val a = inA match
                case sc: SIRType.SumCaseClass => sc
                case _ => throw IllegalStateException(s"Expecting inA be SumCaseClass, have ${inA}")
            val declSize = DataDeclFlat.bitSizeHC(a.decl, state)
            val typeArgsSize =
                HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).bitSizeHC(a.typeArgs, state)
            declSize + typeArgsSize

        override def encodeHCNew(inA: SIRType, encode: HashConsedEncoderState): Unit =
            val a = inA match
                case sc: SIRType.SumCaseClass => sc
                case _ => throw IllegalStateException(s"Expecting inA be SumCaseClass, have ${inA}")
            DataDeclFlat.encodeHC(a.decl, encode)
            HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).encodeHC(a.typeArgs, encode)

        override def decodeHCNew(
            decode: HashConsedDecoderState
        ): SIRTypeHashConsedRef =
            val declReprRef = DataDeclFlat.decodeHC(decode)
            val typeArgs = HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
            SIRTypeHashConsedRef.deferred(
              hs => declReprRef.isComplete(hs) && typeArgs.isComplete(hs),
              (hs, l, p) =>
                  SIRType.SumCaseClass(declReprRef.finValue(hs, l, p), typeArgs.finValue(hs, l, p))
            )

    }

    object SIRTypeNonCaseModuleFlat
        extends HashConsedMutRefReprFlat[SIRType, SIRTypeHashConsedRef] {

        override def tag = hashConsTagSIRType

        override def toRepr(
            a: SIRType
        ): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromData(a)
        }

        override def saFromForward(
            state: HashConsed.State,
            fw: HashConsed.ForwardRefAcceptor
        ): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromForward(state, fw)
        }

        override def saFromRef(ref: HashConsedRef[SIRType]): SIRTypeHashConsedRef =
            SIRTypeHashConsedRef.fromRef(ref)

        override def bitSizeHCNew(inA: SIRType, state: HashConsed.State): Int =
            val a = inA match
                case tncm: SIRType.TypeNonCaseModule => tncm
                case _ =>
                    throw IllegalStateException(s"Expected SIRType.TypeNonCaseModule, have ${inA}")
            summon[Flat[String]].bitSize(a.name)

        override def encodeHCNew(
            inA: SIRType,
            encode: HashConsedEncoderState
        ): Unit =
            val a = inA match
                case tncm: SIRType.TypeNonCaseModule => tncm
                case _ =>
                    throw IllegalStateException(s"Expected SIRType.TypeNonCaseModule, have ${inA}")
            summon[Flat[String]].encode(a.name, encode.encode)

        override def decodeHCNew(
            decode: HashConsedDecoderState
        ): SIRTypeHashConsedRef =
            val name = summon[Flat[String]].decode(decode.decode)
            SIRTypeHashConsedRef.fromData(SIRType.TypeNonCaseModule(name))

    }

    object SIRTypeTypeProxyFlat extends HashConsedReprFlat[SIRType, SIRTypeHashConsedRef] {

        def tag = hashConsTagSIRType

        override def toRepr(a: SIRType): SIRTypeHashConsedRef = {
            SIRTypeHashConsedRef.fromData(a)
        }

        override def bitSizeHC(inA: SIRType, hashConsed: HashConsed.State): Int =
            val a = inA match
                case tp: SIRType.TypeProxy => tp
                case _ => throw IllegalStateException(s"Expected TypeProxy, have ${inA}")
            val ref = a.ref
            if ref == null then
                throw new IllegalStateException(
                  "TypeProxy id is null, looks lise save or restore is invalid"
                )
            val ihc = ref.hashCode()
            hashConsed.lookup(ihc, tag) match
                case None =>
                    val preSize = PlainIntFlat.bitSize(ihc)
                    // hashConsed.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                    val restSize = SIRTypeHashConsedFlat.bitSizeHC(ref, hashConsed)
                    hashConsed.lookup(ihc, tag) match
                        case None =>
                            hashConsed.setRef(ihc, tag, HashConsedRef.fromData(ref))
                        case Some(_) =>
                    preSize + restSize
                case Some(_) =>
                    PlainIntFlat.bitSize(a.ref.hashCode())

        override def encodeHC(inA: SIRType, encode: HashConsedEncoderState): Unit =
            val a = inA match
                case tp: SIRType.TypeProxy => tp
                case _ => throw IllegalStateException(s"Expected TypeProxy, have ${inA}")
            val ref = a.ref
            if ref == null then
                throw new IllegalStateException(
                  "TypeProxy id is null, looks lise save or restore is invalid"
                )
            val ihc = ref.hashCode()
            PlainIntFlat.encode(ihc, encode.encode)
            encode.hashConsed.lookup(ihc, tag) match
                case None =>
                    if (encode.debug) then println(s"TypeProxy, ihc=${ihc}, encode body")
                    // we don't know - are we have such ref. If ref is not known, then
                    // encode.hashConsed.putForwardRef(HashConsed.ForwardRefAcceptor(ihc, tag, Nil))
                    SIRTypeHashConsedFlat.encodeHC(ref, encode)
                    encode.hashConsed.lookup(ihc, tag) match
                        case None =>
                            encode.hashConsed.setRef(ihc, tag, SIRTypeHashConsedRef.fromData(ref))
                        case Some(_) =>
                case Some(_) =>
                    if (encode.debug) then println(s"TypeProxy, inc=${ihc}, already encoded")

        override def decodeHC(decode: HashConsedDecoderState): SIRTypeHashConsedRef =
            val ihc = PlainIntFlat.decode(decode.decode)
            decode.hashConsed.lookup(ihc, tag) match
                case None =>
                    val ref = SIRTypeHashConsedFlat.decodeHC(decode)
                    decode.hashConsed.lookup(ihc, tag) match
                        case None =>
                            decode.hashConsed.setRef(ihc, tag, ref)
                        case Some(_) =>
                    val typeProxy = new SIRType.TypeProxy(null)
                    decode.hashConsed.putForwardValueAcceptor(
                      ihc,
                      tag,
                      { x =>
                          typeProxy.ref = x.asInstanceOf[SIRType]
                      }
                    )
                    SIRTypeHashConsedRef.fromData(typeProxy)
                // HashConsed.ConstRef(typeProxy)
                case Some(Left(fw)) =>
                    val newRef = new HashConsed.MutRef[SIRTypeHashConsedRef](null)
                    fw.addAction(
                      decode.hashConsed,
                      (a: HashConsedRef[?]) =>
                          a match
                              case aa: SIRTypeHashConsedRef =>
                                  newRef.value = aa
                              case _ =>
                                  throw new IllegalStateException(
                                    s"Attempt to set non SIRTypeHashConsetRef to TypeProxy: ${a}"
                                  )
                    )
                    SIRTypeHashConsedRef.deferred(
                      hs => (newRef.value != null) && newRef.value.isComplete(hs),
                      (hs, l, ps) => {
                          val ref = newRef.value
                          if (ref == null) then
                              throw new IllegalStateException("Type proxy is not resolved")
                          val v = ref.finValue(hs, l + 1, ps)
                          v match
                              case proxy: SIRType.TypeProxy => proxy
                              case _                        => SIRType.TypeProxy(v)
                      }
                    )
                case Some(Right(a)) =>
                    SIRTypeHashConsedRef.deferred(
                      hs => a.isComplete(hs),
                      (hs, l, ps) => {
                          val v = a.finValue(hs, l + 1, ps).asInstanceOf[SIRType]
                          v match
                              case tpv: SIRType.TypeProxy => tpv
                              case other                  => SIRType.TypeProxy(other)
                      }
                    )

    }

    object SIRLetHashConsedFlat extends HashConsedReprFlat[SIR.Let, HashConsedRef[SIR.Let]] {

        override def toRepr(a: SIR.Let): HashConsedRef[SIR.Let] =
            HashConsedRef.fromData(a)

        def bitSizeHC(a: SIR.Let, hashConsed: HashConsed.State): Int =
            val recSize = summon[Flat[Recursivity]].bitSize(a.recursivity)
            val bindingsSize =
                HashConsedReprFlat.listRepr(BindingFlat).bitSizeHC(a.bindings, hashConsed)
            val bodySize = SIRHashConsedFlat.bitSizeHC(a.body, hashConsed)
            recSize + bindingsSize + bodySize

        def encodeHC(a: SIR.Let, encode: HashConsedEncoderState): Unit = {
            summon[Flat[Recursivity]].encode(a.recursivity, encode.encode)
            HashConsedReprFlat.listRepr(BindingFlat).encodeHC(a.bindings, encode)
            SIRHashConsedFlat.encodeHC(a.body, encode)
        }

        def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Let] = {
            val rec = summon[Flat[Recursivity]].decode(decode.decode)
            val bindings = HashConsedReprFlat.listRepr(BindingFlat).decodeHC(decode)
            val body = SIRHashConsedFlat.decodeHC(decode)
            HashConsedRef.deferred(
              hs => bindings.isComplete(hs) && body.isComplete(hs),
              (hs, l, p) => SIR.Let(rec, bindings.finValue(hs, l, p), body.finValue(hs, l, p))
            )
        }
    }

    object SIRCaseHashConsedFlat extends HashConsedReprFlat[SIR.Case, HashConsedRef[SIR.Case]] {
        private val patternTagWidth = 2
        private val patternTagConstr: Byte = 0
        private val patternTagWildcard: Byte = 1
        override def toRepr(a: SIR.Case): HashConsedRef[SIR.Case] =
            HashConsedRef.fromData(a)

        def bitSizeHC(a: SIR.Case, hashConsed: HashConsed.State): Int =
            a.pattern match
                case Pattern.Constr(constr, bindings, typeBindings) =>
                    val constrSize = ConstrDeclFlat.bitSizeHC(constr, hashConsed)
                    val bindingsSize = summon[Flat[List[String]]].bitSize(bindings)
                    val typeBindingsSize =
                        HashConsedReprFlat
                            .listRepr(SIRTypeHashConsedFlat)
                            .bitSizeHC(typeBindings, hashConsed)
                    val bodySize = SIRHashConsedFlat.bitSizeHC(a.body, hashConsed)
                    patternTagWidth + constrSize + bindingsSize + typeBindingsSize + bodySize
                case Pattern.Wildcard => // FIXME
                    patternTagWidth + SIRHashConsedFlat.bitSizeHC(a.body, hashConsed)

        def encodeHC(a: SIR.Case, encode: HashConsedEncoderState): Unit = {
            a.pattern match
                case Pattern.Constr(constr, bindings, typeBindings) =>
                    encode.encode.bits(patternTagWidth, patternTagConstr)
                    ConstrDeclFlat.encodeHC(constr, encode)
                    summon[Flat[List[String]]].encode(bindings, encode.encode)
                    HashConsedReprFlat
                        .listRepr(SIRTypeHashConsedFlat)
                        .encodeHC(typeBindings, encode)
                    SIRHashConsedFlat.encodeHC(a.body, encode)
                case Pattern.Wildcard => // FIXME
                    encode.encode.bits(patternTagWidth, patternTagWildcard)
                    SIRHashConsedFlat.encodeHC(a.body, encode)
        }

        def decodeHC(decode: HashConsedDecoderState): HashConsedRef[SIR.Case] = {
            val patternTag = decode.decode.bits8(patternTagWidth)
            patternTag match
                case `patternTagWildcard` =>
                    val body = SIRHashConsedFlat.decodeHC(decode)
                    HashConsedRef.deferred(
                      hs => body.isComplete(hs),
                      (hs, l, p) => SIR.Case(Pattern.Wildcard, body.finValue(hs, l, p))
                    )
                case `patternTagConstr` =>
                    val constr = ConstrDeclFlat.decodeHC(decode)
                    val bindings = summon[Flat[List[String]]].decode(decode.decode)
                    val typeBindings =
                        HashConsedReprFlat.listRepr(SIRTypeHashConsedFlat).decodeHC(decode)
                    val body = SIRHashConsedFlat.decodeHC(decode)
                    HashConsedRef.deferred(
                      hs =>
                          constr.isComplete(hs) && typeBindings.isComplete(hs) && body.isComplete(
                            hs
                          ),
                      (hs, l, p) =>
                          SIR.Case(
                            Pattern.Constr(
                              constr.finValue(hs, l, p),
                              bindings,
                              typeBindings.finValue(hs, l, p)
                            ),
                            body.finValue(hs, l, p)
                          )
                    )
        }
    }

    object SIRHashConsedFlat extends HashConsedReprFlat[SIR, HashConsedRef[SIR]]:
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
        final val tagConstr = 0x08
        final val tagMatch = 0x09
        final val tagExternalVar = 0x0a
        final val tagAnd = 0x0b
        final val tagOr = 0x0c
        final val tagNot = 0x0d
        final val tagDecl = 0x0e
        final val tagSelect = 0xf

        override def toRepr(a: SIR): HashConsedRef[SIR] = {
            HashConsedRef.fromData(a)
        }

        def bitSizeHC(a: SIR, hashCons: HashConsed.State): Int = a match
            case SIR.Var(name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(name) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case SIR.ExternalVar(modName, name, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(modName)
                    + summon[Flat[String]].bitSize(name)
                    + SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case aLet: SIR.Let =>
                termTagWidth +
                    SIRLetHashConsedFlat.bitSizeHC(aLet, hashCons)
            case LamAbs(x, t) =>
                termTagWidth + SIRVarHashConsedFlat.bitSizeHC(x, hashCons) + bitSizeHC(t, hashCons)
            case Apply(f, x, tp) =>
                termTagWidth + bitSizeHC(f, hashCons) + bitSizeHC(x, hashCons) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case Select(x, field, tp) =>
                termTagWidth + bitSizeHC(x, hashCons) + summon[Flat[String]].bitSize(field) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case aConst: Const => termTagWidth + SIRConstHashConsedFlat.bitSizeHC(aConst, hashCons)
            case And(x, y)     => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Or(x, y)      => termTagWidth + bitSizeHC(x, hashCons) + bitSizeHC(y, hashCons)
            case Not(x)        => termTagWidth + bitSizeHC(x, hashCons)
            case IfThenElse(c, t, f, tp) =>
                termTagWidth + bitSizeHC(c, hashCons) + bitSizeHC(t, hashCons) + bitSizeHC(
                  f,
                  hashCons
                ) +
                    SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case Builtin(bn, tp)   => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error(msg, cause) => termTagWidth + summon[Flat[String]].bitSize(msg)
            case Constr(name, data, args, tp) =>
                termTagWidth + summon[Flat[String]].bitSize(name)
                    + DataDeclFlat.bitSizeHC(data, hashCons)
                    + HashConsedReprFlat.listRepr(SIRHashConsedFlat).bitSizeHC(args, hashCons)
                    + SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case Match(scrutinee, cases, tp) =>
                termTagWidth + bitSizeHC(scrutinee, hashCons)
                    + HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).bitSizeHC(cases, hashCons)
                    + SIRTypeHashConsedFlat.bitSizeHC(tp, hashCons)
            case Decl(data, term) =>
                termTagWidth + DataDeclFlat.bitSizeHC(data, hashCons) + bitSizeHC(term, hashCons)

        def encodeHC(a: SIR, enc: HashConsedEncoderState): Unit =
            a match
                case v @ SIR.Var(name, tp) =>
                    enc.encode.bits(termTagWidth, tagVar)
                    summon[Flat[String]].encode(name, enc.encode)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case SIR.ExternalVar(modName, name, tp) =>
                    enc.encode.bits(termTagWidth, tagExternalVar)
                    summon[Flat[String]].encode(modName, enc.encode)
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
                case Select(x, field, tp) =>
                    enc.encode.bits(termTagWidth, tagSelect)
                    encodeHC(x, enc)
                    summon[Flat[String]].encode(field, enc.encode)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case cn @ Const(_, _) =>
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
                case Constr(name, data, args, tp) =>
                    enc.encode.bits(termTagWidth, tagConstr)
                    summon[Flat[String]].encode(name, enc.encode)
                    DataDeclFlat.encodeHC(data, enc)
                    HashConsedReprFlat.listRepr(SIRHashConsedFlat).encodeHC(args, enc)
                    SIRTypeHashConsedFlat.encodeHC(tp, enc)
                case Match(scrutinee, cases, tp) =>
                    enc.encode.bits(termTagWidth, tagMatch)
                    encodeHC(scrutinee, enc)
                    HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).encodeHC(cases, enc)
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
                case Decl(data, term) =>
                    enc.encode.bits(termTagWidth, tagDecl)
                    DataDeclFlat.encodeHC(data, enc)
                    encodeHC(term, enc)

        def decodeHC(decoder: HashConsedDecoderState): HashConsedRef[SIR] =
            val tag = decoder.decode.bits8(termTagWidth)
            tag match
                case `tagVar` =>
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => tp.isComplete(hs),
                      (hs, l, p) => Var(name, tp.finValue(hs, l, p))
                    )
                case `tagLet` =>
                    SIRLetHashConsedFlat.decodeHC(decoder)
                case `tagLamAbs` =>
                    val x = SIRVarHashConsedFlat.decodeHC(decoder)
                    val t = decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => x.isComplete(hs) && t.isComplete(hs),
                      (hs, l, p) => LamAbs(x.finValue(hs, l, p), t.finValue(hs, l, p))
                    )
                case `tagApply` =>
                    val f = decodeHC(decoder)
                    val x = decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => f.isComplete(hs) && x.isComplete(hs) && tp.isComplete(hs),
                      (hs, l, p) =>
                          Apply(f.finValue(hs, l, p), x.finValue(hs, l, p), tp.finValue(hs, l, p))
                    )
                case `tagSelect` =>
                    val x = decodeHC(decoder)
                    val field = summon[Flat[String]].decode(decoder.decode)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => x.isComplete(hs) && tp.isComplete(hs),
                      (hs, l, p) => Select(x.finValue(hs, l, p), field, tp.finValue(hs, l, p))
                    )
                case `tagConst` =>
                    SIRConstHashConsedFlat.decodeHC(decoder)
                case `tagIfThenElse` =>
                    val c = decodeHC(decoder)
                    val t = decodeHC(decoder)
                    val f = decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs =>
                          c.isComplete(hs) && t.isComplete(hs) && f.isComplete(hs) && tp.isComplete(
                            hs
                          ),
                      (hs, l, p) =>
                          IfThenElse(
                            c.finValue(hs, l, p),
                            t.finValue(hs, l, p),
                            f.finValue(hs, l, p),
                            tp.finValue(hs, l, p)
                          )
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
                    val args = HashConsedReprFlat.listRepr(SIRHashConsedFlat).decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => data.isComplete(hs) && args.isComplete(hs) && tp.isComplete(hs),
                      (hs, l, p) =>
                          Constr(
                            name,
                            data.finValue(hs, l, p),
                            args.finValue(hs, l, p),
                            tp.finValue(hs, l, p)
                          )
                    )
                case `tagMatch` =>
                    val scrutinee = decodeHC(decoder)
                    val cases = HashConsedReprFlat.listRepr(SIRCaseHashConsedFlat).decodeHC(decoder)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => scrutinee.isComplete(hs) && cases.isComplete(hs) && tp.isComplete(hs),
                      (hs, l, p) =>
                          Match(
                            scrutinee.finValue(hs, l, p),
                            cases.finValue(hs, l, p),
                            tp.finValue(hs, l, p)
                          )
                    )
                case `tagExternalVar` =>
                    val modName = summon[Flat[String]].decode(decoder.decode)
                    val name = summon[Flat[String]].decode(decoder.decode)
                    val tp = SIRTypeHashConsedFlat.decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => tp.isComplete(hs),
                      (hs, l, p) => ExternalVar(modName, name, tp.finValue(hs, l, p))
                    )
                case `tagAnd` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => x.isComplete(hs) && y.isComplete(hs),
                      (hs, l, p) => And(x.finValue(hs, l, p), y.finValue(hs, l, p))
                    )
                case `tagOr` =>
                    val x = decodeHC(decoder)
                    val y = decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => x.isComplete(hs) && y.isComplete(hs),
                      (hs, l, p) => Or(x.finValue(hs, l, p), y.finValue(hs, l, p))
                    )
                case `tagNot` =>
                    val x = decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => x.isComplete(hs),
                      (hs, l, p) => Not(x.finValue(hs, l, p))
                    )
                case `tagDecl` =>
                    val data = DataDeclFlat.decodeHC(decoder)
                    val term = decodeHC(decoder)
                    HashConsedRef.deferred(
                      hs => data.isComplete(hs) && term.isComplete(hs),
                      (hs, l, p) => Decl(data.finValue(hs, l, p), term.finValue(hs, l, p))
                    )

    end SIRHashConsedFlat

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
              (hs, l, p) => SIR.Var(name, tp.finValue(hs, l, p))
            )

    given Flat[SIRVarStorage] with
        override def bitSize(a: SIRVarStorage): Int =
            summon[Flat[Boolean]].bitSize(a == SIRVarStorage.Data)

        override def decode(decode: DecoderState): SIRVarStorage =
            if summon[Flat[Boolean]].decode(decode) then SIRVarStorage.Data
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
              (hs, l, p) => SIR.Const(uplcConst, tp.finValue(hs, l, p))
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
            HashConsedRef.deferred(
              hs => data.isComplete(hs) && term.isComplete(hs),
              (hs, l, p) => SIR.Decl(data.finValue(hs, l, p), term.finValue(hs, l, p))
            )

    class ModuleSerializedRef(val version: (Int, Int), val defs: HashConsedRef[List[Binding]])
        extends HashConsedRef[Module] {

        override def isComplete(hs: HashConsed.State): Boolean = defs.isComplete(hs)

        override def finValue(
            hc: HashConsed.State,
            level: Int,
            parents: HSRIdentityHashMap
        ): Module = {
            Module(version, defs.finValue(hc, level + 1, parents))
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
            val retval = ModuleHashSetReprFlat.bitSizeHC(a, hs)
            retval

        def encodeHC(a: Module, enc: HashConsedEncoderState): Unit =
            ModuleHashSetReprFlat.encodeHC(a, enc)
            enc.encode.filler()

        def decodeHC(decoder: HashConsedDecoderState): Module =
            // here we know that Module is the whole data, so decondign is finished
            try
                val repr = ModuleHashSetReprFlat.decodeHC(decoder)
                decoder.runFinCallbacks()
                val parents = new HSRIdentityHashMap
                val retval = repr.finValue(decoder.hashConsed, 0, parents)
                retval
            catch
                case scala.util.control.NonFatal(ex) =>
                    println("Can decode module ")
                    throw ex
