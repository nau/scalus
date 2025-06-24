package scalus.sir

import scalus.uplc.DefaultUni
import scalus.uplc.TypeScheme as UplcTypeScheme
import scalus.sir.SIRType.TypeVar

import java.util

sealed trait SIRType {

    def show: String

    def ~=~(that: SIRType): Boolean =
        SIRUnify.unifyType(this, that, SIRUnify.Env.empty).isSuccess

    def ->:(that: SIRType): SIRType.Fun =
        SIRType.Fun(that, this)

    inline def =>>:(that: SIRType): SIRType.TypeLambda =
        this match
            case x: SIRType.TypeVar => SIRType.TypeLambda(scala.List(x), that)
            case other =>
                throw new IllegalArgumentException(
                  s"Expected type variable at the left of =>>:, got $other"
                )

}

sealed trait SIRVarStorage

object SIRVarStorage {
    case object Data extends SIRVarStorage
    case object ScottEncoding extends SIRVarStorage
    case object Cases extends SIRVarStorage

    val DEFAULT = ScottEncoding
}

object SIRType {

    sealed trait Primitive extends SIRType {
        def uplcType: DefaultUni
    }

    case object ByteString extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.ByteString
        override def show: String = "ByteString"
    }
    given ByteString.type = ByteString

    case object Integer extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Integer
        override def show: String = "Int"
    }
    given Integer.type = Integer

    case object String extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.String
        override def show: String = "String"
    }
    given String.type = String

    case object Boolean extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Bool
        override def show: String = "Boolean"
    }
    given Boolean.type = Boolean
    case object Unit extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Unit
        override def show: String = "Unit"
    }

    // sealed trait MappedBuiltin[T] extends Lifted[T] with ULPCMapped

    case object Data extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.Data
        override def show: String = "Data"
    }
    given Data.type = Data

    case object BLS12_381_G1_Element extends Primitive {
        override def uplcType: DefaultUni = DefaultUni.BLS12_381_G1_Element
        override def show: String = "BLS12_381_G1_Element"
    }

    case object BLS12_381_G2_Element extends Primitive {

        override def uplcType: DefaultUni = DefaultUni.BLS12_381_G2_Element
        override def show: String = "BLS12_381_G2_Element"
    }

    case object BLS12_381_MlResult extends Primitive {

        override def uplcType: DefaultUni = DefaultUni.BLS12_381_MlResult
        override def show: String = "BLS12_381_MlResult"
    }

    case class CaseClass(
        constrDecl: ConstrDecl,
        typeArgs: scala.List[SIRType],
        parent: Option[SIRType]
    ) extends SIRType {

        override def show: String =
            if typeArgs.isEmpty then constrDecl.name
            else s"${constrDecl.name}[${typeArgs.map(_.show).mkString(", ")}]"

    }

    case class SumCaseClass(decl: DataDecl, typeArgs: scala.List[SIRType]) extends SIRType {

        override def show: String =
            if typeArgs.isEmpty then decl.name
            else s"${decl.name}[${typeArgs.map(_.show).mkString(", ")}]"

    }

    case class Fun(in: SIRType, out: SIRType) extends SIRType {

        override def show: String = {
            in match {
                case inF: Fun =>
                    s"(${in.show}) -> ${out.show}"
                case SIRType.TypeLambda(params, body: Fun) =>
                    s"(${in.show}) -> ${body.show}"
                case _ =>
                    s"${in.show} -> ${out.show}"
            }

        }

    }

    object Pair {

        val constrDecl = {
            val A = TypeVar("A", None, true)
            val B = TypeVar("B", None, true)
            ConstrDecl(
              "Pair",
              scala.List(TypeBinding("fst", A), TypeBinding("snd", B)),
              scala.List(A, B),
              scala.Nil,
              AnnotationsDecl.empty
            )
        }

        def apply(a: SIRType, b: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a, b), None)

        def unapply(x: SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(`constrDecl`, scala.List(a, b), None) => Some((a, b))
            case _                                               => None
        }

    }

    /** Type variable have two forms: when id is not set, that means that for each instantiation of
      * type-lambda, a new set of type-variables with fresh id-s are created. when id is set, that
      * means that computations are situated in the process of instantiation of some type-lambda,
      * @param name
      * @param id
      * @param isBuiltin - if true, then this type variable is a type variable of a builtin uplc function, if false, then
      *                  this is Scala type variable, which can be represented in uplc in a special form.
      *                   Note, that buildin type variables can come only from Builtin Functions.
      */
    case class TypeVar(name: String, optId: Option[Long] = None, isBuiltin: Boolean)
        extends SIRType {

        override def show: String = name

        def :=>>(body: SIRType): TypeLambda = TypeLambda(scala.List(this), body)

    }

    /** Type lamnda (always carried).
      * @param param
      * @param body
      */
    case class TypeLambda(params: scala.List[TypeVar], body: SIRType) extends SIRType {

        override def show: String = s" [${params.map(_.show).mkString(",")}] =>> ${body.show}"

    }

    object TypeLambda {
        def apply(param: String, body: TypeVar => SIRType, isBuiltin: Boolean): TypeLambda = {
            val tv = TypeVar(param, None, isBuiltin)
            TypeLambda(scala.List(tv), body(tv))
        }
    }

    object TypeLambda2 {
        def apply(
            param1: String,
            param2: String,
            body: (TypeVar, TypeVar) => SIRType,
            isBuiltin: Boolean
        ): TypeLambda = {
            val tv1 = TypeVar(param1, None, isBuiltin)
            val tv2 = TypeVar(param2, None, isBuiltin)
            TypeLambda(scala.List(tv1, tv2), body(tv1, tv2))
        }
    }

    case object FreeUnificator extends SIRType {
        override def show: String = "*"
    }

    class TypeProxy(private var _ref: SIRType | Null) extends SIRType {

        val ex = new RuntimeException("type-proxy-created")

        override def hashCode(): Int = {
            if ref == null then 0
            else
                // do not call ref.hashCode because it will be recursively called
                //  TODO: actually this beeak case-class contract
                //    (equal recursive base-classes become unequal)
                //    (maybe pass some hashcode to the constructor)

                // in principle now we can change this to summon[SIRHashCodeInRec[TypeProxy]].newRec(ref)
                //  but let's wait
                java.lang.System.identityHashCode(ref)

        }

        override def show: String = {
            val internal =
                if ref eq null then "null" else java.lang.System.identityHashCode(ref).toString
            s"Proxy($internal)"
        }

        def ref: SIRType | Null = _ref

        def ref_=(value: SIRType | Null): Unit = {
            value match
                case tp: TypeProxy =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be assigned to TypeProxy: $tp, tp.ref=${tp.ref}"
                    )
                case _ =>
            _ref = value
        }

    }

    object TypeProxy {

        def apply(ref: SIRType | Null): TypeProxy = {
            ref match
                case TypeProxy(ref1) =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be created from another TypeProxy: $ref1"
                    )
                case _: Primitive =>
                    throw new IllegalArgumentException(
                      s"TypeProxy cannot be created from Primitive: $ref"
                    )
                case _ =>
            val proxy = new TypeProxy(ref)
            ref match {
                case tp: TypeProxy =>
                    proxy.ref = tp.ref
                case _ =>
            }
            proxy
        }

        def unapply(tp: SIRType): Option[SIRType | Null] = tp match {
            case tp: TypeProxy => Some(tp.ref)
            case _             => None
        }

    }

    /** Represents a package or companion object of a case class. Note, that case objects going to
      * case-class.
      * @param name
      */
    case class TypeNonCaseModule(name: String) extends SIRType {
        override def show: String = s"PackageOrEnclosingObject($name)"
    }

    case object TypeNothing extends SIRType {
        override def show: String = "Nothing"
    }

    object List {

        lazy val dataDecl: DataDecl = {
            val proxy = new TypeProxy(null)
            val aInCons = TypeVar("A", Some(1), false)
            val retval = DataDecl(
              "scalus.prelude.List",
              scala.List(NilConstr, Cons.buildConstr(aInCons, proxy)),
              scala.List(TypeVar("A", Some(2), false)),
              AnnotationsDecl.empty
            )
            proxy.ref = SumCaseClass(retval, scala.List(TypeVar("A", Some(1), false)))
            if !checkAllProxiesFilled(retval.tp) then
                throw new IllegalStateException(s"List dataDecl has unfilled proxies: ${retval.tp}")
            retval
        }

        def apply(a: SIRType): SumCaseClass =
            SumCaseClass(dataDecl, scala.List(a))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case SumCaseClass(dataDecl, scala.List(a)) =>
                if dataDecl.name == "scalus.prelude.List" then Some(a)
                else None
            case this.Cons(a) => Some(a)
            case this.Nil     => Some(Unit)
            case _            => None
        }

        object Cons {

            def buildConstr(a: TypeVar, listSum: SIRType): ConstrDecl = {
                ConstrDecl(
                  "scalus.prelude.List$.Cons",
                  scala.List(TypeBinding("head", a), TypeBinding("tail", listSum)),
                  scala.List(a),
                  scala.List(a),
                  AnnotationsDecl.empty
                )
            }

            // TODO:  remove duplication via cache
            lazy val constr = {
                dataDecl.constructors
                    .find(_.name == "scalus.prelude.List$.Cons")
                    .getOrElse(
                      throw new IllegalStateException("Cons constructor not found in List.dataDecl")
                    )
            }

            def apply(a: SIRType) =
                CaseClass(constr, scala.List(a), Some(List.apply(a)))

            def unapply(x: SIRType): Option[SIRType] = x match {
                case CaseClass(constr, scala.List(a), _) =>
                    if constr.name == "scalus.prelude.List$.Cons" then Some(a)
                    else None
                case _ => None
            }

        }

        // val NilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, scala.Nil, scala.Nil, scala.Nil)
        val NilConstr =
            ConstrDecl(
              "scalus.prelude.List$.Nil",
              scala.Nil,
              scala.Nil,
              scala.List(SIRType.TypeNothing),
              AnnotationsDecl.empty
            )

        val Nil = CaseClass(NilConstr, scala.Nil, Some(List.apply(SIRType.TypeNothing)))

    }

    /** Check if the type is a function or a polymorphic function without unfolding type arguments.
      * (i.e. isPolyFunOrFun(SIRType.Fun(SIRType.Unit, SIRType.Unit)) == true,
      * isPolyFunOrFun(SIRType.TypeVar("A")) == false even if A is a needed function)
      * @param tp - type to check
      * @param trace - trace for recursive entries
      * @return
      */
    def isPolyFunOrFun(
        tp: SIRType,
        trace: java.util.IdentityHashMap[SIRType, SIRType] = new util.IdentityHashMap()
    ): Boolean = {
        if trace.containsKey(tp) then false
        else
            trace.put(tp, tp)
            tp match {
                case SIRType.Fun(_, _)           => true
                case SIRType.TypeLambda(_, body) => isPolyFunOrFun(body, trace)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then false
                    else isPolyFunOrFun(ref, trace)
                case _ => false
            }
    }

    /** Check if the type is a function or a polymorphic function  from unit without unfolding type arguments.
      * (i.e. isPolyFunOrFunUnit(SIRType.Fun(SIRType.Unit, SIRType.Unit)) == true,
      *       isPolyFunOrFunInut(SIRType.Fun(SIRType.Integer, SIRType.Unit)) == false,
      * isPolyFunOrFunUnt(SIRType.TypeVar("A")) == false even if A is a needed function)
      *
      * @param tp    - type to check
      * @param trace - trace for recursive entries
      * @return
      */
    def isPolyFunOrFunUnit(
        tp: SIRType,
        trace: java.util.IdentityHashMap[SIRType, SIRType] = new util.IdentityHashMap()
    ): Boolean = {
        if trace.containsKey(tp) then false
        else
            trace.put(tp, tp)
            tp match
                case SIRType.Fun(SIRType.Unit, _) => true
                case SIRType.TypeLambda(_, body)  => isPolyFunOrFunUnit(body, trace)
                case SIRType.TypeProxy(ref) =>
                    if ref == null then false
                    else isPolyFunOrFunUnit(ref, trace)
                case _ => false
    }

    case class TypeApplyException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    def typeApply(tpl: SIRType, args: List[SIRType]): SIRType =
        if args.isEmpty then tpl
        else
            tpl match
                case TypeLambda(params, body) =>
                    if params.length != args.length then
                        throw TypeApplyException(
                          s"length of type-lambda parameter list is differ then args list. tpl=${tpl.show}, args=${args
                                  .map(_.show)}"
                        )
                    else
                        val env = params.zip(args).toMap
                        substitute(body, env, Map.empty)
                case TypeProxy(ref) =>
                    typeApply(ref, args)
                case _ =>
                    throw TypeApplyException(
                      s"type-lambda should be the first argument of typeApply,  we have ${tpl.show}"
                    )

    case class CaclulateApplyTypeException(msg: String, cause: Throwable | Null = null)
        extends RuntimeException(msg, cause)

    def calculateApplyType(
        f: SIRType,
        arg: SIRType,
        env: Map[TypeVar, SIRType],
        debug: Boolean = false
    ): SIRType =
        f match {
            case Fun(in, out) =>
                // TODO: check unification exceptions and rethrow as type exception
                SIRUnify.unifyType(
                  in,
                  arg,
                  SIRUnify.Env.empty.copy(filledTypes = env, debug = debug)
                ) match
                    case r: SIRUnify.UnificationSuccess[?] =>
                        substitute(out, r.env.filledTypes, Map.empty)
                    case e: SIRUnify.UnificationFailure[?] =>
                        val message = s"Cannot unify $in with $arg, difference at path ${e.path}"
                        println(s"fun=$f")
                        println(s"in=$in")
                        println(s"arg=$arg")
                        throw new CaclulateApplyTypeException(message)
            // TypeError(s"Cannot unify $in with $arg, difference at path ${e.path}", null)
            case tvF: TypeVar =>
                env.get(tvF) match
                    case Some(f1) => calculateApplyType(tvF, arg, env)
                    case None =>
                        throw new CaclulateApplyTypeException(s"Unbound type variable ${tvF.name}")
            case TypeLambda(params, body) =>
                val newEnv = params.foldLeft(env) { case (acc, tv) =>
                    acc + (tv -> FreeUnificator)
                }
                calculateApplyType(body, arg, newEnv, debug)
            case TypeProxy(next) =>
                if next == null then
                    throw CaclulateApplyTypeException(s"TypeProxy is not resolved: $f")
                else calculateApplyType(next, arg, env, debug)
            case other =>
                throw CaclulateApplyTypeException(s"Expected function type, got $other", null)
        }

    def substitute(
        rType: SIRType,
        env: Map[SIRType.TypeVar, SIRType],
        proxyEnv: Map[SIRType.TypeProxy, SIRType.TypeProxy]
    ): SIRType = {
        rType match
            case tv: TypeVar =>
                env.get(tv) match
                    case Some(t) => t
                    case None    => tv
            case TypeLambda(params, body) =>
                TypeLambda(params, substitute(body, env, proxyEnv))
            case CaseClass(constrDecl, typeArgs, optParent) =>
                CaseClass(
                  constrDecl,
                  typeArgs.map(substitute(_, env, proxyEnv)),
                  optParent.map(c => substitute(c, env, proxyEnv))
                )
            case SumCaseClass(decl, typeArgs) =>
                SumCaseClass(decl, typeArgs.map(substitute(_, env, proxyEnv)))
            case Fun(in, out) =>
                Fun(substitute(in, env, proxyEnv), substitute(out, env, proxyEnv))
            case tp: TypeProxy =>
                proxyEnv.get(tp) match
                    case Some(t) => t
                    case None =>
                        val newProxy = new TypeProxy(null)
                        newProxy.ref = substitute(tp.ref, env, proxyEnv.updated(tp, newProxy))
                        newProxy
            case other =>
                other
    }

    def fromDefaultUni(uplcType: DefaultUni): SIRType = {
        uplcType match
            case DefaultUni.ByteString           => ByteString
            case DefaultUni.Integer              => Integer
            case DefaultUni.String               => String
            case DefaultUni.Bool                 => Boolean
            case DefaultUni.Unit                 => Unit
            case DefaultUni.Data                 => Data
            case DefaultUni.BLS12_381_G1_Element => BLS12_381_G1_Element
            case DefaultUni.BLS12_381_G2_Element => BLS12_381_G2_Element
            case DefaultUni.BLS12_381_MlResult   => BLS12_381_MlResult
            case DefaultUni.ProtoList =>
                val a = TypeVar("A", Some(DefaultUni.ProtoList.hashCode()), true)
                TypeLambda(scala.List(a), SumCaseClass(List.dataDecl, scala.List(a)))
            case DefaultUni.ProtoPair =>
                val a = TypeVar("A", Some(DefaultUni.ProtoPair.hashCode()), true)
                val b = TypeVar("B", Some(DefaultUni.ProtoPair.hashCode() + 1), true)
                TypeLambda(scala.List(a, b), Pair(a, b))
            case DefaultUni.Apply(f, arg) =>
                f match
                    case DefaultUni.ProtoList =>
                        List(fromDefaultUni(arg))
                    case DefaultUni.Apply(DefaultUni.ProtoPair, a) =>
                        Pair(fromDefaultUni(a), fromDefaultUni(arg))
                    case DefaultUni.ProtoPair =>
                        val a = TypeVar("A", Some(DefaultUni.ProtoPair.hashCode()), true)
                        TypeLambda(scala.List(a), Pair(a, fromDefaultUni(arg)))
                    case _ =>
                        SIRType.Fun(fromDefaultUni(f), fromDefaultUni(arg))
    }

    def fromUplcTypeScheme(uplcTypeSchema: UplcTypeScheme): SIRType = {
        uplcTypeSchema match
            case UplcTypeScheme.Type(argType) => fromDefaultUni(argType)
            case UplcTypeScheme.Arrow(argType, resType) =>
                Fun(fromUplcTypeScheme(argType), fromUplcTypeScheme(resType))
            case UplcTypeScheme.All(typeVar, body) =>
                TypeLambda(scala.List(TypeVar(typeVar, None, true)), fromUplcTypeScheme(body))
            case UplcTypeScheme.App(f, arg) =>
                calculateApplyType(fromUplcTypeScheme(f), fromUplcTypeScheme(arg), Map.empty)
            case UplcTypeScheme.TVar(name) => TypeVar(name, None, true)
    }

    def parentsEqSeq(input: SIRType, parent: SIRType): List[SIRType] = {
        SIRUnify.subtypeSeq(input, parent, SIRUnify.Env.empty)
    }

    def parentsNoEqSeq(input: SIRType, parent: SIRType): List[SIRType] = {
        parentsEqSeq(input, parent) match
            case Nil => Nil
            case x :: xs =>
                if x ~=~ parent then xs
                else x :: xs
    }

    def leastUpperBound(left: SIRType, right: SIRType): SIRType = {
        SIRUnify.unifyType(left, right, SIRUnify.Env.empty.withUpcasting) match {
            case SIRUnify.UnificationSuccess(env, res) => res
            case SIRUnify.UnificationFailure(_, _, _) =>
                SIRType.FreeUnificator
        }
    }

    @scala.annotation.tailrec
    def retrieveDataDecl(tp: SIRType): Either[String, DataDecl] = {
        tp match {
            case tp: SumCaseClass => Right(tp.decl)
            case TypeProxy(ref) =>
                if ref == null then Left("TypeProxy is not resolved")
                else retrieveDataDecl(ref)
            case TypeLambda(_, body) => retrieveDataDecl(body)
            case _                   => Left(s"Expected SumCaseClass, got $tp")
        }
    }

    def retrieveConstrDecl(tp: SIRType): Either[String, ConstrDecl] = {
        tp match {
            case tp: CaseClass => Right(tp.constrDecl)
            case TypeProxy(ref) =>
                if ref == null then Left("TypeProxy is not resolved")
                else retrieveConstrDecl(ref)
            case TypeLambda(_, body) => retrieveConstrDecl(body)
            case _ => Left(s"Expected CaseClass, got ${tp.show} (${tp.getClass.getSimpleName}")
        }
    }

    def checkAllProxiesFilled(tp: SIRType): Boolean = {
        checkAllProxiesFilledTraced(tp, new util.IdentityHashMap[SIRType, SIRType], Nil)
    }

    def checkAllProxiesFilledTraced(
        tp: SIRType,
        traceloops: java.util.IdentityHashMap[SIRType, SIRType],
        log: List[String]
    ): Boolean = {
        if traceloops.get(tp) != null then true
        else
            traceloops.put(tp, tp)
            tp match
                case tp: TypeProxy =>
                    if tp.ref == null then
                        println("Found null typeproxe, created at: ")
                        tp.ex.printStackTrace()
                        false
                    else if traceloops.get(tp.ref) != null then
                        checkAllProxiesFilledTraced(tp.ref, traceloops, log)
                    else true
                case Fun(in, out) =>
                    checkAllProxiesFilledTraced(in, traceloops, "in" :: log) &&
                    checkAllProxiesFilledTraced(out, traceloops, "out" :: log)
                case CaseClass(constrDecl, typeArgs, optParent) =>
                    constrDecl.params.forall(x =>
                        checkAllProxiesFilledTraced(
                          x.tp,
                          traceloops,
                          s"${constrDecl.name} param" :: log
                        )
                    ) &&
                    typeArgs.forall(x =>
                        checkAllProxiesFilledTraced(x, traceloops, "typeArgs" :: log)
                    )
                case SumCaseClass(dataDecl: DataDecl, typeArgs) =>
                    checkAllProxiesFilledTraced(
                      dataDecl.tp,
                      traceloops,
                      s"dataDecl ${dataDecl.name}" :: log
                    ) &&
                    dataDecl.constructors.forall { constrDecl =>
                        constrDecl.params.forall(x =>
                            checkAllProxiesFilledTraced(
                              x.tp,
                              traceloops,
                              s"${constrDecl.name}" :: log
                            )
                        )
                    } &&
                    typeArgs.forall(x =>
                        checkAllProxiesFilledTraced(x, traceloops, "typeArgs" :: log)
                    )
                case TypeLambda(params, body) =>
                    checkAllProxiesFilledTraced(body, traceloops, "type-lambda bofy" :: log)
                case _ => true
    }

    def syntheticNarrowConstrDeclName(childDataDeclName: String): String = {
        s"z_narrow$$${childDataDeclName}"
    }

    def isSynteticNarrowConstrDeclName(name: String): Boolean = {
        name.startsWith("z_narrow$$")
    }

}
