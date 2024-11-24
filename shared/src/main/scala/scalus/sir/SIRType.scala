package scalus.sir

import scala.util.control.NonFatal
import scala.quoted.*
import scalus.uplc.DefaultUni
import scalus.sir.SIRType.TypeVar

sealed trait SIRType {

    type Carrier

    def show: String
    
    def ~=~(that: SIRType): Boolean =
        SIRUnify.unifyType(this, that, SIRUnify.Env.empty).isSuccess

    // def recursiveHashCode(env: Map[Int, SIRType]): Int

}

sealed trait SIRVarStorage

object SIRVarStorage {
    case object Data extends SIRVarStorage
    case object LocalUPLC extends SIRVarStorage

    val DEFAULT = LocalUPLC
}

object SIRType {

    type Aux[T <: AnyKind] = SIRType {
        type Carrier = T
    }

    trait ULPCMapped {
        this: SIRType =>
        def uplcTpe: DefaultUni
    }

    sealed trait Lifted[T] extends SIRType {
        type Carrier = T
    }

    sealed trait Primitive[T] extends Lifted[T] with ULPCMapped

    case object ByteStringPrimitive extends Primitive[scalus.builtin.ByteString] {
        override def uplcTpe: DefaultUni = DefaultUni.ByteString
        override def show: String = "ByteString"
    }
    given ByteStringPrimitive.type = ByteStringPrimitive

    case object IntegerPrimitive extends Primitive[BigInt] {
        override def uplcTpe: DefaultUni = DefaultUni.Integer
        override def show: String = "Int"
    }
    given IntegerPrimitive.type = IntegerPrimitive

    case object StringPrimitive extends Primitive[String] {
        override def uplcTpe: DefaultUni = DefaultUni.String
        override def show: String = "String"
    }
    given StringPrimitive.type = StringPrimitive

    case object BooleanPrimitive extends Primitive[Boolean] {
        override def uplcTpe: DefaultUni = DefaultUni.Bool
        override def show: String = "Boolean"
    }
    given BooleanPrimitive.type = BooleanPrimitive
    case object VoidPrimitive extends Primitive[Unit] {
        override def uplcTpe: DefaultUni = DefaultUni.Unit
        override def show: String = "Unit"
    }

    // sealed trait MappedBuiltin[T] extends Lifted[T] with ULPCMapped

    case object Data extends SIRType with Lifted[scalus.builtin.Data] with ULPCMapped {
        type Carrier = scalus.builtin.Data

        override def uplcTpe: DefaultUni = DefaultUni.Data
        override def show: String = "Data"
    }
    given Data.type = Data
    
    case object BLS12_381_G1_Element extends SIRType with Lifted[scalus.builtin.BLS12_381_G1_Element] with ULPCMapped {
        type Carrier = scalus.builtin.BLS12_381_G1_Element

        override def uplcTpe: DefaultUni = DefaultUni.BLS12_381_G1_Element
        override def show: String = "BLS12_381_G1_Element"
    }
    
    case object BLS12_381_G2_Element extends SIRType with Lifted[scalus.builtin.BLS12_381_G2_Element] with ULPCMapped {
        type Carrier = scalus.builtin.BLS12_381_G2_Element

        override def uplcTpe: DefaultUni = DefaultUni.BLS12_381_G2_Element
        override def show: String = "BLS12_381_G2_Element"
    }
    
    case object BLS12_381_MlResult extends SIRType with Lifted[scalus.builtin.BLS12_381_MlResult] with ULPCMapped {
        type Carrier = scalus.builtin.BLS12_381_MlResult

        override def uplcTpe: DefaultUni = DefaultUni.BLS12_381_MlResult
        override def show: String = "BLS12_381_MlResult"
    }
    
    case class CaseClass(constrDecl: ConstrDecl, typeArgs: scala.List[SIRType]) extends SIRType {

        override def show: String =
            if (typeArgs.isEmpty) then constrDecl.name
            else s"${constrDecl.name}[${typeArgs.map(_.show).mkString(", ")}]"

    }

    case class SumCaseClass(decl: DataDecl, typeArgs: scala.List[SIRType]) extends SIRType {
        override def show: String =
            if (typeArgs.isEmpty) then decl.name
            else s"${decl.name}[${typeArgs.map(_.show).mkString(", ")}]"
    }

    case class Fun(in: SIRType, out: SIRType) extends SIRType {

        override type Carrier = in.Carrier => out.Carrier

        override def show: String = s"${in.show} -> ${out.show}"

    }

    given liftFun1[A, B](using a: SIRType.Aux[A], b: SIRType.Aux[B]): SIRType.Aux[A => B] =
        Fun(a, b).asInstanceOf[SIRType.Aux[A => B]]
    // given liftFun2[A,B,C](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C]): SIRType.Aux[(A, B) => C] =
    //    Fun(Tuple(List(a, b)), c).asInstanceOf[SIRType.Aux[(A, B) => C]]
    // given liftFun3[A,B,C,D](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D]): SIRType.Aux[(A, B, C) => D] =
    //    Fun(Tuple(List(a, b, c)), d).asInstanceOf[SIRType.Aux[(A, B, C) => D]]
    // given liftFun4[A,B,C,D,E](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D], e: SIRType.Aux[E]): SIRType.Aux[(A, B, C, D) => E] =
    //    Fun(Tuple(List(a, b, c, d)), e).asInstanceOf[SIRType.Aux[(A, B, C, D) => E]]

    /*
    case class Tuple(fields: List[SIRType]) extends SIRType {

        override def show: String = s"(${fields.map(_.show).mkString(", ")})"

    }
    given liftTuple2[A,B](using a: SIRType.Aux[A], b: SIRType.Aux[B]): SIRType.Aux[(A, B)] =
        Tuple(List(a, b)).asInstanceOf[SIRType.Aux[(A, B)]]
    given liftTuple3[A,B,C](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C]): SIRType.Aux[(A, B, C)] =
        Tuple(List(a, b, c)).asInstanceOf[SIRType.Aux[(A, B, C)]]
    given liftTuple4[A,B,C,D](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D]): SIRType.Aux[(A, B, C, D)] =
        Tuple(List(a, b, c, d)).asInstanceOf[SIRType.Aux[(A, B, C, D)]]
     */

    object Pair {

        val constrDecl = {
            val A = TypeVar("A")
            val B = TypeVar("B")
            ConstrDecl(
              "Pair",
              SIRVarStorage.LocalUPLC,
              scala.List(TypeBinding("fst", A), TypeBinding("snd", B)),
              scala.List(A, B) // ,
              // scala.Nil
            )
        }

        def apply(a: SIRType, b: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a, b))

        def unapply(x: SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(`constrDecl`, scala.List(a, b)) => Some((a, b))
            case _                                         => None
        }

    }

    /** Type variable have two forms: when id is not set, that means that for each instantiation of
      * type-lambda, a new set of type-variables with fresh id-s are created. when id is set, that
      * means that computations are situated in the process of instantiation of some type-lambda,
      * @param name
      * @param id
      */
    case class TypeVar(name: String, optId: Option[Long] = None) extends SIRType {

        override def show: String = name

    }

    /** Type lamnda (always carried).
      * @param param
      * @param body
      */
    case class TypeLambda(params: scala.List[TypeVar], body: SIRType) extends SIRType {

        override def show: String = s" [${params.map(_.show).mkString(",")}] =>> ${body}"

    }

    case class TypeError(msg: String, cause: Throwable | Null) extends SIRType {
        override def show: String =
            if (cause eq null) then s"Error: $msg"
            else s"Error: $msg\nCause: ${cause.getMessage}"
    }

    // given liftLambda1List: SIRType.Aux[[A] =>> List[A]] = ???
    // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => scalus.list(a.head)).asInstanceOf[SIRType.Aux[[A] =>> List[A]]]
    //  TODO: implement predefined constants and type cache.
    // given liftBoolAA: SIRType.Aux[[A] =>> (Boolean,A,A)] = ???
    // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => Tuple(List(a.head, a.head, a.head))).asInstanceOf[SIRType.Aux[[A] =>> (Boolean,A,A)]]

    case object FreeUnificator extends SIRType {
        override def show: String = "*"
    }

    class TypeProxy(private var _ref: SIRType | Null) extends SIRType {

        override def hashCode(): Int = {
            if (ref == null) then 0
            else
                // do not call ref.hashCode because it will be recursively called
                //  TODO: actually this beeak case-class contract
                //    (equal recursive base-classes become unequal)
                //    (maybe pass some hashcode to the constructor)
                java.lang.System.identityHashCode(ref)
        }

        override def show: String = {
            val internal =
                if (ref eq null) then "null" else java.lang.System.identityHashCode(ref).toString
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
                case _: Primitive[?] =>
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
            val aInCons = TypeVar("A", Some(1))
            val retval = DataDecl(
              "List",
              scala.List(Cons.buildConstr(aInCons, proxy), NilConstr),
              scala.List(TypeVar("A", Some(2)))
            )
            proxy.ref = SumCaseClass(retval, scala.List(TypeVar("A", Some(1))))
            retval
        }

        def apply(a: SIRType): SIRType =
            SumCaseClass(dataDecl, scala.List(a))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case SumCaseClass(dataDecl, scala.List(a)) =>
                if (dataDecl.name == "List") then Some(a)
                else None
            case this.Cons(a) => Some(a)
            case this.Nil     => Some(VoidPrimitive)
            case _            => None
        }

        object Cons {

            def buildConstr(a: TypeVar, listSum: SIRType): ConstrDecl = {
                ConstrDecl(
                  "Cons",
                  SIRVarStorage.LocalUPLC,
                  scala.List(TypeBinding("head", a), TypeBinding("tail", listSum)),
                  scala.List(a)
                  // scala.List(a)
                )
            }

            // TODO:  remove duplication via cache
            lazy val constr = {
                dataDecl.constructors
                    .find(_.name == "Cons")
                    .getOrElse(
                      throw new IllegalStateException("Cons constructor not found in List.dataDecl")
                    )
            }

            def apply(a: SIRType) = CaseClass(constr, scala.List(a))

            def unapply(x: SIRType): Option[SIRType] = x match {
                case CaseClass(constr, scala.List(a)) =>
                    if (constr.name == "Cons") then Some(a)
                    else None
                case _ => None
            }

        }

        // val NilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, scala.Nil, scala.Nil, scala.Nil)
        val NilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, scala.Nil, scala.Nil)

        val Nil = CaseClass(NilConstr, scala.Nil)

    }

    def calculateApplyType(f: SIRType, arg: SIRType, env: Map[TypeVar, SIRType], debug: Boolean = false): SIRType =
        f match {
            case Fun(in, out) =>
                // TODO: check unification exceptions and rethrow as type exception
                SIRUnify.unifyType(in, arg, SIRUnify.Env.empty.copy(filledTypes = env, debug=debug)) match
                    case r: SIRUnify.UnificationSuccess[?] => substitute(out, r.env.filledTypes, Map.empty)
                    case e: SIRUnify.UnificationFailure[?] => 
                                                TypeError(s"Cannot unify $in with $arg, difference at path ${e.path}", null)
            case tvF @ TypeVar(name, _) =>
                env.get(tvF) match
                    case Some(f1) => calculateApplyType(tvF, arg, env)
                    case None     => TypeError(s"Unbound type variable $name", null)
            case TypeLambda(params, body) =>
                val newEnv = params.foldLeft(env) { case (acc, tv) =>
                    acc + (tv -> FreeUnificator)
                }
                calculateApplyType(body, arg, newEnv, debug)
            case TypeProxy(next) =>
                if (next == null) then TypeError(s"TypeProxy is not resolved: $f", null)
                else calculateApplyType(next, arg, env, debug)
            case other =>
                TypeError(s"Expected function type, got $other", null)
        }
    
    def substitute(
        rType: SIRType,
        env: Map[SIRType.TypeVar, SIRType],
        proxyEnv: Map[SIRType.TypeProxy, SIRType.TypeProxy]
    ): SIRType = {
        rType match
            case tv @ TypeVar(name, _) =>
                env.get(tv) match
                    case Some(t) => t
                    case None    => tv
            case TypeLambda(params, body) =>
                TypeLambda(params, substitute(body, env, proxyEnv))
            case CaseClass(constrDecl, typeArgs) =>
                CaseClass(constrDecl, typeArgs.map(substitute(_, env, proxyEnv)))
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
            case DefaultUni.ByteString => ByteStringPrimitive
            case DefaultUni.Integer    => IntegerPrimitive
            case DefaultUni.String     => StringPrimitive
            case DefaultUni.Bool       => BooleanPrimitive
            case DefaultUni.Unit       => VoidPrimitive
            case DefaultUni.Data       => Data
            case DefaultUni.ProtoList  => List(FreeUnificator)
            case DefaultUni.ProtoPair  => Pair(FreeUnificator, FreeUnificator)
            case DefaultUni.Apply(f, arg) =>
                SIRType.Fun(fromDefaultUni(f), fromDefaultUni(arg))
    }

}
