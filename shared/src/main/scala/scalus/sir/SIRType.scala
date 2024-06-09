package scalus.sir

import scala.quoted.*
import scalus.uplc.DefaultUni


sealed trait SIRType {

    type Carrier

    def show: String

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

    case object BoolPrimitive extends Primitive[Boolean] {
        override def uplcTpe: DefaultUni = DefaultUni.Bool
        override def show: String = "Bool"
    }
    given BoolPrimitive.type = BoolPrimitive
    case object VoidPrimitive extends Primitive[Unit] {
        override def uplcTpe: DefaultUni = DefaultUni.Unit
        override def show: String = "Unit"
    }

    sealed trait MappedBuiltin[T] extends Lifted[T] with ULPCMapped

    case object Data extends MappedBuiltin[scalus.builtin.Data] {
        type Carrier = scalus.builtin.Data

        override def uplcTpe: DefaultUni = DefaultUni.Data
        override def show: String = "Data"
    }
    given Data.type = Data

    case class Sum(name: String, childs: List[SIRType]) extends SIRType {
        override def show: String = s"Sum($name, ${childs.map(_.show).mkString(", ")})"
    }


    case class CaseClass(constrDecl: ConstrDecl) extends SIRType

    case class SumCaseClass(decl: DataDecl) extends SIRType

    case class Ref(i: Int) extends SIRType {
        // TODO: add type-environment for show
        override def show: String = s"Ref($i)"
    }

    case class Fun(in:SIRType, out: SIRType) extends SIRType {

        override type Carrier = in.Carrier => out.Carrier

        override def show: String = s"${in.show} -> ${out.show}"

    }

    given liftFun1[A,B](using a: SIRType.Aux[A], b: SIRType.Aux[B]): SIRType.Aux[A => B] =
        Fun(a, b).asInstanceOf[SIRType.Aux[A => B]]
    given liftFun2[A,B,C](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C]): SIRType.Aux[(A, B) => C] =
        Fun(Tuple(List(a, b)), c).asInstanceOf[SIRType.Aux[(A, B) => C]]
    given liftFun3[A,B,C,D](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D]): SIRType.Aux[(A, B, C) => D] =
        Fun(Tuple(List(a, b, c)), d).asInstanceOf[SIRType.Aux[(A, B, C) => D]]
    given liftFun4[A,B,C,D,E](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D], e: SIRType.Aux[E]): SIRType.Aux[(A, B, C, D) => E] =
        Fun(Tuple(List(a, b, c, d)), e).asInstanceOf[SIRType.Aux[(A, B, C, D) => E]]

    case class Tuple(fields: List[SIRType]) extends SIRType {

        override def show: String = s"(${fields.map(_.show).mkString(", ")})"

    }
    given liftTuple2[A,B](using a: SIRType.Aux[A], b: SIRType.Aux[B]): SIRType.Aux[(A, B)] =
        Tuple(List(a, b)).asInstanceOf[SIRType.Aux[(A, B)]]
    given liftTuple3[A,B,C](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C]): SIRType.Aux[(A, B, C)] =
        Tuple(List(a, b, c)).asInstanceOf[SIRType.Aux[(A, B, C)]]
    given liftTuple4[A,B,C,D](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D]): SIRType.Aux[(A, B, C, D)] =
        Tuple(List(a, b, c, d)).asInstanceOf[SIRType.Aux[(A, B, C, D)]]



    object Pair {
        def apply(a: SIRType, b: SIRType): SIRType = CaseClass(
            ConstrDecl("Pair", SIRVarStorage.LocalUPLC,
                scala.List(TypeBinding("fst", a),
                           TypeBinding("snd", b)
                )
            )
        )
        def unapply(x:SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(ConstrDecl("Pair", SIRVarStorage.LocalUPLC, scala.List(TypeBinding("fst", a), TypeBinding("snd", b))) ) => Some((a,b))
            case _ => None
        }
    }

    /**
     * Type variable have two forms:
     *   when id is not set,  that means that for each instantiation of type-lambda,
     *   a new set of type-variables with fresh id-s are created.
     *   when id is set, that means that computations are situated in the process of instantiation
     *   of some type-lambda,
     * @param name
     * @param id
     */
    case class TypeVar(name: String, id: Option[Long] = None) extends SIRType {

        override def show: String = name

    }

    /**
     * Type lamnda (always carried).
     * @param param
     * @param body
     */
    case class TypeLambda(param: TypeVar, body: SIRType) extends SIRType {

        override def show: String = s" [${param.show}] =>> ${body}"

    }
    //given liftLambda1List: SIRType.Aux[[A] =>> List[A]] = ???
       // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => scalus.list(a.head)).asInstanceOf[SIRType.Aux[[A] =>> List[A]]]
       //  TODO: implement predefined constants and type cache.
    //given liftBoolAA: SIRType.Aux[[A] =>> (Boolean,A,A)] = ???
         // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => Tuple(List(a.head, a.head, a.head))).asInstanceOf[SIRType.Aux[[A] =>> (Boolean,A,A)]]




    object List {
        
        def apply(a: SIRType): SIRType = Sum("List",  scala.List(this.Cons(a), this.Nil))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case Sum("List", scala.List(this.Cons(a),this.Nil)) => Some(a)
            case this.Cons(a) => Some(a)
            case this.Nil => Some(VoidPrimitive)
            case _ => None
        }
        
        object Cons {
            def apply(a: SIRType): SIRType = CaseClass(
              ConstrDecl("Cons", SIRVarStorage.LocalUPLC,
                  scala.List(TypeBinding("head", a),
                             TypeBinding("tail", List(a))
                  )
              )
            )
            
            def unapply(x:SIRType): Option[SIRType] = x match {
                case CaseClass(ConstrDecl("Cons", SIRVarStorage.LocalUPLC, scala.List(TypeBinding("head", a), TypeBinding("tail", this.List(b))) ) ) => Some(a)
                case _ => None
            }
            
        }

        val Nil = CaseClass(ConstrDecl("Nil", SIRVarStorage.LocalUPLC, scala.Nil))


        
    }
    
    def lift[T](implicit ev: SIRType.Aux[T]): SIRType.Aux[T] = ev
    
    inline def liftM[T]: SIRType.Aux[T] = ${liftMImpl[T]}

    def liftMImpl[T:Type](using Quotes): Expr[SIRType.Aux[T]] = {
        import quotes.reflect.*

        ???
    }

    
    
}

