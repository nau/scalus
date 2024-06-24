package scalus.sir

import scalus.sir.SIRType.TypeVar

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

    case object BooleanPrimitive extends Primitive[Boolean] {
        override def uplcTpe: DefaultUni = DefaultUni.Bool
        override def show: String = "Boolean"
    }
    given BooleanPrimitive.type = BooleanPrimitive
    case object VoidPrimitive extends Primitive[Unit] {
        override def uplcTpe: DefaultUni = DefaultUni.Unit
        override def show: String = "Unit"
    }

    //sealed trait MappedBuiltin[T] extends Lifted[T] with ULPCMapped

    case object Data extends SIRType with Lifted[scalus.builtin.Data] with ULPCMapped {
        type Carrier = scalus.builtin.Data

        override def uplcTpe: DefaultUni = DefaultUni.Data
        override def show: String = "Data"
    }
    given Data.type = Data

    case class CaseClass(constrDecl: ConstrDecl, typeParams: scala.List[SIRType]) extends SIRType {

            override def show: String =
                if (typeParams.isEmpty) then
                    constrDecl.name
                else
                    s"${constrDecl.name}[${typeParams.map(_.show).mkString(", ")}]"

    }

    case class SumCaseClass(decl: DataDecl, typeParams: scala.List[SIRType]) extends SIRType {
        override def show: String =
            if (typeParams.isEmpty) then
                decl.name
            else
                s"${decl.name}[${typeParams.map(_.show).mkString(", ")}]"
    }

    case class Fun(in:SIRType, out: SIRType) extends SIRType {

        override type Carrier = in.Carrier => out.Carrier

        override def show: String = s"${in.show} -> ${out.show}"

    }


    given liftFun1[A,B](using a: SIRType.Aux[A], b: SIRType.Aux[B]): SIRType.Aux[A => B] =
        Fun(a, b).asInstanceOf[SIRType.Aux[A => B]]
    //given liftFun2[A,B,C](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C]): SIRType.Aux[(A, B) => C] =
    //    Fun(Tuple(List(a, b)), c).asInstanceOf[SIRType.Aux[(A, B) => C]]
    //given liftFun3[A,B,C,D](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D]): SIRType.Aux[(A, B, C) => D] =
    //    Fun(Tuple(List(a, b, c)), d).asInstanceOf[SIRType.Aux[(A, B, C) => D]]
    //given liftFun4[A,B,C,D,E](using a: SIRType.Aux[A], b: SIRType.Aux[B], c: SIRType.Aux[C], d: SIRType.Aux[D], e: SIRType.Aux[E]): SIRType.Aux[(A, B, C, D) => E] =
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
            ConstrDecl("Pair", SIRVarStorage.LocalUPLC,
                scala.List(TypeBinding("fst",A),TypeBinding("snd",B)),
                scala.List(A,B)
            )
        }

        def apply(a: SIRType, b: SIRType): SIRType =
            CaseClass(constrDecl, scala.List(a,b))


        def unapply(x:SIRType): Option[(SIRType, SIRType)] = x match {
            case CaseClass(`constrDecl`,scala.List(a,b)) => Some((a,b))
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
    case class TypeLambda(params: scala.List[TypeVar], body: SIRType) extends SIRType {

        override def show: String = s" [${params.map(_.show).mkString(",")}] =>> ${body}"

    }

    case class TypeError(msg: String) extends SIRType {
        override def show: String = s"Error: $msg"
    }
    //given liftLambda1List: SIRType.Aux[[A] =>> List[A]] = ???
       // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => scalus.list(a.head)).asInstanceOf[SIRType.Aux[[A] =>> List[A]]]
       //  TODO: implement predefined constants and type cache.
    //given liftBoolAA: SIRType.Aux[[A] =>> (Boolean,A,A)] = ???
         // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => Tuple(List(a.head, a.head, a.head))).asInstanceOf[SIRType.Aux[[A] =>> (Boolean,A,A)]]


    object List {

        lazy val dataDecl = DataDecl("List",
            scala.List(Cons.constr, NilConstr),
            scala.List(TypeVar("A"))
        )

        def apply(a: SIRType): SIRType =
            SumCaseClass(dataDecl, scala.List(a))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case SumCaseClass(`dataDecl`,List(a)) => Some(a)
            case this.Cons(a) => Some(a)
            case this.Nil => Some(VoidPrimitive)
            case _ => None
        }
        
        object Cons {

            lazy val constr = {
                val a = TypeVar("A")
                ConstrDecl("Cons", SIRVarStorage.LocalUPLC,
                    scala.List(TypeBinding("head", a), TypeBinding("tail", List(a))),
                    scala.List(a)
                )
            }


            def apply(a: SIRType) = CaseClass(constr, scala.List(a))


            def unapply(x:SIRType): Option[SIRType] = x match {
                case CaseClass(`constr`,scala.List(a)) => Some(a)
                case _ => None
            }
            
        }

        val NilConstr = ConstrDecl("Nil", SIRVarStorage.LocalUPLC, scala.Nil, scala.Nil)

        val Nil = CaseClass(NilConstr, scala.Nil)



        
    }
    
    //def lift[T](implicit ev: SIRType.Aux[T]): SIRType.Aux[T] = ev
    
    inline def liftM[T <: AnyKind]: SIRType.Aux[T] = ${liftMImpl[T]}

    def liftMImpl[T<:AnyKind:Type](using Quotes): Expr[SIRType.Aux[T]] = {
        import quotes.reflect.*

        println(s"liftM: ${TypeRepr.of[T].show}")

        def liftRepr(tp: TypeRepr, enclosingBounds: Map[Int,TypeVar]): Expr[SIRType] = {
            println(s"liftRepr: ${tp.show}")
            tp match
                case typeLambda@quotes.reflect.TypeLambda(paramNames, paramBounds, resType) =>
                    println(s"type-lambda detected, tp.symbol = ${tp.typeSymbol}, tp.symbol.hashCode=${tp.typeSymbol.hashCode}, Type: ${tp.show}")
                    // we need to generate param names with unique ids.
                    //println(s"tp.hashCode = ${tp.hashCode}")
                    //println(s"resType = ${resType.show}")
                    var newEnclosingBounds = enclosingBounds
                    val sirParamExprs = paramNames.zipWithIndex.map {
                        case (name, idx) =>
                            val id = typeLambda.param(idx).typeSymbol.hashCode
                            newEnclosingBounds = newEnclosingBounds + (id -> TypeVar(name, Some(id)))
                            '{ TypeVar(${ Expr(name) }, Some(${ Expr(id) })) }
                    }
                    '{ SIRType.TypeLambda(${ Expr.ofList(sirParamExprs) }, ${ liftRepr(resType, newEnclosingBounds) }) }
                case pr@quotes.reflect.ParamRef(p, n) =>
                    println(s"ParamRef detected,  Type: ${tp.show}, p=$p, p.typeSymbol.hashCode = ${p.typeSymbol.hashCode} n=$n")
                    val name = tp.show // TODO: get from enclosing lambda.
                    val id = pr.typeSymbol.hashCode
                    enclosingBounds.get(id) match
                        case Some(value) =>
                        case None =>
                            report.error(s"No enclosing lambda found for typevar ${tp.show}", Position.ofMacroExpansion)
                    '{ TypeVar(${ Expr(name) }, Some(${ Expr(id) })) }
                case other =>
                    tp.asType match {
                        case '[scalus.builtin.ByteString] =>
                            '{ByteStringPrimitive}.asExprOf[SIRType.Aux[scalus.builtin.ByteString]]
                        case '[BigInt] =>
                            '{IntegerPrimitive}.asExprOf[SIRType.Aux[BigInt]]
                        case '[String] => '{StringPrimitive}.asExprOf[SIRType.Aux[String]]
                        case '[Boolean] => '{BooleanPrimitive}.asExprOf[SIRType.Aux[Boolean]]
                        case '[Unit] => '{VoidPrimitive}.asExprOf[SIRType.Aux[Unit]]
                        case '[scalus.builtin.Data] => '{Data}.asExprOf[SIRType.Aux[scalus.builtin.Data]]
                        case '[a => b] =>
                            //println(s"Fun detected,  Type: ${tp.show}")
                            val in = TypeRepr.of[a]
                            val out = TypeRepr.of[b]
                            '{Fun(${liftRepr(in,enclosingBounds)}, ${liftRepr(out, enclosingBounds)}) }
                        case '[(a,b)=>c] =>
                            println(s"Fun2 detected,  Type: ${tp.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            '{ Fun(${ liftRepr(TypeRepr.of[a], enclosingBounds) },
                                Fun(${ liftRepr(TypeRepr.of[b], enclosingBounds) },
                                    ${ liftRepr(TypeRepr.of[c], enclosingBounds)}) ) }
                        case '[(a,b,c)=>d] =>
                            println(s"Fun3 detected,  Type: ${tp.show}")
                            val ta = TypeRepr.of[a]
                            val tb = TypeRepr.of[b]
                            val tc = TypeRepr.of[c]
                            val td = TypeRepr.of[d]
                            println(s"ta: ${ta.show}, tb: ${tb.show}, tc: ${tc.show}, td: ${td.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            '{ Fun(${ liftRepr(TypeRepr.of[a], enclosingBounds) },
                                Fun(${ liftRepr(TypeRepr.of[b], enclosingBounds) },
                                 Fun( ${ liftRepr(TypeRepr.of[c], enclosingBounds) },
                                     ${ liftRepr(TypeRepr.of[d], enclosingBounds) })))
                            }
                        case '[(a,b,c,d)=>e] =>
                            println(s"Fun4 detected,  Type: ${tp.show}")
                            '{ Fun(${ liftRepr(TypeRepr.of[a], enclosingBounds) },
                                Fun(${ liftRepr(TypeRepr.of[b], enclosingBounds) },
                                    Fun( ${ liftRepr(TypeRepr.of[c], enclosingBounds) },
                                        Fun( ${ liftRepr(TypeRepr.of[d], enclosingBounds) },
                                            ${ liftRepr(TypeRepr.of[e], enclosingBounds) }))))
                            }
                        case '[scalus.builtin.Pair[a,b]] =>
                            println(s"Builtin pair detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            '{SIRType.Pair(${liftRepr(a, enclosingBounds)}, ${liftRepr(b, enclosingBounds)})}
                        case '[(a,b)] =>
                            println(s"Tuple detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            '{SIRType.Pair(${liftRepr(a, enclosingBounds)}, ${liftRepr(b, enclosingBounds)})}
                        case '[List[a]] =>
                            println(s"List detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            '{SIRType.List(${liftRepr(a, enclosingBounds)})}
                        case '[Option[a]] =>
                            ???
                        case other =>
                            println(s"Unrecognized type: ${tp.show} tree: $other")
                            report.error(s"Unsupported type [1]: ${tp.show} in ${TypeRepr.of[T].show}", Position.ofMacroExpansion)
                            ???
            }
        }

        '{  ${liftRepr(TypeRepr.of[T], Map.empty)}.asInstanceOf[SIRType.Aux[T]] }.asExprOf[SIRType.Aux[T]]
    }

    
    
}

