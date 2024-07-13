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

    case class CaseClass(constrDecl: ConstrDecl, typeArgs: scala.List[SIRType]) extends SIRType {

            override def show: String =
                if (typeArgs.isEmpty) then
                    constrDecl.name
                else
                    s"${constrDecl.name}[${typeArgs.map(_.show).mkString(", ")}]"

    }

    case class SumCaseClass(decl: DataDecl, typeArgs: scala.List[SIRType]) extends SIRType {
        override def show: String =
            if (typeArgs.isEmpty) then
                decl.name
            else
                s"${decl.name}[${typeArgs.map(_.show).mkString(", ")}]"
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
                scala.List(A,B),
                scala.Nil
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


    case object FreeUnificator extends SIRType {
        override def show: String = "*"
    }

    case class TypeProxy(var ref: SIRType|Null) extends SIRType {
        override def show: String = s"Proxy(${ref.show})"
    }

    object List {

        lazy val dataDecl: DataDecl = {
            val proxy = new TypeProxy(null)
            val aInCons = TypeVar("A", Some(1))
            val retval = DataDecl("List",
                scala.List(Cons.buildConstr(aInCons, proxy), NilConstr),
                scala.List(TypeVar("A", Some(2)))
            )
            proxy.ref = SumCaseClass(retval, scala.List(TypeVar("A", Some(1))))
            retval
        }


        def apply(a: SIRType): SIRType =
            SumCaseClass(dataDecl, scala.List(a))

        def unapply(l: SIRType): Option[SIRType] = l match {
            case SumCaseClass(`dataDecl`,List(a)) => Some(a)
            case this.Cons(a) => Some(a)
            case this.Nil => Some(VoidPrimitive)
            case _ => None
        }
        
        object Cons {

            def buildConstr(a: TypeVar, listSum: SIRType): ConstrDecl = {
                ConstrDecl("Cons", SIRVarStorage.LocalUPLC,
                    scala.List(TypeBinding("head", a), TypeBinding("tail", listSum)),
                    scala.List(a),
                    scala.List(a)
                )
            }

            // TODO:  remove duplication via cache
            lazy val constr = {
                dataDecl.constructors.find(_.name == "Cons").getOrElse(
                    throw new IllegalStateException("Cons constructor not found in List.dataDecl")
                )
            }




            def apply(a: SIRType) = CaseClass(constr, scala.List(a))


            def unapply(x:SIRType): Option[SIRType] = x match {
                case CaseClass(`constr`,scala.List(a)) => Some(a)
                case _ => None
            }
            
        }

        val NilConstr = ConstrDecl("Nil", SIRVarStorage.LocalUPLC, scala.Nil, scala.Nil, scala.Nil)

        val Nil = CaseClass(NilConstr, scala.Nil)

        
    }
    
    def calculateApplyType(f: SIRType, arg: SIRType, env: Map[TypeVar,SIRType]): SIRType = f match {
        case Fun(in, out) =>
            unify(in, arg, env) match
                case r: SuccessfulTypeUnificationResult[?] => substitute(out,r.env, Map.empty)
                case e: ErroredTypeUnificationResult => TypeError(e.msg)
                case EmptyTypeUnificationResult => TypeError(s"Cannot unify $in with $arg")
        case tvF@TypeVar(name, _) =>
            env.get(tvF) match
                case Some(f1) => calculateApplyType(tvF, arg, env)
                case None => TypeError(s"Unbound type variable $name")
        case TypeLambda(params, body) =>
            val newEnv = params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            calculateApplyType(body, arg, newEnv)
        case TypeProxy(next) =>
            if (next == null) then
                TypeError(s"TypeProxy is not resolved: $f")
            else
                calculateApplyType(next, arg, env)
        case other =>
            TypeError(s"Expected function type, got $other")
    }

    sealed trait TypeUnificationResult[+T] {
        def map[S](f: T => S): TypeUnificationResult[S]
        def flatMap[S](f: T=> TypeUnificationResult[S]): TypeUnificationResult[S]
    }

    object TypeUnificationResult {

      def empty: EmptyTypeUnificationResult.type = EmptyTypeUnificationResult

      def error(msg: String): ErroredTypeUnificationResult = ErroredTypeUnificationResult(msg)

      def success[T](unificator: T, env: Map[TypeVar,SIRType]): SuccessfulTypeUnificationResult[T] =
          SuccessfulTypeUnificationResult(unificator, env)

    }

    object EmptyTypeUnificationResult extends TypeUnificationResult[Nothing] {
        def map[S](f: Nothing => S): TypeUnificationResult[S] = this.asInstanceOf[TypeUnificationResult[S]]

        override def flatMap[S](f: Nothing => TypeUnificationResult[S]): TypeUnificationResult[S] =
            this.asInstanceOf[TypeUnificationResult[S]]
    }

    case class ErroredTypeUnificationResult(msg: String) extends TypeUnificationResult[Nothing] {
        override def map[S](f: Nothing => S): TypeUnificationResult[S] =
            this.asInstanceOf[TypeUnificationResult[S]]

        override def flatMap[S](f: Nothing => TypeUnificationResult[S]): TypeUnificationResult[S] =
            this.asInstanceOf[TypeUnificationResult[S]]
    }

    case class SuccessfulTypeUnificationResult[T](unificator: T, env: Map[TypeVar,SIRType]) extends TypeUnificationResult[T] {
        def map[S](f: T => S): TypeUnificationResult[S] =
                SuccessfulTypeUnificationResult(f(unificator), env)
        override def flatMap[S](f: T => TypeUnificationResult[S]): TypeUnificationResult[S] =
            f(unificator)
    }

    def unify(left: SIRType, right: SIRType, env: Map[TypeVar,SIRType]): TypeUnificationResult[SIRType] = {
        
        def checkLeftTypeVar(tv: TypeVar): TypeUnificationResult[SIRType] =
            env.get(tv) match
                case Some(tv1) =>
                    if (tv1 == FreeUnificator) then
                        SuccessfulTypeUnificationResult(right, env + (tv -> right))
                    else
                        unify(tv1, right, env)
                case None =>
                    TypeUnificationResult.error(s"Unbound type variable $tv")

        def checkRightTypeVar(tv: TypeVar): TypeUnificationResult[SIRType] =
            env.get(tv) match
                case Some(t) =>
                    if t == FreeUnificator then
                        TypeUnificationResult.success(left, env + (tv -> left))
                    else
                        unify(left, t, env)
                case None =>
                    TypeUnificationResult.error(s"Unbound type variable $tv")


        def checkLeftTypeLambda(tl: TypeLambda): TypeUnificationResult[SIRType] =
            val newEnv = tl.params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            unify(tl.body, right, newEnv)

        def checkRightTypeLambda(tl: TypeLambda): TypeUnificationResult[SIRType] =
            val newEnv = tl.params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            unify(left, tl.body, newEnv)

        def checkLeftProxy(ltp: TypeProxy): TypeUnificationResult[SIRType] =
            if ltp.ref == null then
                ErroredTypeUnificationResult(s"Unresolved type proxy $ltp")
            else
                unify(ltp.ref, right, env)

        def checkRightProxy(rtp: TypeProxy): TypeUnificationResult[SIRType] =
            if rtp.ref == null then
                ErroredTypeUnificationResult(s"Unresolved type proxy $rtp")
            else
                unify(left, rtp.ref, env)

        def checkRightNoSame: TypeUnificationResult[SIRType] =
            right match
                case tv: TypeVar =>
                    checkRightTypeVar(tv)
                case tl@TypeLambda(params, body) =>
                    checkRightTypeLambda(tl)
                case tp: TypeProxy =>
                    checkRightProxy(tp)
                case FreeUnificator =>
                    TypeUnificationResult.success(left, env)
                case te: TypeError =>
                    TypeUnificationResult.error(te.msg)
                case _ => TypeUnificationResult.empty

        def unifyList(left: List[SIRType], right: List[SIRType], env: Map[TypeVar,SIRType]): TypeUnificationResult[List[SIRType]] = {
           val s0: TypeUnificationResult[scala.List[SIRType]] = TypeUnificationResult.success(scala.List.empty, env)
           val r = left.zip(right).foldLeft(s0) {
               case (SuccessfulTypeUnificationResult(accUnificator, env), (l, r)) =>
                   unify(l, r, env) match
                       case SuccessfulTypeUnificationResult(unificator, env) =>
                           SuccessfulTypeUnificationResult(unificator :: accUnificator, env)
                       case e@ErroredTypeUnificationResult(msg) => e
                       case EmptyTypeUnificationResult => EmptyTypeUnificationResult
               case (e@ErroredTypeUnificationResult(msg), _) => e
               case (e@EmptyTypeUnificationResult, _) => e
           }
           r.map(_.reverse)
        }

        left match
            case leftP: Primitive[?] =>
                right match
                    case rightP: Primitive[?] =>
                        if leftP == rightP then
                            TypeUnificationResult.success(leftP, env)
                        else
                            TypeUnificationResult.empty
                    case _ =>
                        checkRightNoSame
            case Data =>
                right match
                    case Data => TypeUnificationResult.success(Data, env)
                    case other => checkRightNoSame
            case ccl@CaseClass(constrDecl, typeArgs) =>
                right match
                    case rrl@CaseClass(constrDeclRight, typeArgsRight) =>
                        if constrDecl == constrDeclRight then
                            unifyList(typeArgs,typeArgsRight, env).map(CaseClass(constrDecl, _))
                        else
                            TypeUnificationResult.empty
                    case SumCaseClass(decl, typeArgsRight) =>
                        decl.constructors.find(_ == constrDecl) match
                            case Some(_) =>
                                val nEnv = constrDecl.typeParams.zip(typeArgs).foldLeft(env) {
                                    case (acc, (tv,t)) => acc + (tv -> t)
                                }
                                unifyList(constrDecl.parentTypeArgs, typeArgsRight, nEnv).map(SumCaseClass(decl, _))
                            case None => TypeUnificationResult.empty
                    case _ =>
                        checkRightNoSame
            case SumCaseClass(declLeft, typeArgsLeft) =>
                right match
                    case SumCaseClass(declRight, typeArgsRight) =>
                        if declLeft == declRight then
                            unifyList(typeArgsLeft, typeArgsRight, env).map(SumCaseClass(declLeft, _))
                        else
                            TypeUnificationResult.empty
                    case ccr@CaseClass(constrDeclRight, typeArgsRight) =>
                        declLeft.constructors.find(_ == constrDeclRight) match
                            case Some(_) =>
                                val nEnv = constrDeclRight.typeParams.zip(typeArgsRight).foldLeft(env) {
                                    case (acc, (tv,t)) => acc + (tv -> t)
                                }
                                unifyList(typeArgsLeft, constrDeclRight.parentTypeArgs, nEnv).map(CaseClass(constrDeclRight, _))
                            case None => TypeUnificationResult.empty
                    case _ =>
                        checkRightNoSame
            case Fun(inLeft,outLeft) =>
                right match
                    case Fun(inRight, outRight) =>
                        unify(inLeft, inRight, env) match
                            case SuccessfulTypeUnificationResult(unificator, env) =>
                                unify(outLeft, outRight, env).map(Fun(inLeft, _))
                            case e@ErroredTypeUnificationResult(msg) => e
                            case EmptyTypeUnificationResult => EmptyTypeUnificationResult
                    case _ =>
                        checkRightNoSame
            case tvl@TypeVar(name, _) =>
                checkLeftTypeVar(tvl)
            case tll@TypeLambda(params, body) =>
                checkLeftTypeLambda(tll)
            case TypeError(msg) =>
                TypeUnificationResult.error(msg)
            case FreeUnificator =>
                TypeUnificationResult.success(right, env)
            case tp: TypeProxy =>
                checkLeftProxy(tp)
    }


    def substitute(rType: SIRType, env: Map[SIRType.TypeVar, SIRType], proxyEnv:Map[SIRType.TypeProxy,SIRType.TypeProxy]): SIRType = {
        rType match
            case tv@TypeVar(name, _) =>
                env.get(tv) match
                    case Some(t) => t
                    case None => tv
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
                        newProxy.ref = substitute(tp.ref, env, proxyEnv.updated(tp,newProxy))
                        newProxy
            case other =>
                other
    }

    def fromDefaultUni(uplcType: DefaultUni): SIRType = {
        uplcType match
            case DefaultUni.ByteString => ByteStringPrimitive
            case DefaultUni.Integer => IntegerPrimitive
            case DefaultUni.String => StringPrimitive
            case DefaultUni.Bool => BooleanPrimitive
            case DefaultUni.Unit => VoidPrimitive
            case DefaultUni.Data => Data
            case DefaultUni.ProtoList => List(FreeUnificator)
            case DefaultUni.ProtoPair => Pair(FreeUnificator, FreeUnificator)
            case DefaultUni.Apply(f, arg) =>
                SIRType.Fun(fromDefaultUni(f), fromDefaultUni(arg))
    }


    inline def liftM[T <: AnyKind]: SIRType.Aux[T] = ${liftMImpl[T]}

    def liftMImpl[T<:AnyKind:Type](using Quotes): Expr[SIRType.Aux[T]] = {
        import quotes.reflect.*

        def liftRepr(tp: TypeRepr, enclosingBounds: Map[Int,TypeVar]): Expr[SIRType] = {
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
                        case '[(a,b,c,d,e)=>f] =>
                            println(s"Fun5 detected,  Type: ${tp.show}")
                            '{ Fun(${ liftRepr(TypeRepr.of[a], enclosingBounds) },
                                Fun(${ liftRepr(TypeRepr.of[b], enclosingBounds) },
                                    Fun( ${ liftRepr(TypeRepr.of[c], enclosingBounds) },
                                        Fun( ${ liftRepr(TypeRepr.of[d], enclosingBounds) },
                                            Fun( ${ liftRepr(TypeRepr.of[e], enclosingBounds) },
                                                ${ liftRepr(TypeRepr.of[f], enclosingBounds) })))))
                            }
                        case '[(a,b,c,d,e,f)=>g] =>
                            println(s"Fun6 detected,  Type: ${tp.show}")
                            '{ Fun(${ liftRepr(TypeRepr.of[a], enclosingBounds) },
                                Fun(${ liftRepr(TypeRepr.of[b], enclosingBounds) },
                                    Fun( ${ liftRepr(TypeRepr.of[c], enclosingBounds) },
                                        Fun( ${ liftRepr(TypeRepr.of[d], enclosingBounds) },
                                            Fun( ${ liftRepr(TypeRepr.of[e], enclosingBounds) },
                                                Fun( ${ liftRepr(TypeRepr.of[f], enclosingBounds) },
                                                    ${ liftRepr(TypeRepr.of[g], enclosingBounds) }))))))
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
                        case '[scalus.builtin.List[a]] =>
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

