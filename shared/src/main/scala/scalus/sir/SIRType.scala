package scalus.sir


import scala.util.control.NonFatal
import scala.quoted.*
import scalus.uplc.DefaultUni
import scalus.sir.SIRType.TypeVar



sealed trait SIRType {

    type Carrier

    def show: String

    //def recursiveHashCode(env: Map[Int, SIRType]): Int

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
    case class TypeVar(name: String, optId: Option[Long] = None) extends SIRType {

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

    case class TypeError(msg: String, cause: Throwable|Null) extends SIRType {
        override def show: String =
           if (cause eq null) then
               s"Error: $msg"
           else
               s"Error: $msg\nCause: ${cause.getMessage}"
    }

    //given liftLambda1List: SIRType.Aux[[A] =>> List[A]] = ???
       // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => scalus.list(a.head)).asInstanceOf[SIRType.Aux[[A] =>> List[A]]]
       //  TODO: implement predefined constants and type cache.
    //given liftBoolAA: SIRType.Aux[[A] =>> (Boolean,A,A)] = ???
         // TypeLambda(List(TypeVar("A")), (a: Seq[SIRType]) => Tuple(List(a.head, a.head, a.head))).asInstanceOf[SIRType.Aux[[A] =>> (Boolean,A,A)]]


    case object FreeUnificator extends SIRType {
        override def show: String = "*"
    }

    class TypeProxy(private var _ref: SIRType|Null) extends SIRType {

        override def hashCode(): Int = {
            if (ref==null) then
                0
            else
                // do not call ref.hashCode because it will be recursively called
                //  TODO: actually this beeak case-class contract
                //    (equal recursive base-classes become unequal)
                //    (maybe pass some hashcode to the constructor)
                java.lang.System.identityHashCode(ref)
        }

        override def show: String = {
            val internal = if (ref eq null) then "null" else java.lang.System.identityHashCode(ref).toString
            s"Proxy($internal)"
        }
        
        def ref: SIRType|Null = _ref
        
        def ref_=(value: SIRType|Null): Unit = {
            value match
                case tp: TypeProxy =>
                    throw new IllegalArgumentException(s"TypeProxy cannot be assigned to TypeProxy: $tp, tp.ref=${tp.ref}")
                case _ =>    
            _ref = value
        }
        
        
    }

    object TypeProxy {
        
        def apply(ref: SIRType|Null): TypeProxy = {
            ref match
                case TypeProxy(ref1) => 
                    throw new IllegalArgumentException(s"TypeProxy cannot be created from another TypeProxy: $ref1")
                case _ : Primitive[?] =>
                    throw new IllegalArgumentException(s"TypeProxy cannot be created from Primitive: $ref")    
                case _ =>    
            val proxy = new TypeProxy(ref)
            ref match {
                case tp: TypeProxy =>
                    proxy.ref = tp.ref
                case _ =>
            }
            proxy
        }

        def unapply(tp: SIRType): Option[SIRType|Null] = tp match {
            case tp: TypeProxy => Some(tp.ref)
            case _ => None
        }
        
        
    }

    /**
     * Represents a package or companion object of a case class.
     * Note, that case objects going to case-class.
     * @param name
     */
    case class TypeNonCaseModule(name:String) extends SIRType {
        override def show: String = s"PackageOrEnclosingObject($name)"
    }

    case object TypeNothing extends SIRType {
        override def show: String = "Nothing"
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
            case SumCaseClass(dataDecl,scala.List(a)) =>
                if (dataDecl.name == "List") then
                    Some(a)
                else
                    None
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
                case CaseClass(constr,scala.List(a)) =>
                    if (constr.name == "Cons") then
                        Some(a)
                    else
                        None
                case _ => None
            }
            
        }

        val NilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, scala.Nil, scala.Nil, scala.Nil)

        val Nil = CaseClass(NilConstr, scala.Nil)

        
    }
    
    def calculateApplyType(f: SIRType, arg: SIRType, env: Map[TypeVar,SIRType]): SIRType = f match {
        case Fun(in, out) =>
            unify(in, arg, env) match
                case r: SuccessfulUnificationResult[?] => substitute(out,r.env, Map.empty)
                case e: ErroredUnificationResult => TypeError(e.msg, null)
                case EmptyUnificationResult => TypeError(s"Cannot unify $in with $arg", null)
        case tvF@TypeVar(name, _) =>
            env.get(tvF) match
                case Some(f1) => calculateApplyType(tvF, arg, env)
                case None => TypeError(s"Unbound type variable $name", null)
        case TypeLambda(params, body) =>
            val newEnv = params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            calculateApplyType(body, arg, newEnv)
        case TypeProxy(next) =>
            if (next == null) then
                TypeError(s"TypeProxy is not resolved: $f", null)
            else
                calculateApplyType(next, arg, env)
        case other =>
            TypeError(s"Expected function type, got $other", null)
    }

    sealed trait UnificationResult[+T] {
        def isDefined: Boolean
        def map[S](f: T => S): UnificationResult[S]
        def flatMap[S](f: T=> UnificationResult[S]): UnificationResult[S]
    }

    object UnificationResult {

      def empty: EmptyUnificationResult.type = EmptyUnificationResult

      def error(msg: String): ErroredUnificationResult = ErroredUnificationResult(msg)

      def success[T](unificator: T, env: Map[TypeVar,SIRType]): SuccessfulUnificationResult[T] =
          SuccessfulUnificationResult(unificator, env)

    }

    object EmptyUnificationResult extends UnificationResult[Nothing] {
        
        override def isDefined: Boolean = false
        
        def map[S](f: Nothing => S): UnificationResult[S] = this.asInstanceOf[UnificationResult[S]]

        override def flatMap[S](f: Nothing => UnificationResult[S]): UnificationResult[S] =
            this.asInstanceOf[UnificationResult[S]]
    }

    case class ErroredUnificationResult(msg: String) extends UnificationResult[Nothing] {
        
        override def isDefined: Boolean = false
        
        override def map[S](f: Nothing => S): UnificationResult[S] =
            this.asInstanceOf[UnificationResult[S]]

        override def flatMap[S](f: Nothing => UnificationResult[S]): UnificationResult[S] =
            this.asInstanceOf[UnificationResult[S]]
    }

    case class SuccessfulUnificationResult[T](unificator: T, env: Map[TypeVar,SIRType]) extends UnificationResult[T] {
        
        override def isDefined: Boolean = true
        
        def map[S](f: T => S): UnificationResult[S] =
                SuccessfulUnificationResult(f(unificator), env)
        override def flatMap[S](f: T => UnificationResult[S]): UnificationResult[S] =
            f(unificator)
    }

    def unify(left: SIRType, right: SIRType, env: Map[TypeVar,SIRType]): UnificationResult[SIRType] = {

        println(s"unify: left: ${left.show}, right: ${right.show}, env: $env")

        def checkLeftTypeVar(tv: TypeVar): UnificationResult[SIRType] =
            right match
                case tvRight: TypeVar if (tv == tvRight) =>
                    SuccessfulUnificationResult(right, env)
                case _ =>
                    env.get(tv) match
                        case Some(tv1) =>
                            if (tv1 == FreeUnificator) then
                                SuccessfulUnificationResult(right, env + (tv -> right))
                            else
                                unify(tv1, right, env)
                        case None =>
                            UnificationResult.error(s"Unbound type variable $tv")

        def checkRightTypeVar(tv: TypeVar): UnificationResult[SIRType] =
            env.get(tv) match
                case Some(t) =>
                    if t == FreeUnificator then
                        UnificationResult.success(left, env + (tv -> left))
                    else
                        unify(left, t, env)
                case None =>
                    UnificationResult.error(s"Unbound type variable $tv")


        def checkLeftTypeLambda(tl: TypeLambda): UnificationResult[SIRType] =
            val newEnv = tl.params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            unify(tl.body, right, newEnv)

        def checkRightTypeLambda(tl: TypeLambda): UnificationResult[SIRType] =
            val newEnv = tl.params.foldLeft(env) {
                case (acc, tv) => acc + (tv -> FreeUnificator)
            }
            unify(left, tl.body, newEnv)

        def checkLeftProxy(ltp: TypeProxy): UnificationResult[SIRType] =
            if ltp.ref == null then
                ErroredUnificationResult(s"Unresolved type proxy $ltp")
            else
                unify(ltp.ref, right, env)

        def checkRightProxy(rtp: TypeProxy): UnificationResult[SIRType] =
            if rtp.ref == null then
                ErroredUnificationResult(s"Unresolved type proxy $rtp")
            else
                unify(left, rtp.ref, env)

        def checkRightNoSame: UnificationResult[SIRType] =
            right match
                case tv: TypeVar =>
                    checkRightTypeVar(tv)
                case tl@TypeLambda(params, body) =>
                    checkRightTypeLambda(tl)
                case tp: TypeProxy =>
                    checkRightProxy(tp)
                case FreeUnificator =>
                    UnificationResult.success(left, env)
                case te: TypeError =>
                    UnificationResult.error(te.msg)
                case _ => UnificationResult.empty

        def unifyListOfTypes(left: List[SIRType], right: List[SIRType], env: Map[TypeVar,SIRType]): UnificationResult[List[SIRType]] = {
           val s0: UnificationResult[scala.List[SIRType]] = UnificationResult.success(scala.List.empty, env)
           val r = left.zip(right).foldLeft(s0) {
               case (SuccessfulUnificationResult(accUnificator, env), (l, r)) =>
                   unify(l, r, env) match
                       case SuccessfulUnificationResult(unificator, env) =>
                           SuccessfulUnificationResult(unificator :: accUnificator, env)
                       case e@ErroredUnificationResult(msg) => e
                       case EmptyUnificationResult => EmptyUnificationResult
               case (e@ErroredUnificationResult(msg), _) => e
               case (e@EmptyUnificationResult, _) => e
           }
           r.map(_.reverse)
        }

        def unifyDecl(left: DataDecl, right: DataDecl, env: Map[TypeVar,SIRType]): UnificationResult[DataDecl] = {
            if left.name != right.name then
                UnificationResult.empty
            else
                // we think that if name are equals then constructors are also equals.
                //TODO:  need check: are names here full-names.
                val s0: UnificationResult[List[TypeVar]] = SuccessfulUnificationResult(scala.List.empty, env)
                left.typeParams.zip(right.typeParams).foldLeft(s0) {
                    case (SuccessfulUnificationResult(accUnificator, env), (l, r)) =>
                        unify(l, r, env) match
                            case SuccessfulUnificationResult(unificator, env) =>
                                SuccessfulUnificationResult(l :: accUnificator, env)
                            case ErroredUnificationResult(msg) => ErroredUnificationResult(msg)
                            case EmptyUnificationResult => EmptyUnificationResult
                    case (e@ErroredUnificationResult(msg), _) => ErroredUnificationResult(msg)
                    case (e@EmptyUnificationResult, _) => e
                }.map{ rTypeArgs =>
                    DataDecl(left.name, left.constructors, rTypeArgs.reverse)
                }
        }

        val retval: UnificationResult[SIRType] = left match
            case leftP: Primitive[?] =>
                right match
                    case rightP: Primitive[?] =>
                        if leftP == rightP then
                            UnificationResult.success(leftP, env)
                        else
                            UnificationResult.empty
                    case tp: TypeProxy =>
                        checkRightProxy(tp)
                    case _ =>
                        checkRightNoSame
            case Data =>
                right match
                    case Data => UnificationResult.success(Data, env)
                    case other => checkRightNoSame
            case ccl@CaseClass(constrDecl, typeArgs) =>
                right match
                    case rrl@CaseClass(constrDeclRight, typeArgsRight) =>
                        println(s"CaseClass: constrDecl: $constrDecl, constrDeclRight: $constrDeclRight, same=${constrDecl == constrDeclRight}")
                        if constrDecl == constrDeclRight then
                            unifyListOfTypes(typeArgs,typeArgsRight, env).map(CaseClass(constrDecl, _))
                        else
                            UnificationResult.empty
                    case SumCaseClass(decl, typeArgsRight) =>
                        decl.constructors.find(_ == constrDecl) match
                            case Some(_) =>
                                val nEnv = constrDecl.typeParams.zip(typeArgs).foldLeft(env) {
                                    case (acc, (tv,t)) => acc + (tv -> t)
                                }
                                unifyListOfTypes(constrDecl.parentTypeArgs, typeArgsRight, nEnv).map(SumCaseClass(decl, _))
                            case None => UnificationResult.empty
                    case _ =>
                        checkRightNoSame
            case SumCaseClass(declLeft, typeArgsLeft) =>
                right match
                    case SumCaseClass(declRight, typeArgsRight) =>
                        println(s"SumCaseClass: declLeft: $declLeft, declRight: $declRight, same=${declLeft == declRight}")
                        unifyDecl(declLeft,declRight, env) match
                            case SuccessfulUnificationResult(declUnificator,env) =>
                                unifyListOfTypes(typeArgsLeft, typeArgsRight, env).map(SumCaseClass(declUnificator, _))
                            case ErroredUnificationResult(msg) => ErroredUnificationResult(msg)
                            case EmptyUnificationResult => EmptyUnificationResult
                    case ccr@CaseClass(constrDeclRight, typeArgsRight) =>
                        declLeft.constructors.find(_ == constrDeclRight) match
                            case Some(_) =>
                                val nEnv = constrDeclRight.typeParams.zip(typeArgsRight).foldLeft(env) {
                                    case (acc, (tv,t)) => acc + (tv -> t)
                                }
                                unifyListOfTypes(typeArgsLeft, constrDeclRight.parentTypeArgs, nEnv).map(CaseClass(constrDeclRight, _))
                            case None => UnificationResult.empty
                    case _ =>
                        checkRightNoSame
            case Fun(inLeft,outLeft) =>
                right match
                    case Fun(inRight, outRight) =>
                        unify(inLeft, inRight, env) match
                            case SuccessfulUnificationResult(unificator, env) =>
                                unify(outLeft, outRight, env).map(Fun(unificator, _))
                            case e@ErroredUnificationResult(msg) => e
                            case EmptyUnificationResult => EmptyUnificationResult
                    case _ =>
                        checkRightNoSame
            case tvl@TypeVar(name, _) =>
                checkLeftTypeVar(tvl)
            case tll@TypeLambda(params, body) =>
                checkLeftTypeLambda(tll)
            case TypeError(msg, _) =>
                // TODO: propagate cause
                UnificationResult.error(msg)
            case FreeUnificator =>
                UnificationResult.success(right, env)
            case tp: TypeProxy =>
                checkLeftProxy(tp)

        println(s"unify: left: ${left.show}, right: ${right.show}, env: $env, retval: $retval")
        retval
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


    inline def liftM[T <: AnyKind]: SIRType = ${liftMImpl[T]}



    def liftMImpl[T<:AnyKind:Type](using Quotes): Expr[SIRType] = {
        import quotes.reflect.*

        println(s"liftMImpl: ${TypeRepr.of[T].show}")

        case class Env(
            val forwardRefs: Map[Symbol, TypeProxy] = Map.empty,
            val vars: Map[Symbol, TypeVar] = Map.empty,
            filledVars: Map[TypeVar, SIRType] = Map.empty,
            //var enclosingBounds = Map.empty[Int,TypeVar]
        )

        def liftRepr(tp: TypeRepr, env: Env): SIRType = {
            tp match
                case typeLambda@quotes.reflect.TypeLambda(paramNames, paramBounds, resType) =>
                    println(s"type-lambda detected, tp.symbol = ${tp.typeSymbol}, tp.symbol.hashCode=${tp.typeSymbol.hashCode}, Type: ${tp.show}")
                    // we need to generate param names with unique ids.
                    //println(s"tp.hashCode = ${tp.hashCode}")
                    //println(s"resType = ${resType.show}")
                    var newVars = env.vars
                    val sirParamExprs = paramNames.zipWithIndex.map {
                        case (name, idx) =>
                            val sym = typeLambda.param(idx).typeSymbol
                            val id = sym.hashCode
                            val tv = TypeVar(name, Some(id))
                            newVars = newVars + (sym -> tv)
                            tv
                    }
                    val nEnv = env.copy(vars = newVars)
                    SIRType.TypeLambda(sirParamExprs, liftRepr(resType, nEnv))
                case pr@quotes.reflect.ParamRef(p, n) =>
                    println(s"ParamRef detected,  Type: ${tp.show}, p=$p, p.typeSymbol.hashCode = ${p.typeSymbol.hashCode} n=$n")
                    val name = tp.show // TODO: get from enclosing lambda.
                    val id = pr.typeSymbol.hashCode
                    env.vars.get(pr.typeSymbol) match
                        case Some(value) =>
                        case None =>
                            report.error(s"No enclosing lambda found for typevar ${tp.show}", Position.ofMacroExpansion)
                    TypeVar(name, Some(id) )
                case tr: TermRef =>
                    if !(tr.widen =:= tr) then
                        liftRepr(tp.widen, env)
                    else
                        report.error(s"TermRef is unsupported,  Type: ${tp.show}", Position.ofMacroExpansion)
                        SIRType.TypeError( s"TermRef ${tp.show} is unsupported", null)
                case tpc: TypeRef =>
                    val sym = tpc.typeSymbol
                    if (sym == defn.NothingClass) then
                        SIRType.TypeNothing
                    else if (sym.isAliasType) then
                        liftRepr(tpc.dealias, env)
                    else if (sym.isClassDef) then
                        liftClassType(tpc, Nil, env)
                    else if (sym.isTypeParam) then
                        env.vars.get(sym) match
                            case Some(tv) => tv
                            case None =>
                                report.error(s"Unbound type variable ${tpc.show}", Position.ofMacroExpansion)
                                SIRType.TypeError( s"Unbound type variable ${tpc.show}", null)
                    else
                        report.error(s"Unsupported type [1]: ${tp.show}", Position.ofMacroExpansion)
                        SIRType.TypeError(s"Unsupported type [1]: ${tp.show}", null)
                case ct: ConstantType =>
                    liftRepr(tp.widen, env)
                case other =>
                    tp.asType match {
                        case '[scalus.builtin.ByteString] =>
                            ByteStringPrimitive
                        case '[BigInt] =>
                            IntegerPrimitive
                        case '[String] => StringPrimitive
                        case '[Boolean] => BooleanPrimitive
                        case '[Unit] => VoidPrimitive
                        case '[scalus.builtin.Data] => Data
                        case '[a => b] =>
                            //println(s"Fun detected,  Type: ${tp.show}")
                            val in = TypeRepr.of[a]
                            val out = TypeRepr.of[b]
                            Fun(liftRepr(in, env), liftRepr(out, env))
                        case '[(a,b)=>c] =>
                            println(s"Fun2 detected,  Type: ${tp.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            Fun( liftRepr(TypeRepr.of[a], env),
                                Fun(  liftRepr(TypeRepr.of[b], env),
                                      liftRepr(TypeRepr.of[c], env)) )
                        case '[(a,b,c)=>d] =>
                            println(s"Fun3 detected,  Type: ${tp.show}")
                            val ta = TypeRepr.of[a]
                            val tb = TypeRepr.of[b]
                            val tc = TypeRepr.of[c]
                            val td = TypeRepr.of[d]
                            println(s"ta: ${ta.show}, tb: ${tb.show}, tc: ${tc.show}, td: ${td.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                 Fun( liftRepr(TypeRepr.of[c], env),
                                      liftRepr(TypeRepr.of[d], env) )))
                        case '[(a,b,c,d)=>e] =>
                            println(s"Fun4 detected,  Type: ${tp.show}")
                            Fun( liftRepr(TypeRepr.of[a], env),
                                Fun( liftRepr(TypeRepr.of[b], env),
                                    Fun( liftRepr(TypeRepr.of[c], env),
                                        Fun( liftRepr(TypeRepr.of[d], env),
                                             liftRepr(TypeRepr.of[e], env) ))))
                        case '[(a,b,c,d,e)=>f] =>
                            println(s"Fun5 detected,  Type: ${tp.show}")
                            Fun( liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun( liftRepr(TypeRepr.of[c], env),
                                        Fun(  liftRepr(TypeRepr.of[d], env),
                                            Fun( liftRepr(TypeRepr.of[e], env),
                                                 liftRepr(TypeRepr.of[f], env) )))))
                        case '[(a,b,c,d,e,f)=>g] =>
                            println(s"Fun6 detected,  Type: ${tp.show}")
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun(liftRepr(TypeRepr.of[c], env),
                                        Fun(liftRepr(TypeRepr.of[d], env),
                                            Fun( liftRepr(TypeRepr.of[e], env),
                                                Fun( liftRepr(TypeRepr.of[f], env),
                                                     liftRepr(TypeRepr.of[g], env) ))))))
                        case '[scalus.builtin.Pair[a,b]] =>
                            println(s"Builtin pair detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            SIRType.Pair(liftRepr(a, env),
                                         liftRepr(b, env))
                        case '[(a,b)] =>
                            println(s"Tuple detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            SIRType.Pair(liftRepr(a, env),
                                         liftRepr(b, env))
                        case '[scalus.builtin.List[a]] =>
                            println(s"List detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            SIRType.List(liftRepr(a, env))
                        case '[Option[a]] =>
                            ???
                        case other =>

                            println(s"Unrecognized type: ${tp.show} tree: ${other}, tp=${tp}")
                            report.error(s"Unsupported type [1]: ${tp.show} in ${TypeRepr.of[T].show}", Position.ofMacroExpansion)
                            ???
            }
        }

        def liftClassType(tp: TypeRepr, typeArgs: List[SIRType], env: Env): SIRType = {
            val sym = tp.typeSymbol
            println(s"liftClassType ${sym} ${typeArgs.map(_.show)},  ")
            val retval = (tryLiftPrimitive(sym, typeArgs) orElse
                    tryLiftBuildinType(sym, typeArgs, env) orElse
                    tryLiftCaseClassOrCaseParent(tp, sym, typeArgs, env) orElse
                    tryMakeNonCaseModule(tp, sym, typeArgs, env)
                ).getOrElse {
                    val name = sym.name
                    val typeArgsShow = typeArgs.map(_.show)
                    println(s"makeSIRClassType ${name} ${typeArgs}")
                    unsupportedType(tp, s"tree=${tp}, isClass=${sym.isClassDef} isAliasType=${sym.isAliasType}", env)
                }
            println("liftClassType return " + retval)
            retval
        }

        def tryLiftPrimitive(sym: Symbol, types: List[SIRType]): Option[SIRType] = {
            if (sym == defn.IntClass || sym == defn.LongClass ) then
                Some(IntegerPrimitive)
            else if (sym == Symbol.requiredClass("scalus.builtin.ByteString")) then
                Some(ByteStringPrimitive)
            else if (sym == Symbol.requiredClass("scala.math.BigInt") ) then
                Some(IntegerPrimitive)
            else if (sym == defn.StringClass) then
                Some(StringPrimitive)
            else if (sym == defn.BooleanClass) then
                Some(BooleanPrimitive)
            else if (sym == defn.UnitClass) then
                Some(VoidPrimitive)
            else
                None
        }

        def tryLiftBuildinType(sym: Symbol, types: List[SIRType], env: Env): Option[SIRType] = {
            if (sym == Symbol.requiredClass("scalus.builtin.Data")) then
                Some(Data)
            else if (sym == Symbol.requiredClass("scalus.builtin.List")) then
                Some(SIRType.List(types.head))
            else if (sym == Symbol.requiredClass("scalus.builtin.Pair")) then
                Some(SIRType.Pair(types.head, types(1)))
            else
                None
        }

        def tryLiftCaseClassOrCaseParent(tp: TypeRepr, sym: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            env.forwardRefs.get(sym) match
                case Some(tp) =>
                    Some(tp)
                case None =>
                    val TypeProxy = new TypeProxy(null)
                    val nEnv = env.copy(forwardRefs = env.forwardRefs + (sym -> TypeProxy))
                    tryLiftCaseClassOrCaseParentNoProxy(tp, sym, targs, nEnv) match
                        case Some(sirType) =>
                            TypeProxy.ref = sirType
                            Some(sirType)
                        case None =>
                            None
        }

        def tryLiftCaseClassOrCaseParentNoProxy(tp: TypeRepr, symbol: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            if (symbol.flags.is(Flags.Case) || symbol.flags.is(Flags.Enum)) {
                ???
            } else {
                ???
            }
        }

        def tryMakeNonCaseModule(tp: TypeRepr, symbol: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            ???
        }

        def unsupportedType(tp: TypeRepr, msg: String, env: Env, throwException: Boolean = true): SIRType = {
            val fullMessage = s"Unsupported type: ${tp.show} in ${TypeRepr.of[T].show}, $msg"
            report.error(fullMessage, Position.ofMacroExpansion)
            if (throwException) then
                throw new IllegalArgumentException(fullMessage)
            SIRType.TypeError(fullMessage, null)
        }

        try
            val sirType = liftRepr(TypeRepr.of[T], Env())
            SIRTypeToExpr.apply(sirType)
        catch
            case NonFatal(ex) =>
                println(s"Error: during lifting ${TypeRepr.of[T].show}: ${ex.getMessage}")
                ex.printStackTrace()
                throw ex
    }

    
    
}

