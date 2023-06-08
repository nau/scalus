package scalus.uplc
import scalus.builtins.Builtins
import scalus.uplc.Data.FromData

import scala.quoted.*

/*
  WIP ToData derivation, not working yet
 */
object TotoData {
  type ToData[A] = A => Data

  inline def deriveProduct[T](inline constrIdx: Int): ToData[T] = ${
    deriveProductMacro[T]('{ constrIdx })
  }

  def deriveProductMacro[T: Type](constrIdx: Expr[Int])(using Quotes): Expr[ToData[T]] =
    import quotes.reflect.*
    val classSym = TypeTree.of[T].symbol
    val constr = classSym.primaryConstructor
    val params = constr.paramSymss.flatten
    val toDataOfArgs = params.map { param =>
      val tpe = param.termRef.widen.dealias
      val implicitType = TypeRepr.of[ToData].appliedTo(tpe)
      val convTerm = Implicits.search(implicitType) match {
        case iss: ImplicitSearchSuccess => iss.tree
        case isf: ImplicitSearchFailure =>
          report.errorAndAbort(s"Could not find implicit for ${implicitType.show}")
      }
      // println(s"convTerm: $convTerm, symbol: ${convTerm.symbol}, isFUnc: ${convTerm.tpe.isFunctionType}, methods: ${convTerm.symbol.methodMembers}")
      val term =
        if convTerm.tpe.isFunctionType then convTerm
        else convTerm.select(convTerm.symbol.methodMember("apply").head)

    }
    println(s"Derive ${classSym}, ${constr} ${params} toDataOfArgs: $toDataOfArgs")

    val args = '{ scalus.builtins.Builtins.mkNilData() }

    '{ (a: T) =>
      Builtins.mkConstr(BigInt($constrIdx), $args)
    }

  extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]].apply(a)
}

/*
  WIP FromData derivation
 */
object FromData {

  inline def derived[T]: FromData[T] = ${ derivedMacro[T] }

  /** Derive FromData for an enum type
    *
    * @param conf
    *   a partial function mapping tag to constructor function, like
    * @return
    *   a FromData instance
    *
    * @example
    *   {{{
    *   enum Adt:
    *     case A
    *     case B(b: Boolean)
    *     case C(a: Adt, b: Adt)
    *
    *   given FromData[Adt] = FromData.deriveEnum[Adt] {
    *     case 0 => _ => Adt.A
    *     case 1 => FromData.deriveConstructor[Adt.B]
    *     case 2 => FromData.deriveConstructor[Adt.C]
    *     }
    *   }}}
    */
  inline def deriveEnum[T](
      inline conf: PartialFunction[Int, scalus.builtins.List[Data] => T]
  ): FromData[T] = ${ deriveEnumMacro[T]('{ conf }) }

  inline def deriveConstructor[T]: scalus.builtins.List[Data] => T = ${ deriveConstructorMacro[T] }

  def deriveConstructorMacro[T: Type](using Quotes): Expr[scalus.builtins.List[Data] => T] =
    import quotes.reflect.*
    val classSym = TypeTree.of[T].symbol
    val constr = classSym.primaryConstructor
    val params = constr.paramSymss.flatten
    val fromDataOfArgs = params.map { param =>
      val tpe = param.termRef.widen.dealias
      tpe.asType match
        case '[t] =>
          Expr.summon[FromData[t]] match
            case None => report.errorAndAbort(s"Could not find implicit for FromData[${tpe.show}]")
            case Some(value) => value
    }
    def genGetter(init: Expr[scalus.builtins.List[Data]], idx: Int)(using Quotes): Expr[Data] =
      var expr = init
      var i = 0
      while i < idx do
        val exp = expr // save the current expr, otherwise it will loop forever
        expr = '{ $exp.tail }
        i += 1
      '{ $expr.head }

    def genConstructorCall(
        a: Expr[scalus.builtins.List[scalus.uplc.Data]]
    )(using Quotes): Expr[T] = {
      val args = fromDataOfArgs.zipWithIndex.map { case (appl, idx) =>
        val arg = genGetter(a, idx)
        '{ $appl($arg) }.asTerm
      }
      // Couldn't find a way to do this using quotes, so just construct the tree manually
      New(TypeTree.of[T]).select(constr).appliedToArgs(args).asExprOf[T]
    }
    '{ (args: scalus.builtins.List[scalus.uplc.Data]) => ${ genConstructorCall('{ args }) } }

  def derivedMacro[T: Type](using Quotes): Expr[FromData[T]] =
    '{ (d: Data) =>
      val args = scalus.builtins.Builtins.unsafeDataAsConstr(d).snd

      // generate f = (args) => new Constructor(args.head, args.tail.head, ...)
      // then apply to args: f(args)
      // and finally beta reduce it in compile time
      ${ Expr.betaReduce('{ ${ deriveConstructorMacro[T] }(args) }) }
    }

  def deriveEnumMacro[T: Type](conf: Expr[PartialFunction[Int, scalus.builtins.List[Data] => T]])(
      using Quotes
  ): Expr[FromData[T]] =
    import quotes.reflect.*
    val mapping = conf.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        cases.map { case CaseDef(Literal(IntConstant(tag)), _, code) =>
          (tag, code.asExprOf[scalus.builtins.List[Data] => T])
        }
    // stage programming is cool, but it's hard to comprehend what's going on
    '{ (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val tag = pair.fst
      val args = pair.snd
      ${
        mapping.foldRight('{ throw new Exception("Invalid tag") }.asExprOf[T]) {
          case ((t, code), acc) =>
            '{ if Builtins.equalsInteger(tag, BigInt(${ Expr(t) })) then $code(args) else $acc }
        }
      }
    }
}
