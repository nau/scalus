package scalus.uplc
import scalus.uplc.Data.FromData

import scala.deriving.*
import scala.quoted.*
import scalus.builtins.Builtins

/*
  WIP ToData derivation, not working yet
 */
object TotoData {
  import scala.compiletime.*
  import scala.quoted.*
  type ToData[A] = A => Data

  inline def deriveProduct[T](inline constrIdx: Int): ToData[T] = ${
    deriveProductMacro[T]('{ constrIdx })
  }

  def deriveProductMacro[T: Type](constrIdx: Expr[Int])(using Quotes): Expr[ToData[T]] =
    import quotes.reflect.*
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

    val args = '{ scalus.builtins.List.Nil }

    '{ (a: T) =>
      Builtins.mkConstr(BigInt($constrIdx), $args)
    }

  extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]].apply(a)
}

/*
  WIP FromData derivation, not working yet
 */
object FromData {
  import scala.compiletime.*

  inline def derived[T]: FromData[T] = ${ derivedMacro[T] }

  def derivedMacro[T: Type](using Quotes): Expr[FromData[T]] =
    import quotes.reflect.*
    val classSym = TypeTree.of[T].symbol
    val constr = classSym.primaryConstructor
    val params = constr.paramSymss.flatten
    val fromDataOfArgs = params.map { param =>
      val tpe = param.termRef.widen.dealias
      val implicitType = TypeRepr.of[FromData].appliedTo(tpe)
      val convTerm = Implicits.search(implicitType) match {
        case iss: ImplicitSearchSuccess => iss.tree
        case isf: ImplicitSearchFailure =>
          report.errorAndAbort(s"Could not find implicit for ${implicitType.show}")
      }
      // println(s"convTerm: $convTerm, symbol: ${convTerm.symbol}, isFUnc: ${convTerm.tpe.isFunctionType}, methods: ${convTerm.symbol.methodMembers}")
      if convTerm.tpe.isFunctionType then convTerm
      else convTerm.select(convTerm.symbol.methodMember("apply").head)
    }
    def genGetter(init: Expr[scalus.builtins.List[Data]], idx: Int): Expr[Data] =
      var expr = init
      var i = 0
      while i < idx do
        val exp = expr // save the current expr, otherwise it will loop forever
        expr = '{ $exp.tail }
        i += 1
      '{ $expr.head }

    def expr(a: Term) = {
      val args = fromDataOfArgs.zipWithIndex.map { case (appl, idx) =>
        val arg = genGetter(a.asExprOf[scalus.builtins.List[Data]], idx).asTerm
        appl.appliedTo(arg)
      }
      New(TypeTree.of[T]).select(constr).appliedToArgs(args).asExprOf[T]
    }
    '{ (d: Data) =>
      val args = scalus.builtins.Builtins.unsafeDataAsConstr(d).snd
      ${ expr('{ args }.asTerm) }
    }
}
