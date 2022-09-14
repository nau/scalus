package scalus.macros

import scalus.uplc.ExprBuilder.*
import scalus.uplc.{Data, ExprBuilder, Expr as Exp, Term as Trm}

import scala.quoted.*
object Macros {
  def lamMacro[A: Type, B: Type](f: Expr[Exp[A] => Exp[B]])(using Quotes): Expr[Exp[A => B]] =
    import quotes.reflect.*
    val name = f.asTerm match
      // lam(x => body)
      case Inlined(_, _, Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _)) =>
        Expr(name)
      // lam { x => body }
      case Inlined(
            _,
            _,
            Block(List(), Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _))
          ) =>
        Expr(name)
      case x => report.errorAndAbort(x.toString)
    '{
      Exp(Trm.LamAbs($name, $f(vr($name)).term))
    }

  def asExprMacro[A: Type](e: Expr[A])(using Quotes): Expr[Exp[A]] =
    import quotes.reflect.*
    e.asTerm match
      // lam(x => body)
      case Inlined(_, _, Block(stmts, expr)) =>
        def asdf(e: Term): Expr[Exp[A]] = e match
          case Ident(name) =>
            val nm = Expr(name)
            '{ vr[A]($nm) }
          case Typed(e, _) =>
            asdf(e)
          case _ =>
            report.errorAndAbort(e.toString)
        asdf(expr)
      case x => report.errorAndAbort("asExprMacro: " + x.toString)

  def fieldAsDataMacro[A: Type](e: Expr[A => Any])(using Quotes): Expr[Exp[Data] => Exp[Data]] =
    import quotes.reflect.*
    e.asTerm match
      case Inlined(
            _,
            _,
            Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _)
          ) =>
        def genGetter(typeSymbolOfA: Symbol, fieldName: String): Expr[Exp[Data] => Exp[Data]] =
          val fieldOpt: Option[(Symbol, Int)] =
            if typeSymbolOfA == TypeRepr.of[Tuple2].typeSymbol then
              fieldName match
                case "_1" => typeSymbolOfA.caseFields.find(_.name == fieldName).map(s => (s, 0))
                case "_2" => typeSymbolOfA.caseFields.find(_.name == fieldName).map(s => (s, 1))
                case _ =>
                  report.errorAndAbort("Unexpected field name for Tuple2 type: " + fieldName)
            else typeSymbolOfA.caseFields.zipWithIndex.find(_._1.name == fieldName)
//          report.info(s"$typeSymbolOfA => fieldOpt: $fieldOpt")
          fieldOpt match
            case Some((fieldSym: Symbol, idx)) =>
              val idxExpr = Expr(idx)
              '{
                var expr: Exp[Data] => Exp[List[Data]] = d => sndPair(unConstrData(d))
                var i = 0
                while i < $idxExpr do
                  val exp = expr // save the current expr, otherwise it will loop forever
                  expr = d => tailList(exp(d))
                  i += 1
                d => headList(expr(d))
              }
            case None =>
              report.errorAndAbort("fieldMacro: " + fieldName)

        def composeGetters(tree: Tree): Expr[Exp[Data] => Exp[Data]] = tree match
          case Select(select @ Select(_, _), fieldName) =>
            val a = genGetter(select.tpe.typeSymbol, fieldName)
            val b = composeGetters(select)
            '{ $a compose $b }
          case Select(ident @ Ident(_), fieldName) =>
            genGetter(ident.tpe.typeSymbol, fieldName)
          case _ =>
            report.errorAndAbort(
              s"field macro supports only this form: _.caseClassField1.field2, but got " + tree.show
            )
        composeGetters(select)
      case x => report.errorAndAbort(x.toString)

  def fieldMacro[A: Type](e: Expr[A => Any])(using Quotes): Expr[Exp[Data] => Exp[Any]] =
    import quotes.reflect.*
    e.asTerm match
      case Inlined(
            _,
            _,
            Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _)
          ) =>
        def genGetter(
            typeSymbolOfA: Symbol,
            fieldName: String
        ): (Symbol, Expr[Exp[Data] => Exp[Data]]) =
          val fieldOpt: Option[(Symbol, Int)] =
            if typeSymbolOfA == TypeRepr.of[Tuple2].typeSymbol then
              fieldName match
                case "_1" => typeSymbolOfA.caseFields.find(_.name == fieldName).map(s => (s, 0))
                case "_2" => typeSymbolOfA.caseFields.find(_.name == fieldName).map(s => (s, 1))
                case _ =>
                  report.errorAndAbort("Unexpected field name for Tuple2 type: " + fieldName)
            else typeSymbolOfA.caseFields.zipWithIndex.find(_._1.name == fieldName)
          fieldOpt match
            case Some((fieldSym: Symbol, idx)) =>
              val idxExpr = Expr(idx)
              (
                fieldSym,
                '{
                  var expr: Exp[Data] => Exp[List[Data]] = d => sndPair(unConstrData(d))
                  var i = 0
                  while i < $idxExpr do
                    val exp = expr // save the current expr, otherwise it will loop forever
                    expr = d => tailList(exp(d))
                    i += 1
                  (d: Exp[Data]) => headList(expr(d))
                }
              )
            case None =>
              report.errorAndAbort("fieldMacro: " + fieldName)

        def composeGetters(tree: Tree): (TypeRepr, Expr[Exp[Data] => Exp[Data]]) = tree match
          case Select(select @ Select(_, _), fieldName) =>
            val (_, a) = genGetter(select.tpe.typeSymbol, fieldName)
            val (s, b) = composeGetters(select)
            (s, '{ $a compose $b })
          case Select(ident @ Ident(_), fieldName) =>
            val (fieldSym, f) = genGetter(ident.tpe.typeSymbol, fieldName)
            val fieldType = ident.tpe.memberType(fieldSym).dealias
            (fieldType, f)
          case _ =>
            report.errorAndAbort(
              s"field macro supports only this form: _.caseClassField1.field2, but got " + tree.show
            )

        val (fieldType, getter) = composeGetters(select)
        val unliftTypeRepr = TypeRepr.of[Unlift].appliedTo(fieldType)
        /*report.info(
          s"composeGetters: fieldType = ${fieldType.show} unliftTypeRepr = ${unliftTypeRepr.show}, detailed fieldType: $fieldType"
        )*/
        Implicits.search(unliftTypeRepr) match
          case success: ImplicitSearchSuccess =>
            unliftTypeRepr.asType match
              case '[Unlift[t]] =>
                val expr = success.tree
                val impl = success.tree.asExpr
                /*report
                  .info(
                    s"found implicit ${unliftTypeRepr.show} => ${expr.show}: ${expr.tpe.show}"
                  )*/
                '{ (d: Exp[Data]) =>
                  ExprBuilder
                    .app($impl.asInstanceOf[Unlift[t]].unlift, $getter(d))
                }
          case failure: ImplicitSearchFailure =>
            report.info(s"not found implicit of type ${unliftTypeRepr.show}")
            getter
      case x => report.errorAndAbort(x.toString)

}
