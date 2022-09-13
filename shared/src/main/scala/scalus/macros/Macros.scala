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

  def fieldMacro[A: Type](e: Expr[A => Any])(using Quotes): Expr[Exp[Data] => Exp[Data]] =
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
          report.info(s"$typeSymbolOfA => fieldOpt: $fieldOpt")
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

  def fieldMacro2[A: Type](e: Expr[A => Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    e.asTerm match
      case Inlined(
            _,
            _,
            Block(List(DefDef(_, _, _, Some(Select(_, fieldName)))), _)
          ) =>
        val tpe: TypeRepr = TypeRepr.of[A]
        val s: Symbol = tpe.typeSymbol
        val fieldOpt = s.caseFields.zipWithIndex.find(_._1.name == fieldName)
        fieldOpt match
          case Some((fieldSym: Symbol, idx)) =>
            val idxExpr = Expr(idx)
            val fieldType = tpe.memberType(fieldSym).dealias
            val asdf = TypeRepr.of[Unlift].appliedTo(fieldType)
            Implicits.search(asdf) match
              case success: ImplicitSearchSuccess =>
                val expr = success.tree
                val exprType = expr.tpe
                val exprTypeStr = exprType.show
                val exprStr = expr.show
                val impl = success.tree.asExpr.asInstanceOf[Expr[Unlift[Any]]]
                val unlift: Expr[Exp[Data => Any]] = '{ $impl.unlift }
//                report.errorAndAbort(s"found implicit: $exprTypeStr $exprStr")
                '{
                  var expr: Exp[Data] => Exp[List[Data]] = d => sndPair(unConstrData(d))
                  var i = 0
                  while i < $idxExpr do
                    val exp = expr // save the current expr, otherwise it will loop forever
                    expr = d => tailList(exp(d))
                    i += 1
                  (d: Exp[Data]) => ExprBuilder.app($unlift, headList(expr(d)))
                }
              case failure: ImplicitSearchFailure =>
                '{
                  var expr: Exp[Data] => Exp[List[Data]] = d => sndPair(unConstrData(d))
                  var i = 0
                  while i < $idxExpr do
                    val exp = expr // save the current expr, otherwise it will loop forever
                    expr = d => tailList(exp(d))
                    i += 1
                  (d: Exp[Data]) => headList(expr(d))
                }
          case None =>
            report.errorAndAbort("fieldMacro: " + fieldName)
      case x => report.errorAndAbort(x.toString)

}
