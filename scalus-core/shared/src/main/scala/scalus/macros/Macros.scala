package scalus.macros

import scalus.{builtin, Compiler}
import scalus.builtin.Builtins
import scalus.builtin.Data
import scalus.sir.SIR
import scalus.uplc.{BuiltinRuntime, BuiltinsMeaning, DefaultFun, Expr as Exp, ExprBuilder, Term as Trm}
import scalus.uplc.ExprBuilder.*

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.immutable
import scala.quoted.*
import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer

object Macros {

    @nowarn
    def lamMacro[A: Type, B: Type](f: Expr[Exp[A] => Exp[B]])(using Quotes): Expr[Exp[A => B]] =
        import quotes.reflect.*
        val name = f.asTerm match
            // lam(x => body)
            case Inlined(
                  _,
                  _,
                  Block(List(DefDef(_, List(List(ValDef(name, _, _))), _, body)), _)
                ) =>
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

    def fieldAsExprDataMacro[A: Type](e: Expr[A => Any])(using
        Quotes
    ): Expr[Exp[Data] => Exp[Data]] =
        import quotes.reflect.*
        e.asTerm match
            case Inlined(
                  _,
                  _,
                  Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _)
                ) =>
                def genGetter(
                    typeSymbol: Symbol,
                    fieldName: String
                ): Expr[Exp[Data] => Exp[Data]] =
                    val typeSymbolOfA = typeSymbol.typeRef.dealias.typeSymbol
                    val fields = typeSymbolOfA.caseFields.filter(_.isValDef)
                    val fieldOpt: Option[(Symbol, Int)] =
                        fields.zipWithIndex.find(_._1.name.stripTrailing() == fieldName)
//          report.info(s"$typeSymbolOfA => fieldOpt: $fieldOpt")
                    fieldOpt match
                        case Some((fieldSym: Symbol, idx)) =>
                            val idxExpr = Expr(idx)
                            '{
                                var expr: Exp[Data] => Exp[List[Data]] = d =>
                                    sndPair(unConstrData(d))
                                var i = 0
                                while i < $idxExpr do
                                    val exp =
                                        expr // save the current expr, otherwise it will loop forever
                                    expr = d => tailList(exp(d))
                                    i += 1
                                d => headList(expr(d))
                            }
                        case None =>
                            report.errorAndAbort(
                              s"fieldAsData: can't find a field `$fieldName` in $typeSymbolOfA. Available fields: ${fields
                                      .map(_.name)}"
                            )

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
            case x => report.errorAndAbort(s"fieldAsExprDataMacro: $x")

    def fieldAsDataMacro[A: Type](e: Expr[A => Any])(using Quotes): Expr[Data => Data] =
        import quotes.reflect.*
        fieldAsDataMacroTerm(e.asTerm)

    def fieldAsDataMacroTerm(using q: Quotes)(e: q.reflect.Term): Expr[Data => Data] =
        import quotes.reflect.*
        e match
            case Inlined(_, _, block) => fieldAsDataMacroTerm(block)
            case Block(List(DefDef(_, _, _, Some(select @ Select(_, fieldName)))), _) =>
                def collectSymbolsAndFields(tree: Tree): List[(Symbol, String)] = tree match
                    case Select(select @ Select(_, _), fieldName) =>
                        collectSymbolsAndFields(
                          select
                        ) :+ (select.tpe.widen.dealias.typeSymbol, fieldName)
                    case Select(ident @ Ident(_), fieldName) =>
                        List((ident.tpe.widen.dealias.typeSymbol, fieldName))
                    case _ =>
                        report.errorAndAbort(
                          s"field macro supports only this form: _.caseClassField1.field2, but got " + tree.show
                        )

                /* I tried to optimize the code below, but it's not worth it.
                  1. Reuse val double = (d: builtin.List[Data]) => d.tail.tail.tail.tail (2,3,4,5,6,7 tails)
                  Expression: _.txInfo.id
                  Simple
                  tail 1 budget: { mem: 0.005780, cpu: 1.890490 }
                  tail 2 budget: { mem: 0.007280, cpu: 2.235490 }
                  tail 3 budget: { mem: 0.006980, cpu: 2.166490 }
                  tail 4 budget: { mem: 0.006680, cpu: 2.097490 }
                  tail 5 budget: { mem: 0.006380, cpu: 2.028490 }
                  tail 6 budget: { mem: 0.006380, cpu: 2.028490 }
                  tail 7 budget: { mem: 0.006380, cpu: 2.028490 }
                  Optimized:
                  tail 1 budget: { mem: 0.005580, cpu: 1.844490 }
                  tail 2 budget: { mem: 0.007080, cpu: 2.189490 }
                  tail 4 budget: { mem: 0.006480, cpu: 2.051490 }
                  Optimized & eta-reduced:
                  tail 1 budget: { mem: 0.005580, cpu: 1.844490 }
                  2. Reuse val getargs = (d: Data) => Builtins.unConstrData(d).snd
                  Expression: _.txInfo.id
                  getargs 2 usages
                  Optimized: budget: { mem: 0.006480, cpu: 2.051490 }
                  Expression: _.txInfo.id.hash
                  getargs 0 usages
                  Optimized: budget: { mem: 0.006276, cpu: 2.144366 }
                  getargs 3 usages
                  Optimized: budget: { mem: 0.007476, cpu: 2.420366 }

                  Conclusion: it's not worth it to abstract and reuse. Inlining is better.
                 */

                '{ d =>
                    ${
                        def getField(idx: Int, d: Expr[Data]): Expr[Data] = {
                            var expr = '{ Builtins.unConstrData($d).snd }
                            var i = 0
                            while i < idx do
                                // save the current expr, otherwise it will loop forever
                                val exp = expr
                                expr = '{ $exp.tail }
                                i += 1
                            '{ $expr.head }
                        }

                        var ddd = '{ d }
                        for (typeSymbolOfA, fieldName) <- collectSymbolsAndFields(select) do {
                            val fields = typeSymbolOfA.caseFields.filter(_.isValDef)
                            val fieldOpt: Option[(Symbol, Int)] =
                                // OMG, don't ask me why, but Scala 3.3.3 adds a trailing space to the field name
                                // specifically in the case _1, _2, etc in Tuples.
                                // it's fixed in 3.4.2
                                // FIXME: remove stripTrailing when we upgrade to 3.4.2
                                fields.zipWithIndex.find(_._1.name.stripTrailing == fieldName)
                            // report.info(s"$typeSymbolOfA => ${fields.map(s => s"'${s.name}'")}")
                            fieldOpt match
                                case Some((_: Symbol, idx)) =>
                                    ddd = getField(idx, ddd)
                                case None =>
                                    report.errorAndAbort(
                                      s"""fieldAsData: can't find a field `$fieldName` in $typeSymbolOfA.
                                      |Available fields: ${fields.map(s =>
                                            s"'${s.name}'"
                                        )}""".stripMargin
                                    )
                        }
                        ddd
                    }
                }

            case x => report.errorAndAbort(x.toString)

    import upickle.default.*
    def mkReadWriterImpl[A: Type](using Quotes): Expr[ReadWriter[A]] = {
        import scala.quoted.*
        import quotes.reflect.*
        val tpe = TypeTree.of[A]
        val fields = tpe.symbol.declaredFields
        val fieldNames = fields.map(_.name)
        val impl = '{
            upickle.default
                .readwriter[ujson.Value]
                .bimap[A](
                  m =>
                      ujson.Obj.from(${
                          Expr.ofList(
                            fields.map(name =>
                                '{
                                    (
                                      ${ Expr(name.name) },
                                      writeJs[Long](${ Select('{ m }.asTerm, name).asExprOf[Long] })
                                    )
                                }
                            )
                          )
                      }),
                  json =>
                      ${
                          val stats = ListBuffer[Statement]()
                          // val params = new A()
                          val value = ValDef(
                            Symbol.newVal(
                              Symbol.spliceOwner,
                              "params",
                              tpe.tpe.widen,
                              Flags.EmptyFlags,
                              Symbol.noSymbol
                            ),
                            Some(New(tpe).select(tpe.symbol.primaryConstructor).appliedToNone)
                          )
                          val ref = Ref(value.symbol)
                          stats += value
                          // params.field1 = read[Long](json.obj("field1"))
                          // ...
                          fields.foreach { field =>
                              stats += Assign(
                                ref.select(field),
                                '{ read[Long](json.obj(${ Expr(field.name.toString) })) }.asTerm
                              )
                          }
                          // { val params = new A(); params.field1 = read[Long](json.obj("field1")); ...; params }
                          Block(stats.toList, ref).asExprOf[A]
                      }
                )
        }
        // println(impl.asTerm.show(using Printer.TreeShortCode))
        impl
    }

    /** Generates a pair of functions to convert a class to a sequence of longs and vice versa. The
      * generated code is equivalent to the following:
      * {{{
      *   (
      *     (m: A) => Seq(m.field1, m.field2, ...),
      *     (seq: Seq[Long]) => {
      *       val params = new A()
      *       params.field1 = if idx < seq.size then seq(0) else 0L
      *       ...
      *       params
      *     }
      *   )
      * }}}
      *
      * @tparam A
      *   the type of the class
      * @return
      *   a pair of functions
      */
    def mkClassFieldsFromSeqIsoImpl[A: Type](using
        Quotes
    ): Expr[(A => Seq[Long], Seq[Long] => A)] = {
        import scala.quoted.*
        import quotes.reflect.*
        val tpe = TypeTree.of[A]
        val fields = tpe.symbol.declaredFields
        val impl = '{
            (
              (m: A) =>
                  ${ Expr.ofSeq(fields.map(name => '{ m }.asTerm.select(name).asExprOf[Long])) },
              (seq: Seq[Long]) =>
                  ${
                      val stmts = ListBuffer[Statement]()
                      // val params = new A()
                      val value = ValDef(
                        Symbol.newVal(
                          Symbol.spliceOwner,
                          "params",
                          tpe.tpe.widen,
                          Flags.EmptyFlags,
                          Symbol.noSymbol
                        ),
                        Some(New(tpe).select(tpe.symbol.primaryConstructor).appliedToNone)
                      )
                      val ref = Ref(value.symbol)
                      stmts += value
                      val size = '{ seq.size }
                      // params.field1 = if idx < seq.size then seq(0) else 0L
                      // ...
                      for (field, index) <- fields.zipWithIndex do
                          val idx = Expr(index)
                          stmts += Assign(
                            ref.select(field),
                            '{ if $idx < $size then seq($idx) else 300_000_000L }.asTerm
                          )

                      // { val params = new A(); params.field1 =...; params }
                      Block(stmts.toList, ref).asExprOf[A]
                  }
            )
        }
        // println(impl.asTerm.show(using Printer.TreeShortCode))
        impl
    }

    def inlineBuiltinCostModelJsonImpl(using Quotes)(name: Expr[String]): Expr[String] = {
        import scala.quoted.*
        val string =
            Files.readString(
              Paths.get(
                "/Users/otto/work/scalus/scalus-core/shared/src/main/resources",
                name.value.get
              )
            )
        Expr(string)
    }

    def questionMark(using Quotes)(x: Expr[Boolean]): Expr[Boolean] = {
        import scala.quoted.*
        '{ if $x then true else Builtins.trace(${ Expr(x.show + " ? False") })(false) }
    }

    /** Generates `match` expression on [[DefaultFun]] ordinals that should be efficiently compiled
      * to table switch (and it is).
      *
      * {{{
      *   fun.ordinal() match {
      *     case 0 => bm.AddInteger
      *     // ...
      *   }
      * }}}
      */
    def mkGetBuiltinRuntime(
        bm: Expr[BuiltinsMeaning]
    )(using Quotes): Expr[DefaultFun => BuiltinRuntime] = {
        import quotes.reflect.*
        val cases: List[CaseDef] = DefaultFun.values.toList.map { fun =>
            val funOrdinal = Expr(fun.ordinal())
            val funCase = CaseDef(funOrdinal.asTerm, None, Select.unique(bm.asTerm, fun.toString))
            funCase
        }
        '{ (fun: DefaultFun) =>
            ${ Match('{ fun.ordinal() }.asTerm, cases).asExprOf[BuiltinRuntime] }
        }
    }

    def generateCompileCall(code: Expr[Any])(using Quotes): Expr[SIR] = '{
        Compiler.compile($code)
    }

}
