package scalus

import scalus.ledger.api.v1.ScriptContext
import scalus.uplc.Compiler.compile
import scalus.uplc.Term

import scala.annotation.tailrec

object Bench:

  def validator(redeemer: Unit, datum: Unit, ctx: ScriptContext) =

    val err = scalus.uplc.Term.Error("invalid")

    @tailrec def loop(x: Int): Unit =
      if x > 0 then loop(x - 1)
    loop(10)

    val a: Any = 1
    a match
      case "Hello World" => println("Hello World")
      case 1 | 2         => println("Hello World")
      case (3, 4)        => println("other")

    println("Hello")

  val script = scalus.uplc.Compiler.compile(validator)
  val script1 = compile(1)

  def main(args: Array[String]): Unit =

    val a = 1
    val b = 2
    def foo(): Int = bar(a)
    def bar(x: Int): Int = x + b + foo()

    println(script.pretty.render(80))