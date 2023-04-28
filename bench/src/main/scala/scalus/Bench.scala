package scalus

import scalus.ledger.api.v1.ScriptContext
import scalus.uplc.Compiler.compile
import scalus.uplc.Term

import scala.annotation.tailrec

@Compile
case class Asdf(a: String)

object Bench:
  val script1 = compile(BigInt(1))

  def main(args: Array[String]): Unit =

    val a = 1
    val b = 2
    def foo(): Int = bar(a)
    def bar(x: Int): Int = x + b + foo()

    println(script1.pretty.render(80))
