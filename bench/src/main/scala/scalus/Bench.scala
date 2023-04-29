package scalus

import scalus.ledger.api.v1.ScriptContext
import scalus.uplc.Compiler.compile
import scalus.uplc.Term

import scala.annotation.tailrec

@Compile
case class Asdf(a: String)

object Bench:
  val script1 = compile(true)
  val script2 = compile(BigInt(123))
  val script3 = compile(BigInt("1234567890"))
  val script4 = compile(if true then () else ())

  def main(args: Array[String]): Unit =

    val a = 1
    val b = 2
    def foo(): Int = bar(a)
    def bar(x: Int): Int = x + b + foo()

    println(script1.toString())
    println(script2.toString())
    println(script3.toString())
    println(script4.toString())
    println(script1.pretty.render(80))
