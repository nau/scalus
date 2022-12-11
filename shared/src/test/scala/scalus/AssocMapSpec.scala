package scalus

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.ArbitraryInstances

import scala.collection.immutable

import Prelude.AssocMap
import scalus.Prelude.{===, given}
import Prelude.List
import Prelude.List.*
import Prelude.These.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Cek

class AssocMapSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

  given arbList[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary {
    for {
      lst <- Arbitrary.arbitrary[immutable.List[A]]
    } yield List(lst: _*)
  }

  given arbAssocMap[A: Arbitrary, B: Arbitrary]: Arbitrary[AssocMap[A, B]] = Arbitrary {
    for {
      map <- Arbitrary.arbitrary[Map[A, B]]
    } yield AssocMap.fromList(List(map.toSeq: _*))
  }

  test("empty") {
    assert(AssocMap.toList(AssocMap.empty) == List.Nil)
  }

  test("union") {
    val m1 = AssocMap.fromList(Cons((BigInt(1), BigInt(2)), Cons((BigInt(0), BigInt(0)), List.Nil)))
    val m2 = AssocMap.fromList(Cons((BigInt(1), BigInt(3)), Cons((BigInt(3), BigInt(4)), List.Nil)))
    val m3 = AssocMap.union(m1, m2)
    val compiled = Compiler.compile {
      val a = BigInt(132)
      AssocMap.union(m1, m2)
    }
    println(compiled.pretty.render(100))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    println(Cek.evalUPLC(term).pretty.render(100))
    assert(
      AssocMap.toList(m3) == List(
        (BigInt(1), These(2, 3)),
        (BigInt(0), This(BigInt(0))),
        (BigInt(3), That(BigInt(4)))
      )
    )
  }

  test("toList(fromList(lst)) == lst") {
    check { (lst: List[(BigInt, Boolean)]) =>
      AssocMap.toList(AssocMap.fromList(lst)) == lst
    }
  }

  test("insert") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(k, v)(map)
      val lst1 = AssocMap.toList(m1).toList
      lst1.contains((k, v))
    }
  }

  test("lookup") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(k, v)(map)
      AssocMap.lookup(k)(m1) == Prelude.Maybe.Just(v)
    }
  }

  test("delete") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(k, v)(map)
      AssocMap.lookup(k)(m1) == Prelude.Maybe.Just(v)
      val m2 = AssocMap.delete(m1)(k)
      AssocMap.lookup(k)(m2) == Prelude.Maybe.Nothing
    }
  }

}