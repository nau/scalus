package scalus.prelude

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler
import scalus.prelude.List.*
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.prelude.These.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances

import scala.collection.immutable

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

  test("isEmpty") {
    assert(List.isEmpty(List.empty))
    assert(!List.isEmpty(new List.Cons(true, List.Nil)))
  }

  test("union") {
    val m1 = AssocMap.fromList(
      new Cons((BigInt(1), BigInt(2)), new Cons((BigInt(0), BigInt(0)), List.Nil))
    )
    val m2 = AssocMap.fromList(
      new Cons((BigInt(1), BigInt(3)), new Cons((BigInt(3), BigInt(4)), List.Nil))
    )
    val m3 = AssocMap.union(m1, m2)
    val compiled = Compiler.compile {
      val a = BigInt(132)
      AssocMap.union(m1, m2)
    }
    // println(compiled.pretty.render(100))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    // println(Cek.evalUPLC(term).pretty.render(100))
    assert(
      AssocMap.toList(m3) == List(
        (BigInt(1), These(2, 3)),
        (BigInt(0), This(BigInt(0))),
        (BigInt(3), That(BigInt(4)))
      )
    )
    def equalsAssets(
        a: AssocMap[BigInt, BigInt],
        b: AssocMap[BigInt, BigInt]
    ): Boolean = {
      val combined = AssocMap.toList(AssocMap.union(a, b))
      // all values are equal, absent values are 0
      List.foldLeft(combined, true) { case (acc, pair) =>
        pair._2 match
          case These(v1, v2) => acc && v1 === v2
          case This(v1)      => acc && v1 === BigInt(0)
          case That(v2)      => acc && v2 === BigInt(0)
      }
    }
    {
      val compiled = Compiler.compile {
        equalsAssets(m1, m2)
      }
      // println(compiled.pretty.render(100))
      val term = new SimpleSirToUplcLowering().lower(compiled)
      // println(Cek.evalUPLC(term).pretty.render(100))
    }
    {
      val compiled = Compiler.compile {
        equalsAssets(m1, m1)
      }
      val term = new SimpleSirToUplcLowering().lower(compiled)
      // println(Cek.evalUPLC(term).pretty.render(100))
    }
  }

  test("toList(fromList(lst)) == lst") {
    check { (lst: List[(BigInt, Boolean)]) =>
      AssocMap.toList(AssocMap.fromList(lst)) == lst
    }
  }

  test("insert") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(map)(k, v)
      val lst1 = AssocMap.toList(m1).toList
      lst1.contains((k, v))
    }
  }

  test("lookup") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(map)(k, v)
      AssocMap.lookup(m1)(k) == Maybe.Just(v)
    }
  }

  test("delete") {
    check { (map: AssocMap[BigInt, BigInt], k: BigInt, v: BigInt) =>
      val m1 = AssocMap.insert(map)(k, v)
      AssocMap.lookup(m1)(k) == Maybe.Just(v)
      val m2 = AssocMap.delete(m1)(k)
      AssocMap.lookup(m2)(k) == Maybe.Nothing
    }
  }

}
