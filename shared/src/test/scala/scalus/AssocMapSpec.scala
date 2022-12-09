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
import Prelude.List

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
      val m2 = AssocMap.delete(k)(m1)
      AssocMap.lookup(k)(m2) == Prelude.Maybe.Nothing
    }
  }

}
