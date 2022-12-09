package scalus

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

import org.scalacheck.{Arbitrary, Prop, Properties}
import Prelude.AssocMap
import Prelude.AssocMap.*
import Prelude.List
import scalus.uplc.ArbitraryInstances
import scala.collection.immutable

object AssocMapProperties extends Properties("AssocMap") with ArbitraryInstances {

  given arbList[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary {
    for {
      lst <- Arbitrary.arbitrary[immutable.List[A]]
    } yield List(lst: _*)
  }

  given Arbitrary[AssocMap[BigInt, String]] = Arbitrary {
    for {
      lst <- Arbitrary.arbitrary[List[(BigInt, String)]]
    } yield fromList(lst)
  }

  property("toList") = Prop.forAll { (lst: List[(BigInt, String)]) =>
    fromList(lst) == fromList(lst)
  }

}
