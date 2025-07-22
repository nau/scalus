package scalus.prelude

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.ledger.api.v2.OutputDatum
import scalus.prelude.List.*

import scalus.prelude.Option.{None, Some}
import scalus.uplc.test.ArbitraryInstances

private object ListTest {
    val testScalusList1: List[BigInt] = List(1, 2, 3)
    val testScalaList1: scala.List[BigInt] = scala.List(1, 2, 3)
}

class ListTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    import ListTest.*

    test("empty") {
        assert(List.empty[Boolean] === Nil)
        assert(scala.List.empty[Boolean].asScalus === scala.Nil.asScalus)
        assert(List.empty[Boolean].asScala == Nil.asScala)

        assert(List.empty[Boolean] !== List(true))
        assert(scala.List.empty[Boolean].asScalus !== scala.List(true).asScalus)
        assert(List.empty[Boolean].asScala != List(true).asScala)
    }

    test("isEmpty") {
        assert(List.empty.isEmpty)
        assert(scala.List.empty.asScalus.isEmpty)
        assert(List.empty.asScala.isEmpty)

        assert(!List(true).isEmpty)
        assert(!scala.List(true).asScalus.isEmpty)
        assert(!List(true).asScala.isEmpty)
    }

    test("nonEmpty") {
        assert(!List.empty.nonEmpty)
        assert(!scala.List.empty.asScalus.nonEmpty)
        assert(!List.empty.asScala.nonEmpty)

        assert(List(true).nonEmpty)
        assert(scala.List(true).asScalus.nonEmpty)
        assert(List(true).asScala.nonEmpty)
    }

    test("get") {
        assert(List.empty[BigInt].get(0) === None)

        assert(testScalusList1.get(0) === Some[BigInt](1))
        assert(testScalusList1.get(3) === None)
        assert(testScalusList1.get(-1) === None)

        check { (list: List[BigInt], value: BigInt) =>
            list.prepended(value).get(0) === Some(value)
        }
    }

    test("map") {
        assert(List.empty[BigInt].map(_ * 2) === Nil)
        assert(scala.List.empty[BigInt].map(_ * 2).asScalus === scala.Nil.asScalus)
        assert(List.empty[BigInt].map(_ * 2).asScala == Nil.asScala)

        assert(
          testScalusList1.map(_ * 2) === List[BigInt](2, 4, 6)
        )

        assert(
          testScalaList1.map(_ * 2).asScalus === scala.List[BigInt](2, 4, 6).asScalus
        )

        assert(
          testScalusList1.map(_ * 2).asScala == List[BigInt](2, 4, 6).asScala
        )

        check { (list: List[BigInt]) =>
            list.map(_ * 2) === list.asScala.map(_ * 2).asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list.map(_ * 2) == list.asScalus.map(_ * 2).asScala
        }
    }

    test("filter") {
        assert(List.empty[BigInt].filter(_ % 2 == 1) === Nil)
        assert(scala.List.empty[BigInt].filter(_ % 2 == 1).asScalus === scala.Nil.asScalus)
        assert(List.empty[BigInt].filter(_ % 2 == 1).asScala == Nil.asScala)

        assert(
          testScalusList1.filter(_ % 2 == 1) === List[BigInt](1, 3)
        )

        assert(
          testScalaList1.filter(_ % 2 == 1).asScalus === scala.List[BigInt](1, 3).asScalus
        )

        assert(
          testScalusList1.filter(_ % 2 == 1).asScala == List[BigInt](1, 3).asScala
        )

        check { (list: List[BigInt]) =>
            list.filter(_ % 2 == 1) === list.asScala.filter(_ % 2 == 1).asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list.filter(_ % 2 == 1) == list.asScalus.filter(_ % 2 == 1).asScala
        }
    }

    test("find") {
        assert(List.empty[BigInt].find(_ > 1) === None)
        assert(scala.List.empty[BigInt].find(_ > 1).asScalus === scala.None.asScalus)
        assert(List.empty[BigInt].find(_ > 1).asScala == None.asScala)

        assert(testScalusList1.find(_ > 1) === Some[BigInt](2))
        assert(testScalaList1.find(_ > 1).asScalus === scala.Some[BigInt](2).asScalus)
        assert(testScalusList1.find(_ > 1).asScala == Some[BigInt](2).asScala)

        assert(testScalusList1.find(_ > 3) === None)
        assert(testScalaList1.find(_ > 3).asScalus === scala.None.asScalus)
        assert(testScalusList1.find(_ > 3).asScala == None.asScala)

        check { (list: List[BigInt], value: BigInt) =>
            list.find(_ > value) === list.asScala.find(_ > value).asScalus
        }

        check { (list: scala.List[BigInt], value: BigInt) =>
            list.find(_ > value) == list.asScalus.find(_ > value).asScala
        }
    }

    test("foldLeft") {
        assert(List.empty[BigInt].foldLeft(BigInt(0))(_ + _) === BigInt(0))
        assert(
          List.empty[BigInt].foldLeft(BigInt(0))(_ + _) === scala.List
              .empty[BigInt]
              .foldLeft(BigInt(0))(_ + _)
        )

        assert(testScalusList1.foldLeft(BigInt(0))(_ + _) === BigInt(6))
        assert(
          testScalusList1.foldLeft(BigInt(0))(_ + _) === testScalaList1.foldLeft(BigInt(0))(_ + _)
        )

        check { (list: List[BigInt], value: BigInt) =>
            list.foldLeft(value)(_ + _) === list.asScala.foldLeft(value)(_ + _)
        }

        check { (list: scala.List[BigInt], value: BigInt) =>
            list.foldLeft(value)(_ + _) === list.asScalus.foldLeft(value)(_ + _)
        }
    }

    test("indexOfOption") {
        assert(List.empty[BigInt].indexOfOption(BigInt(2)) === None)

        assert(testScalusList1.indexOfOption(BigInt(2)) === Some(BigInt(1)))
        assert(testScalusList1.indexOfOption(BigInt(4)) === None)

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOfOption(value)
            val scalaResult = list.asScala.indexOf(value)
            scalusResult match {
                case Some(index) => scalaResult == index
                case None        => scalaResult == -1
            }
        }

        check { (list: scala.List[BigInt], value: BigInt) =>
            val scalaResult = list.indexOf(value)
            val scalusResult = list.asScalus.indexOfOption(value)
            scalusResult match {
                case Some(index) => scalaResult == index
                case None        => scalaResult == -1
            }
        }
    }

    test("headOption") {
        assert(List.empty[BigInt].headOption === None)
        assert(scala.List.empty[BigInt].headOption.asScalus === scala.None.asScalus)
        assert(List.empty[BigInt].headOption.asScala == None.asScala)

        assert(testScalusList1.headOption === Some[BigInt](1))
        assert(testScalaList1.headOption.asScalus === scala.Some[BigInt](1).asScalus)
        assert(testScalusList1.headOption.asScala == Some[BigInt](1).asScala)

        check { (list: List[BigInt]) =>
            list.headOption === list.asScala.headOption.asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list.headOption == list.asScalus.headOption.asScala
        }
    }

    test("lastOption") {
        assert(List.empty[BigInt].lastOption === None)
        assert(scala.List.empty[BigInt].lastOption.asScalus === scala.None.asScalus)
        assert(List.empty[BigInt].lastOption.asScala == None.asScala)

        assert(testScalusList1.lastOption === Some[BigInt](3))
        assert(testScalaList1.lastOption.asScalus === scala.Some[BigInt](3).asScalus)
        assert(testScalusList1.lastOption.asScala == Some[BigInt](3).asScala)

        check { (list: List[BigInt]) =>
            list.lastOption === list.asScala.lastOption.asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list.lastOption == list.asScalus.lastOption.asScala
        }
    }

    test("length") {
        assert(List.empty[BigInt].length === BigInt(0))
        assert(List.empty[BigInt].length == scala.List.empty[BigInt].length)

        assert(testScalusList1.length === BigInt(3))
        assert(testScalusList1.length == testScalaList1.length)

        check { (list: List[BigInt]) =>
            list.length == list.asScala.length
        }

        check { (list: scala.List[BigInt]) =>
            list.length == list.asScalus.length
        }
    }

    test("reverse") {
        assert(List.empty[BigInt].reverse === Nil)
        assert(scala.List.empty[BigInt].reverse.asScalus === scala.Nil.asScalus)
        assert(List.empty[BigInt].reverse.asScala == Nil.asScala)

        assert(testScalusList1.reverse === List[BigInt](3, 2, 1))
        assert(testScalaList1.reverse.asScalus === scala.List[BigInt](3, 2, 1).asScalus)
        assert(testScalusList1.reverse.asScala == List[BigInt](3, 2, 1).asScala)

        check { (list: List[BigInt]) =>
            list.reverse === list.asScala.reverse.asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list.reverse == list.asScalus.reverse.asScala
        }
    }

    test("asScala/asScalus") {
        assert(scala.List.empty[BigInt].asScalus === List.empty[BigInt])
        assert(List.empty[BigInt].asScala == scala.List.empty[BigInt])

        assert(testScalaList1.asScalus === testScalusList1)
        assert(testScalusList1.asScala == testScalaList1)

        check { (list: List[BigInt]) =>
            list === list.asScala.asScalus
        }

        check { (list: scala.List[BigInt]) =>
            list == list.asScalus.asScala
        }
    }

    test("right assoc infix operators") {
        val ls: List[OutputDatum] =
            OutputDatum.OutputDatumHash(hex"00") +: List(OutputDatum.NoOutputDatum)
        assert(ls == List(OutputDatum.OutputDatumHash(hex"00"), OutputDatum.NoOutputDatum))
        val ls2: List[OutputDatum] =
            List(OutputDatum.OutputDatumHash(hex"00")) ++: List(OutputDatum.NoOutputDatum)
        assert(ls2 == List(OutputDatum.OutputDatumHash(hex"00"), OutputDatum.NoOutputDatum))
        val ls3 = List(BigInt(1)) ++: List(BigInt(12))
        assert(ls3 == List(BigInt(1), BigInt(12)))
    }
}
