package scalus.prelude

import scalus.prelude.List.{Cons, Nil}
import scalus.prelude.Option.{None, Some}

private object ListTest {
    val testScalusList1: List[BigInt] = List(1, 2, 3)
    val testScalaList1: scala.List[BigInt] = scala.List(1, 2, 3)
}

class ListTest extends StdlibTestKit {
    import ListTest.*

    test("empty") {
        assertEvalEq(List.empty[BigInt], Nil)

        assert(scala.List.empty[BigInt].asScalus === List.empty[BigInt])
        assert(List.empty[BigInt].asScala == scala.List.empty[BigInt])
    }

    test("single") {
        check { (value: BigInt) =>
            val scalusList = List.single(value)
            val scalaList = scala.List(value)

            scalusList === Cons(
              value,
              Nil
            ) && scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }

        assertEvalEq(List.single(BigInt(1)), Cons(BigInt(1), Nil))
    }

    test("apply") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusList = List(seq*)
            val scalaList = scala.List(seq*)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }
    }

    test("from IterableOnce") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusList = List.from(seq)
            val scalaList = scala.List.from(seq)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }
    }

    test("from java.lang.Iterable") {
        check { (seq: scala.Seq[BigInt]) =>
            import scala.jdk.CollectionConverters.*

            val scalusList = List.from(seq.asJava)
            val scalaList = scala.List.from(seq)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }
    }

    test("range") {
        forAll(`bigIntRangeGen[-5, 10]`) { (start: BigInt, end: BigInt) =>
            val scalusList = List.range(start, end - 1)
            val scalaList = scala.List.range(start, end)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }

        assertEvalEq(List.range(0, 0), List.single(BigInt(0)))
        assertEvalEq(List.range(1, 3), Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))))
        assertEvalEq(List.range(0, -1), List.empty[BigInt])
    }

    test("rangeUntil") {
        forAll(`bigIntRangeGen[-5, 10]`) { (start: BigInt, end: BigInt) =>
            val scalusList = List.rangeUntil(start, end)
            val scalaList = scala.List.range(start, end)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }

        assertEvalEq(List.rangeUntil(0, 1), List.single(BigInt(0)))
        assertEvalEq(List.rangeUntil(1, 4), Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))))
        assertEvalEq(List.rangeUntil(0, 0), List.empty[BigInt])
        assertEvalEq(List.rangeUntil(0, -1), List.empty[BigInt])
    }

    test("fill") {
        val generator: Gen[(BigInt, BigInt)] =
            for
                value <- Arbitrary.arbitrary[BigInt]
                times <- Gen.choose(BigInt(-1), BigInt(10))
            yield (value, times)

        forAll(generator) { (value: BigInt, times: BigInt) =>
            val scalusList = List.fill(value, times)
            val scalaList = scala.List.fill(times.toInt)(value)

            scalaList.asScalus === scalusList && scalusList.asScala == scalaList
        }

        assertEvalEq(List.fill(BigInt(1), 1), List.single(BigInt(1)))

        assertEvalEq(
          List.fill(BigInt(1), 3),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil)))
        )

        assertEvalEq(List.fill(BigInt(1), 0), List.empty[BigInt])

        assertEvalEq(List.fill(BigInt(1), -1), List.empty[BigInt])
    }

    test("map2") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = List.map2(list1, list2)(_ + _)
            val scalaResult = list1.asScala.zip(list2.asScala).map { case (a, b) => a + b }.asScalus

            scalusResult === scalaResult
        }

        check { (list1: scala.List[BigInt], list2: scala.List[BigInt]) =>
            val scalaResult = list1.zip(list2).map { case (a, b) => a + b }
            val scalusResult = List.map2(list1.asScalus, list2.asScalus)(_ + _).asScala

            scalaResult == scalusResult
        }

        check { (list: List[BigInt]) =>
            List.map2(list, List.empty[BigInt])(_ + _) === List.empty[BigInt] &&
            List.map2(List.empty[BigInt], list)(_ + _) === List.empty[BigInt]
        }

        assertEvalEq(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), List.empty[BigInt])(_ + _),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.map2(List.empty[BigInt], Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), Cons(BigInt(3), Cons(BigInt(4), Nil)))(
            _ + _
          ),
          Cons(BigInt(4), Cons(BigInt(6), Nil))
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            list === fromDataList
        }

        assertEvalEq(
          fromData[List[BigInt]](List.empty[BigInt].toData),
          List.empty[BigInt]
        )

        assertEvalEq(
          fromData[List[BigInt]](List.single(BigInt(1)).toData),
          List.single(BigInt(1))
        )

        assertEvalEq(
          fromData[List[BigInt]](Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).toData),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("Eq") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        check { (list1: scala.List[BigInt], list2: scala.List[BigInt]) =>
            val scalaResult = list1 == list2
            val scalusResult = list1.asScalus === list2.asScalus

            scalaResult === scalusResult
        }

        assertEvalEq(List.empty[BigInt], List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )

        assertEvalNotEq(
          List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalNotEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(0), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

//    test("Ord") {
//
//    }

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

//    test("right assoc infix operators") {
//        val ls: List[OutputDatum] =
//            OutputDatum.OutputDatumHash(hex"00") +: List(OutputDatum.NoOutputDatum)
//        assert(ls == List(OutputDatum.OutputDatumHash(hex"00"), OutputDatum.NoOutputDatum))
//        val ls2: List[OutputDatum] =
//            List(OutputDatum.OutputDatumHash(hex"00")) ++: List(OutputDatum.NoOutputDatum)
//        assert(ls2 == List(OutputDatum.OutputDatumHash(hex"00"), OutputDatum.NoOutputDatum))
//        val ls3 = List(BigInt(1)) ++: List(BigInt(12))
//        assert(ls3 == List(BigInt(1), BigInt(12)))
//    }

    private val `bigIntRangeGen[-5, 10]`: Gen[(BigInt, BigInt)] =
        for {
            start <- Gen.choose(BigInt(-5), BigInt(5))
            end <- Gen.choose(BigInt(0), BigInt(10))
        } yield (start, end)
}
