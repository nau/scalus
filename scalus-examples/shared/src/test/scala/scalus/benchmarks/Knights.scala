package scalus.benchmarks

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Builtins.{multiplyInteger, remainderInteger}
import scalus.prelude.Eq.given
import scalus.prelude.Ord.{*, given}
import scalus.prelude.{*, given}
import scalus.testkit.ScalusTest

class Knights extends AnyFunSuite, ScalusTest:
end Knights

@Compile
object Knights:
    enum Direction:
        case UL, UR, DL, DR, LU, LD, RU, RD

    val directions: List[Direction] = {
        import Direction.*
        List.Cons(
          UL,
          List.Cons(
            UR,
            List.Cons(
              DL,
              List.Cons(DR, List.Cons(LU, List.Cons(LD, List.Cons(RU, List.Cons(RD, List.Nil)))))
            )
          )
        )
    }

    type Tile = (BigInt, BigInt)

    extension (self: Tile)
        def move(direction: Direction): Tile =
            val (x, y) = self
            import Direction.*
            direction match
                case UL => (x - 1, y - 2)
                case UR => (x + 1, y - 2)
                case DL => (x - 1, y + 2)
                case DR => (x + 1, y + 2)
                case LU => (x - 2, y - 1)
                case LD => (x - 2, y + 1)
                case RU => (x + 2, y - 1)
                case RD => (x + 2, y + 1)

    end extension

    type Solution = List[(BigInt, ChessSet)]

    case class ChessSet(
        size: BigInt,
        moveNumber: BigInt,
        start: Option[Tile],
        visited: List[Tile]
    )

    def createBoard(size: BigInt, initSquare: Tile): ChessSet =
        ChessSet(
          size = size,
          moveNumber = BigInt(1),
          start = Option.Some(initSquare),
          visited = List.single(initSquare)
        )

    def startTour(tile: Tile, size: BigInt): ChessSet =
        require(remainderInteger(size, BigInt(2)) === BigInt(0))
        createBoard(size, tile)

    given Ord[ChessSet] = Ord.by[ChessSet, List[Tile]](_.visited)

    extension (self: ChessSet)
        def addPiece(tile: Tile): ChessSet =
            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber + 1,
              start = self.start,
              visited = self.visited.prepended(tile)
            )

        def firstPiece: Tile = self.start.get
        def lastPiece: Tile = self.visited.head

        def deleteFirst: ChessSet =
            extension [A](self: List[A])
                def secondLast: Option[A] =
                    self.reverse match
                        case List.Nil => fail()
                        case List.Cons(_, tail) =>
                            tail match
                                case List.Nil            => Option.None
                                case List.Cons(value, _) => Option.Some(value)

            end extension

            val newVisited = self.visited.init

            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber - 1,
              start = self.visited.secondLast,
              visited = newVisited
            )

        def isSquareFree(tile: Tile): Boolean = !self.visited.contains(tile)

        def canMoveTo(tile: Tile): Boolean =
            val (x, y) = tile
            val size = self.size
            x >= 1 && x <= size && y >= 1 && y <= size && isSquareFree(tile)

        def canMove(direction: Direction): Boolean = canMoveTo(lastPiece.move(direction))
        def moveKnight(direction: Direction): ChessSet = addPiece(lastPiece.move(direction))
        def possibleMoves: List[Direction] = directions.filter(canMove)
        def allDescend: List[ChessSet] = possibleMoves.map(moveKnight)

        def descAndNo: Solution = allDescend.map { item =>
            (item.deleteFirst.possibleMoves.length, item)
        }

        def singleDescend: List[ChessSet] =
            descAndNo.filterMap { item =>
                val (moves, board) = item
                if moves === BigInt(1) then Option.Some(board) else Option.None
            }

        def isDeadEnd: Boolean = possibleMoves.isEmpty
        def canJumpFirst: Boolean = deleteFirst.canMoveTo(firstPiece)

        def descendants: List[ChessSet] = {
            extension [A](self: List[A])
                def quicksort[B >: A: Ord]: List[A] =
                    self match
                        case List.Nil => List.Nil
                        case List.Cons(head, tail) =>
                            val before = tail.filter { elem => (elem <=> head).isLess }.quicksort
                            val after = tail.filter { elem => !(elem <=> head).isLess }.quicksort
                            before ++ after.prepended(head)
            end extension

            if canJumpFirst && addPiece(firstPiece).isDeadEnd then List.empty
            else
                val singles = singleDescend
                singles match
                    case List.Nil              => descAndNo.quicksort.map { _._2 }
                    case List.Cons(head, tail) => if tail.isEmpty then singles else List.empty
        }

        def isTourFinished: Boolean =
            self.moveNumber === multiplyInteger(self.size, self.size) && canJumpFirst

    end extension

end Knights
