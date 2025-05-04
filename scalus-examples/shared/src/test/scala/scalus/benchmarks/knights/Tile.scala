package scalus.benchmarks.knights

import scalus.*
import scalus.prelude.{*, given}
import scalus.prelude.Ord.{*, given}

case class Tile(x: BigInt, y: BigInt)

@Compile
object Tile:
    given Eq[Tile] =
        import Eq.orElseBy
        Eq.by[Tile, BigInt](_.x).orElseBy(_.y)

    given Ord[Tile] = Ord.by[Tile, BigInt](_.x).orElseBy(_.y)

    extension (self: Tile)
        def move(direction: Direction): Tile =
            val Tile(x, y) = self
            import Direction.*
            direction match
                case UL => Tile(x - 1, y - 2)
                case UR => Tile(x + 1, y - 2)
                case DL => Tile(x - 1, y + 2)
                case DR => Tile(x + 1, y + 2)
                case LU => Tile(x - 2, y - 1)
                case LD => Tile(x - 2, y + 1)
                case RU => Tile(x + 2, y - 1)
                case RD => Tile(x + 2, y + 1)

    end extension

end Tile
