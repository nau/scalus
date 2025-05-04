package scalus.benchmarks.knights

import scalus.*
import scalus.prelude.*

enum Direction:
    case UL, UR, DL, DR, LU, LD, RU, RD

@Compile
object Direction:
    val list: List[Direction] = List.Cons(
      UL,
      List.Cons(
        UR,
        List.Cons(
          DL,
          List.Cons(DR, List.Cons(LU, List.Cons(LD, List.Cons(RU, List.Cons(RD, List.Nil)))))
        )
      )
    )

end Direction
