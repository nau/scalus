package scalus.prelude

import scalus.{Compile, CompileDerivations}
import scalus.builtin.Builtins.*
import scalus.builtin.{BuiltinPair, Data}

/** * A typeclass for converting values of type `A` to a `String`.
  *
  * This is used to provide a string representation of values, which can be useful for debugging,
  * logging, or displaying information about the value.
  *
  * @tparam A
  *   the type of the value to be shown
  */
@FunctionalInterface
trait Show[A] extends (A => String) with CompileDerivations {
    override def apply(v: A): String
}

extension [A: Show](self: A) inline def show: String = summon[Show[A]].apply(self)

@Compile
object Show {
    inline def apply[A: Show]: Show[A] = summon[Show[A]]

    given Show[Unit] = (x: Unit) => "()"
    given Show[BigInt] = (x: BigInt) => Prelude.showBigInt(x)
    given Show[String] = (x: String) => appendString(appendString("\"", x), "\"")
    given Show[Boolean] = (x: Boolean) => if x then "True" else "False"

    given Show[Data] = (x: Data) => {
        import scalus.builtin
        def showBuiltinList(xs: builtin.BuiltinList[Data]): String = {
            if xs.isEmpty then ""
            else
                val head = xs.head.show
                if xs.tail.isEmpty then head
                else appendString(head, appendString(", ", showBuiltinList(xs.tail)))
        }
        val showConstr = () => {
            import scalus.builtin
            val p = unConstrData(x)
            val lst = appendString("[", appendString(showBuiltinList(p.snd), "]"))
            appendString("<", appendString(p.fst.show, appendString(", ", appendString(lst, ">"))))
        }

        val showMap = () => {
            import scalus.builtin
            val lst = unMapData(x)
            def showDataPair(x: BuiltinPair[Data, Data]): String = {
                val fstShow = x.fst.show
                val sndShow = x.snd.show
                appendString(appendString(fstShow, ": "), sndShow)
            }
            def go(xs: builtin.BuiltinList[BuiltinPair[Data, Data]]): String = {
                if xs.isEmpty then ""
                else
                    val head = showDataPair(xs.head)
                    if xs.tail.isEmpty then head
                    else appendString(head, appendString(", ", go(xs.tail)))
            }
            appendString("{", appendString(go(lst), "}"))
        }
        val showList = () => {
            val lst = unListData(x)
            appendString("[", appendString(showBuiltinList(lst), "]"))
        }
        val showI = () => unIData(x).show
        val showB = () => unBData(x).show
        val f: () => String = chooseData(x, showConstr, showMap, showList, showI, showB)
        f()
    }
}
