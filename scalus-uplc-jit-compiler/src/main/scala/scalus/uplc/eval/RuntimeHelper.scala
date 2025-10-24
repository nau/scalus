package scalus.uplc.eval
import scalus.uplc.*
import scalus.builtin.*

object RuntimeHelper {

    def anyUplcConstant(in: Any): Constant = {
        in match
            case i: Int          => Constant.Integer(i)
            case s: String       => Constant.String(s)
            case b: Boolean      => Constant.Bool(b)
            case bs: ByteString  => Constant.ByteString(bs)
            case d: Data         => Constant.Data(d)
            case p: Tuple2[?, ?] => Constant.Pair(anyUplcConstant(p._1), anyUplcConstant(p._2))
            case l @ Constant.List(t, v) => l
            case _ => throw new IllegalArgumentException(s"Unsupported type: ${in.getClass}")
    }

}
