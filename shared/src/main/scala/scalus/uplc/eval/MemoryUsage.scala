package scalus.uplc.eval

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.eval.CekValue.*

trait MemoryUsage[A]:
    def memoryUsage(a: A): CostingInteger

object MemoryUsage {
    def memoryUsageInteger(i: BigInt): CostingInteger = i.bitLength / 64 + 1

    def memoryUsageByteString(bs: ByteString): CostingInteger = (bs.bytes.length - 1) / 8 + 1

    def memoryUsageString(s: String): CostingInteger = s.length

    def memoryUsageData(d: Data): CostingInteger = {
        val nodeMem = 4L
        val usage = d match
            case Data.I(i)         => memoryUsageInteger(i)
            case Data.B(bs)        => memoryUsageByteString(bs)
            case Data.Constr(_, l) => sumList(l)
            case Data.Map(l) =>
                var acc = 0L
                val it = l.iterator
                while it.hasNext do
                    val t = it.next()
                    acc += memoryUsageData(t._1) + memoryUsageData(t._2)
                acc
            case Data.List(l) => sumList(l)
        // The cost of each node of the 'Data' object (in addition to the cost of its content).
        nodeMem + usage
    }

    private def sumList(l: List[Data]): CostingInteger =
        var acc = 0L
        for d <- l do acc += memoryUsageData(d)
        acc

    def memoryUsage(a: CekValue): CostingInteger = a match
        case VCon(const) => memoryUsage(const)
        case _           => 1

    def memoryUsage(a: Constant): CostingInteger = a match
        case Constant.Integer(i)     => memoryUsageInteger(i)
        case Constant.ByteString(bs) => memoryUsageByteString(bs)
        case Constant.String(s)      => memoryUsageString(s)
        case Constant.Unit           => 1
        case _: Constant.Bool        => 1
        case Constant.Data(d)        => memoryUsageData(d)
        case Constant.List(tpe, l) =>
            var acc = 0L
            for d <- l do acc += memoryUsage(d)
            acc
        case Constant.Pair(a, b) => 1 + memoryUsage(a) + memoryUsage(b)
}
