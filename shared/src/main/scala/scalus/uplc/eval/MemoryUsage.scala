package scalus.uplc.eval

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.eval.CekValue.*

trait MemoryUsage[A]:
    def memoryUsage(a: A): CostingInteger

object MemoryUsage {
    def memoryUsageInteger(i: BigInt): CostingInteger = i.bitLength / 64

    def memoryUsageByteString(bs: ByteString): CostingInteger = bs.bytes.length / 8 + 1

    def memoryUsageString(s: String): CostingInteger = s.length

    def memoryUsageData(d: Data): CostingInteger = {
        val nodeMem = 4L
        val usage = d match
            case Data.I(i)  => memoryUsageInteger(i)
            case Data.B(bs) => memoryUsageByteString(bs)
            case Data.Constr(_, l) =>
                l.foldLeft(0L)((acc, d) => acc + memoryUsageData(d))
            case Data.Map(l) =>
                l.foldLeft(0L) { case (acc, (k, v)) =>
                    acc + memoryUsageData(k) + memoryUsageData(v)
                }
            case Data.List(l) =>
                l.foldLeft(0L)((acc, d) => acc + memoryUsageData(d))
        // The cost of each node of the 'Data' object (in addition to the cost of its content).
        nodeMem + usage
    }

    def memoryUsage(a: CekValue): CostingInteger = a match
        case VCon(const)                 => memoryUsage(const)
        case VDelay(term, env)           => 1
        case VLamAbs(name, term, env)    => 1
        case VBuiltin(bn, term, runtime) => 1

    def memoryUsage(a: Constant): CostingInteger = a match
        case Constant.Integer(i)     => memoryUsageInteger(i)
        case Constant.ByteString(bs) => memoryUsageByteString(bs)
        case Constant.String(s)      => memoryUsageString(s)
        case Constant.Unit           => 1
        case Constant.Bool(_)        => 1
        case Constant.Data(d)        => memoryUsageData(d)
        case Constant.List(tpe, l)   => l.foldLeft(0L)((acc, d) => acc + memoryUsage(d))
        case Constant.Pair(a, b)     => 1 + memoryUsage(a) + memoryUsage(b)
}
