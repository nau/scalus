package scalus.uplc.eval

import cats.kernel.Group

opaque type ExCPU <: Long = Long
object ExCPU {
    inline def apply(l: Long): ExCPU = l
}
opaque type ExMemory <: Long = Long
object ExMemory {
    inline def apply(l: Long): ExMemory = l
}

case class ExBudget(cpu: ExCPU, memory: ExMemory) {
    def showJson: String =
        val memoryFormatted = String.format("%.6f", memory / 1000000d)
        val cpuFormatted = String.format("%.6f", cpu / 1000000d)
        s"{ mem: $memoryFormatted, cpu: $cpuFormatted }"
}

object ExBudget {

    /** The zero budget */
    val zero: ExBudget = ExBudget(ExCPU(0), ExMemory(0))
    val enormous: ExBudget = ExBudget(ExCPU(Long.MaxValue), ExMemory(Long.MaxValue))

    /** Constructs an 'ExBudget' from CPU and memory components. */
    def fromCpuAndMemory(cpu: Long, memory: Long): ExBudget = ExBudget(ExCPU(cpu), ExMemory(memory))

    /// Cats Group instance for ExBudget
    given Group[ExBudget] with
        def combine(x: ExBudget, y: ExBudget): ExBudget =
            ExBudget(ExCPU(x.cpu + y.cpu), ExMemory(x.memory + y.memory))
        def empty: ExBudget = ExBudget.zero
        def inverse(x: ExBudget): ExBudget = ExBudget(ExCPU(-x.cpu), ExMemory(-x.memory))

    given Ordering[ExBudget] with
        def compare(x: ExBudget, y: ExBudget): Int =
            val c = x.cpu.compareTo(y.cpu)
            if c != 0 then c else x.memory.compareTo(y.memory)
}
