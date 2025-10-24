package scalus.uplc
package eval
import upickle.default.*

type CostingInteger = Long
type Intercept = CostingInteger
type Slope = CostingInteger

case class OneVariableLinearFunction(intercept: Intercept, slope: Slope) derives ReadWriter {
    def apply(arg: CostingInteger): CostingInteger = {
        intercept + arg * slope
    }
}

case class SubtractedSizesLinearFunction(
    intercept: Intercept,
    slope: Slope,
    minimum: CostingInteger
) derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        intercept + slope * Math.max(minimum, arg1 - arg2)
    }
}

case class TwoVariableLinearFunction(intercept: Intercept, slope1: Slope, slope2: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        intercept + arg1 * slope1 + arg2 * slope2
    }
}

case class OneVariableQuadraticFunction(
    c0: CostingInteger,
    c1: CostingInteger,
    c2: CostingInteger
) derives ReadWriter {
    def apply(x: CostingInteger): CostingInteger = {
        c0 + c1 * x + c2 * x * x
    }
}

/** c00 + c10*x + c01*y + c20*x^2 + c11*c*y + c02*y^2
  *
  * @note
  *   Minimum values for two-variable quadratic costing functions. Unlike most of our other costing
  *   functions our use cases for two-variable quadratic costing functions may require one or more
  *   negative coefficients, so there's a danger that we could return a negative cost. This is
  *   unlikely, but we make certain that it never happens by returning a result that is at never
  *   smaller than a minimum value that is stored along with the coefficients of the function.
  *
  * @param minimum
  * @param c00
  * @param c10
  * @param c01
  * @param c20
  * @param c11
  * @param c02
  */
case class TwoVariableQuadraticFunction(
    minimum: CostingInteger,
    c00: CostingInteger,
    c10: CostingInteger,
    c01: CostingInteger,
    c20: CostingInteger,
    c11: CostingInteger,
    c02: CostingInteger
) derives ReadWriter {
    def apply(x: CostingInteger, y: CostingInteger): CostingInteger = {
        val result = c00 + c10 * x + c01 * y + c20 * x * x + c11 * x * y + c02 * y * y
        Math.max(minimum, result)
    }
}

case class ConstantOrLinear(constant: CostingInteger, intercept: Intercept, slope: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        if arg1 == arg2 then intercept + arg1 * slope else constant
    }
}

case class ConstantOrOneArgument(constant: CostingInteger, model: OneArgument) derives ReadWriter

case class ConstantOrTwoArguments(constant: CostingInteger, model: TwoArguments) derives ReadWriter

sealed trait CostModel {
    def calculateCost(args: Seq[CostingInteger]): CostingInteger
}

trait OneArgument extends CostModel {
    def apply(arg: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args.head)
}

object OneArgument:
    case class ConstantCost(cost: CostingInteger) extends OneArgument {
        def apply(arg: CostingInteger): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends OneArgument {
        def apply(arg: CostingInteger): CostingInteger =
            costFun(arg)
    }

    given ReadWriter[OneArgument] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case LinearInX(cost) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" =>
                  ConstantCost(json.obj("arguments").num.toLong)
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )

trait TwoArguments extends CostModel {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args(0), args(1))
}

object TwoArguments {
    case class ConstantCost(cost: CostingInteger) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            costFun(arg1)
    }

    case class LinearInY(costFun: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            costFun(arg2)
    }

    case class LinearInXAndY(cost: TwoVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost.intercept + arg1 * cost.slope1 + arg2 * cost.slope2
    }

    case class AddedSizes(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1 + arg2)
    }

    case class SubtractedSizes(cost: SubtractedSizesLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }

    case class MultipliedSizes(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1 * arg2)
    }

    case class MinSize(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(Math.min(arg1, arg2))
    }

    case class MaxSize(cost: OneVariableLinearFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(Math.max(arg1, arg2))
    }

    case class LinearOnDiagonal(cost: ConstantOrLinear) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }
    case class ConstBelowDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 > arg2 then cost.constant else cost.model(arg1, arg2)
    }
    case class ConstAboveDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 < arg2 then cost.constant else cost.model(arg1, arg2)
    }

    case class ConstOffDiagonal(cost: ConstantOrOneArgument) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 != arg2 then cost.constant else cost.model(arg1)
    }

    case class QuadraticInY(cost: OneVariableQuadraticFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg2)
    }

    case class QuadraticInXAndY(cost: TwoVariableQuadraticFunction) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }

    given ReadWriter[TwoArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> writeJs(costFun))
          case LinearInXAndY(cost) =>
              ujson.Obj("type" -> "linear_in_x_and_y", "arguments" -> writeJs(cost))
          case AddedSizes(cost) =>
              ujson.Obj("type" -> "added_sizes", "arguments" -> writeJs(cost))
          case SubtractedSizes(cost) =>
              ujson.Obj("type" -> "subtracted_sizes", "arguments" -> writeJs(cost))
          case MultipliedSizes(cost) =>
              ujson.Obj("type" -> "multiplied_sizes", "arguments" -> writeJs(cost))
          case MinSize(cost) =>
              ujson.Obj("type" -> "min_size", "arguments" -> writeJs(cost))
          case MaxSize(cost) =>
              ujson.Obj("type" -> "max_size", "arguments" -> writeJs(cost))
          case LinearOnDiagonal(cost) =>
              ujson.Obj("type" -> "linear_on_diagonal", "arguments" -> writeJs(cost))
          case ConstAboveDiagonal(cost) =>
              ujson.Obj("type" -> "const_above_diagonal", "arguments" -> writeJs(cost))
          case ConstBelowDiagonal(cost) =>
              ujson.Obj("type" -> "const_below_diagonal", "arguments" -> writeJs(cost))
          case QuadraticInY(cost) =>
              ujson.Obj("type" -> "quadratic_in_y", "arguments" -> writeJs(cost))
          case QuadraticInXAndY(cost) =>
              ujson.Obj("type" -> "quadratic_in_x_and_y", "arguments" -> writeJs(cost))
          case ConstOffDiagonal(cost) =>
              ujson.Obj("type" -> "const_off_diagonal", "arguments" -> writeJs(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case "linear_in_x"   =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_x_and_y" =>
                  LinearInXAndY(read[TwoVariableLinearFunction](json.obj("arguments")))
              case "quadratic_in_y" =>
                  QuadraticInY(read[OneVariableQuadraticFunction](json.obj("arguments")))
              case "quadratic_in_x_and_y" =>
                  QuadraticInXAndY(read[TwoVariableQuadraticFunction](json.obj("arguments")))
              case "added_sizes" =>
                  AddedSizes(read[OneVariableLinearFunction](json.obj("arguments")))
              case "subtracted_sizes" =>
                  SubtractedSizes(read[SubtractedSizesLinearFunction](json.obj("arguments")))
              case "multiplied_sizes" =>
                  MultipliedSizes(read[OneVariableLinearFunction](json.obj("arguments")))
              case "min_size" =>
                  MinSize(read[OneVariableLinearFunction](json.obj("arguments")))
              case "max_size" =>
                  MaxSize(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_on_diagonal" =>
                  LinearOnDiagonal(read[ConstantOrLinear](json.obj("arguments")))
              case "const_above_diagonal" =>
                  ConstAboveDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
              case "const_below_diagonal" =>
                  ConstBelowDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
              case "const_off_diagonal" =>
                  ConstOffDiagonal(read[ConstantOrOneArgument](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait ThreeArguments extends CostModel {
    def apply(arg1: CostingInteger, arg2: CostingInteger, arg3: CostingInteger): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) = apply(args(0), args(1), args(2))
}

object ThreeArguments {
    case class ConstantCost(cost: CostingInteger) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger = cost
    }

    case class LinearInX(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg1)
    }

    case class LinearInY(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg2)
    }

    case class LinearInZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg3)
    }

    case class QuadraticInZ(costFun: OneVariableQuadraticFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg3)
    }

    case class LiteralInYOrLinearInZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            if arg2 == 0 then costFun(arg3) else arg2
    }

    case class LinearInMaxYZ(costFun: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(Math.max(arg2, arg3))
    }

    case class LinearInYAndZ(costFun: TwoVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            costFun(arg2, arg3)
    }

    given ReadWriter[ThreeArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> writeJs(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> writeJs(costFun))
          case LinearInZ(costFun) =>
              ujson.Obj("type" -> "linear_in_z", "arguments" -> writeJs(costFun))
          case QuadraticInZ(costFun) =>
              ujson.Obj("type" -> "quadratic_in_z", "arguments" -> writeJs(costFun))
          case LiteralInYOrLinearInZ(costFun) =>
              ujson.Obj("type" -> "literal_in_y_or_linear_in_z", "arguments" -> writeJs(costFun))
          case LinearInMaxYZ(costFun) =>
              ujson.Obj("type" -> "linear_in_max_yz", "arguments" -> writeJs(costFun))
          case LinearInYAndZ(costFun) =>
              ujson.Obj("type" -> "linear_in_y_and_z", "arguments" -> writeJs(costFun))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case "linear_in_x"   =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_z" =>
                  LinearInZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "quadratic_in_z" =>
                  QuadraticInZ(read[OneVariableQuadraticFunction](json.obj("arguments")))
              case "literal_in_y_or_linear_in_z" =>
                  LiteralInYOrLinearInZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_max_yz" =>
                  LinearInMaxYZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y_and_z" =>
                  LinearInYAndZ(read[TwoVariableLinearFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait FourArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3))
}

object FourArguments {
    case class ConstantCost(cost: CostingInteger) extends FourArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger
        ): CostingInteger = cost
    }

    given ReadWriter[FourArguments] = readwriter[ujson.Value].bimap(
      { case ConstantCost(cost) =>
          ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case other           => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait FiveArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger,
        arg5: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3), args(4))
}

object FiveArguments {
    case class ConstantCost(cost: CostingInteger) extends FiveArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger,
            arg5: CostingInteger
        ): CostingInteger = cost
    }

    given ReadWriter[FiveArguments] = readwriter[ujson.Value].bimap(
      { case ConstantCost(cost) =>
          ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case other           => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait SixArguments extends CostModel {
    def apply(
        arg1: CostingInteger,
        arg2: CostingInteger,
        arg3: CostingInteger,
        arg4: CostingInteger,
        arg5: CostingInteger,
        arg6: CostingInteger
    ): CostingInteger
    def calculateCost(args: Seq[CostingInteger]) =
        apply(args(0), args(1), args(2), args(3), args(4), args(5))
}

object SixArguments {
    case class ConstantCost(cost: CostingInteger) extends SixArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger,
            arg4: CostingInteger,
            arg5: CostingInteger,
            arg6: CostingInteger
        ): CostingInteger = cost
    }

    given ReadWriter[SixArguments] = readwriter[ujson.Value].bimap(
      { case ConstantCost(cost) =>
          ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case other           => throw new RuntimeException(s"Unexpected type ${other}")
      }
    )
}

trait CostingFun {
    def calculateCost(args: CekValue*): ExBudget
}

case class DefaultCostingFun[+M <: CostModel](cpu: M, memory: M) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExBudget = {
        val argsMem = args.map(MemoryUsage.memoryUsage)
        val cpu = ExCPU(this.cpu.calculateCost(argsMem))
        val mem = ExMemory(this.memory.calculateCost(argsMem))
        ExBudget(cpu, mem)
    }
}

/** When invoking `integerToByteString` built-in function, its second argument is a built-in Integer
  * but with a different size measure, specifying the width (in bytes) of the output bytestring
  * (zero-padded to the desired size). The memory consumed by the function is given by `w`, not the
  * size of `w`. Its `MemoryUsage` is equal to the number of eight-byte words required to contain
  * `w` bytes, allowing its costing function to work properly.
  *
  * @see
  *   Plutus implementation
  *   https://github.com/IntersectMBO/plutus/blob/bc8c3a765769d2c0cd41c43278f5954cfdfd9b15/plutus-core/plutus-core/src/PlutusCore/Evaluation/Machine/ExMemoryUsage.hs#L171
  */
case class IntegerToByteStringCostingFun(cpu: ThreeArguments, memory: ThreeArguments)
    extends CostingFun derives ReadWriter {
    def calculateCost(args: CekValue*): ExBudget = {
        val Seq(arg0, CekValue.VCon(Constant.Integer(size)), arg2) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsage(arg0),
          MemoryUsage.memoryUsageLiteralByteSize(size),
          MemoryUsage.memoryUsage(arg2)
        )
        val cpu = ExCPU(this.cpu.calculateCost(argsMem))
        val mem = ExMemory(this.memory.calculateCost(argsMem))
        ExBudget(cpu, mem)
    }
}

case class ReplicateByteCostingFun(cpu: TwoArguments, memory: TwoArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExBudget = {
        val Seq(CekValue.VCon(Constant.Integer(size)), arg1) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsageLiteralByteSize(size),
          MemoryUsage.memoryUsage(arg1)
        )
        val cpu = ExCPU(this.cpu.calculateCost(argsMem))
        val mem = ExMemory(this.memory.calculateCost(argsMem))
        ExBudget(cpu, mem)
    }
}

case class ShiftOrRotateByteStringCostingFun(cpu: TwoArguments, memory: TwoArguments)
    extends CostingFun derives ReadWriter {
    def calculateCost(args: CekValue*): ExBudget = {
        val Seq(arg0, CekValue.VCon(Constant.Integer(size))) = args.toSeq: @unchecked
        val argsMem = Seq(
          MemoryUsage.memoryUsage(arg0),
          MemoryUsage.memoryUsageLiteral(size)
        )
        val cpu = ExCPU(this.cpu.calculateCost(argsMem))
        val mem = ExMemory(this.memory.calculateCost(argsMem))
        ExBudget(cpu, mem)
    }
}

case class WriteBitsCostingFun(cpu: ThreeArguments, memory: ThreeArguments) extends CostingFun
    derives ReadWriter {
    def calculateCost(args: CekValue*): ExBudget = {
        val Seq(arg0, CekValue.VCon(Constant.List(_, list)), arg2) = args.toSeq: @unchecked
        val argsMem =
            Seq(MemoryUsage.memoryUsage(arg0), list.size.toLong, MemoryUsage.memoryUsage(arg2))
        ExBudget.fromCpuAndMemory(
          this.cpu.calculateCost(argsMem),
          this.memory.calculateCost(argsMem)
        )
    }
}
