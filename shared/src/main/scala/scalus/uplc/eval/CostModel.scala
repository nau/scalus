package scalus.uplc.eval
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

case class TwoVariableLinearFunction(intercept: Intercept, slopeX: Slope, slopeY: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        intercept + arg1 * slopeX + arg2 * slopeY
    }
}

case class ModelConstantOrLinear(constant: CostingInteger, intercept: Intercept, slope: Slope)
    derives ReadWriter {
    def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger = {
        if arg1 == arg2 then intercept + arg1 * slope else constant
    }
}

case class ConstantOrTwoArguments(constant: CostingInteger, model: TwoArguments) derives ReadWriter

trait CostModel {
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

    case class LinearCost(costFun: OneVariableLinearFunction) extends OneArgument {
        def apply(arg: CostingInteger): CostingInteger =
            costFun(arg)
    }

    given ReadWriter[OneArgument] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case LinearCost(cost) =>
              ujson.Obj("type" -> "linear_cost", "arguments" -> write(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" =>
                  ConstantCost(json.obj("arguments").num.toLong)
              case "linear_cost" =>
                  LinearCost(read[OneVariableLinearFunction](json.obj("arguments")))
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
            cost.intercept + arg1 * cost.slopeX + arg2 * cost.slopeY
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

    case class LinearOnDiagonal(cost: ModelConstantOrLinear) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            cost(arg1, arg2)
    }
    case class ConstAboveDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 < arg2 then cost.model(arg1, arg2) else cost.constant
    }
    case class ConstBelowDiagonal(cost: ConstantOrTwoArguments) extends TwoArguments {
        def apply(arg1: CostingInteger, arg2: CostingInteger): CostingInteger =
            if arg1 > arg2 then cost.model(arg1, arg2) else cost.constant
    }

    given ReadWriter[TwoArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> write(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> write(costFun))
          case LinearInXAndY(cost) =>
              ujson.Obj("type" -> "linear_in_x_and_y", "arguments" -> write(cost))
          case AddedSizes(cost) =>
              ujson.Obj("type" -> "added_sizes", "arguments" -> write(cost))
          case SubtractedSizes(cost) =>
              ujson.Obj("type" -> "subtracted_sizes", "arguments" -> write(cost))
          case MultipliedSizes(cost) =>
              ujson.Obj("type" -> "multiplied_sizes", "arguments" -> write(cost))
          case MinSize(cost) =>
              ujson.Obj("type" -> "min_size", "arguments" -> write(cost))
          case MaxSize(cost) =>
              ujson.Obj("type" -> "max_size", "arguments" -> write(cost))
          case LinearOnDiagonal(cost) =>
              ujson.Obj("type" -> "linear_on_diagonal", "arguments" -> write(cost))
          case ConstAboveDiagonal(cost) =>
              ujson.Obj("type" -> "const_above_diagonal", "arguments" -> write(cost))
          case ConstBelowDiagonal(cost) =>
              ujson.Obj("type" -> "const_below_diagonal", "arguments" -> write(cost))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_x_and_y" =>
                  LinearInXAndY(read[TwoVariableLinearFunction](json.obj("arguments")))
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
                  LinearOnDiagonal(read[ModelConstantOrLinear](json.obj("arguments")))
              case "const_above_diagonal" =>
                  ConstAboveDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
              case "const_below_diagonal" =>
                  ConstBelowDiagonal(read[ConstantOrTwoArguments](json.obj("arguments")))
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

    case class AddedSizes(cost: OneVariableLinearFunction) extends ThreeArguments {
        def apply(
            arg1: CostingInteger,
            arg2: CostingInteger,
            arg3: CostingInteger
        ): CostingInteger =
            cost(arg1 + arg2 + arg3)
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

    given ReadWriter[ThreeArguments] = readwriter[ujson.Value].bimap(
      {
          case ConstantCost(cost) =>
              ujson.Obj("type" -> "constant_cost", "arguments" -> cost)
          case AddedSizes(cost) =>
              ujson.Obj("type" -> "added_sizes", "arguments" -> write(cost))
          case LinearInX(costFun) =>
              ujson.Obj("type" -> "linear_in_x", "arguments" -> write(costFun))
          case LinearInY(costFun) =>
              ujson.Obj("type" -> "linear_in_y", "arguments" -> write(costFun))
          case LinearInZ(costFun) =>
              ujson.Obj("type" -> "linear_in_z", "arguments" -> write(costFun))
      },
      json => {
          json.obj("type").str match
              case "constant_cost" => ConstantCost(json.obj("arguments").num.toLong)
              case "added_sizes" =>
                  AddedSizes(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_x" =>
                  LinearInX(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_y" =>
                  LinearInY(read[OneVariableLinearFunction](json.obj("arguments")))
              case "linear_in_z" =>
                  LinearInZ(read[OneVariableLinearFunction](json.obj("arguments")))
              case other => throw new RuntimeException(s"Unexpected type ${other}")
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

case class CostingFun[+M <: CostModel](cpu: M, memory: M) derives ReadWriter {
    def calculateCost(args: Seq[CekValue]): ExBudget = {
        val argsMem = args.map(MemoryUsage.memoryUsage)
        val cpu = ExCPU(this.cpu.calculateCost(argsMem))
        val mem = ExMemory(this.memory.calculateCost(argsMem))
        ExBudget(cpu, mem)
    }
}
