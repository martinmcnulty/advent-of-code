package advent.day6

object Main {

  case class Coord(x: Int, y: Int)
  case class Range(from: Coord, to: Coord)

  sealed abstract trait Command
  case object TurnOn extends Command
  case object TurnOff extends Command
  case object Toggle extends Command

  case class Instruction(cmd: Command, range: Range)

  def main(args: Array[String]) {
    val instrs = {
      def parseCoord(s: String) = {
        val parts = s.split(",")
        try {
          Coord(parts(0).toInt, parts(1).toInt)
        } catch {
          case e: Exception => throw new RuntimeException(s"Could not parse [$s] as a co-ordinate", e)
        }
      }
      io.Source.fromFile(new java.io.File(args(0))).getLines.toList map {
        line => try {
        line.split(" ") match {
          case Array("turn", "off", start, "through", end) =>
            Instruction(TurnOff, Range(parseCoord(start), parseCoord(end)))
          case Array("turn", "on", start, "through", end) =>
            Instruction(TurnOn, Range(parseCoord(start), parseCoord(end)))
          case Array("toggle", start, "through", end) =>
            Instruction(Toggle, Range(parseCoord(start), parseCoord(end)))
        }
        } catch {
          case e: Exception => throw new RuntimeException(s"Could not parse line: [$line]", e)
        }
      }
    }
    println(part2(instrs))
  }

  def part1(instrs: List[Instruction]): Int = {
    // All-off initial grid
    val grid = new Array[Array[Boolean]](1000)
    0 until grid.length foreach { i => grid(i) = new Array[Boolean](1000) }

    // Interpret the instructions
    instrs foreach { instr =>
      val f: Boolean => Boolean = instr.cmd match {
        case TurnOn  => _ => true
        case TurnOff => _ => false
        case Toggle  => ! _
      }
      instr.range.from.x to instr.range.to.x foreach { i =>
        instr.range.from.y to instr.range.to.y foreach { j =>
          grid(i)(j) = f(grid(i)(j))
        }
      }
    }

    // Count the lights left on
    val onCount = grid.map(_ count (identity)).sum
    onCount
  }

  def part2(instrs: List[Instruction]): Int = {
    // Zero initial grid
    val grid = new Array[Array[Int]](1000)
    0 until grid.length foreach { i => grid(i) = new Array[Int](1000) }

    // Interpret the instructions
    instrs foreach { instr =>
      val f: Int => Int = instr.cmd match {
        case TurnOn  => _ + 1
        case TurnOff => _ - 1 max 0
        case Toggle  => _ + 2
      }
      instr.range.from.x to instr.range.to.x foreach { i =>
        instr.range.from.y to instr.range.to.y foreach { j =>
          grid(i)(j) = f(grid(i)(j))
        }
      }
    }

    // Brightness count
    grid.map(_.sum).sum
  }
}
