package advent.day12

object Main {

  def main(args: Array[String]) {
    val json = io.Source.fromFile(args(0)).getLines.toList.mkString("\n")
    println(part1(json))
    println(part2(json))
  }

  def part1(json: String) = {
    val digitOrMinus = (c: Char) => c == '-' || c.isDigit
    def sum(s: String): Int = {
      if (s.isEmpty) {
        0
      } else {
        val (num, rest) = s.span(digitOrMinus)
        num.toInt + sum(rest dropWhile (c => ! digitOrMinus(c)))
      }
    }
    val res = sum(json dropWhile (c => ! digitOrMinus(c)))
    res
  }

  def part2(json: String) = {
    import scala.util.parsing.json.JSON
    JSON.globalNumberParser = _.toInt
    val parsed = scala.util.parsing.json.JSON.parseFull(json)
    def total(j: Any): Int = j match {
      case i: Int       => i
      case l : List[_] => l.map(total).sum
      case m: Map[_, _] if m.values.toList.contains("red") => 0
      case m: Map[_, _] => m.values.map(total).sum
      case s: String    => 0
      case o            => sys error s"Unrecognized json value of type ${o.getClass}"
    }
    total(parsed.get)
  }

}
