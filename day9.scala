package advent.day9

object Main {

  def main(args: Array[String]) {
    val map = parse(io.Source.fromFile(args(0)).getLines.toList)
    val cities = map.keySet
    val shortest = (is: List[Int]) => is.min
    val longest = (is: List[Int]) => is.max
    val shortestPath = shortest(cities.toList.map(start => mostest(start, 0, cities - start, map, shortest)))
    val longestPath = longest(cities.toList.map(start => mostest(start, 0, cities - start, map, longest)))
    println(shortestPath)
    println(longestPath)
  }

  def mostest(current: String, travelled: Int, toVisit: Set[String], map: Map[String, Map[String, Int]], choose: List[Int] => Int): Int = {
    if (toVisit.isEmpty) {
      travelled
    } else {
      choose(toVisit.toList.map{ next =>
        mostest(next, travelled + map(current)(next), toVisit - next, map, choose)
      })
    }
  }

  def parse(lines: List[String]): Map[String, Map[String, Int]] = {
    import collection.mutable.{ Map => MutMap }
    val map = MutMap.empty[String, MutMap[String, Int]]
    lines.foreach{ line =>
      line.split(" ") match {
        case Array(start, "to", end, "=", distance) =>
          map.getOrElseUpdate(start, MutMap.empty)(end) = distance.toInt
          map.getOrElseUpdate(end, MutMap.empty)(start) = distance.toInt
      }
    }
    map.mapValues(_.toMap).toMap
  }

}
