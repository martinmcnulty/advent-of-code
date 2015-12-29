package advent.day13

object Main {

  case class HappinessReln(p1: String, p2: String, delta: Int)

  def main(args: Array[String]) {
    val relations = io.Source.fromFile(args(0)).getLines.map(parseLine).toList
    val people = (relations.map(_.p1) ++ relations.map(_.p2)).distinct.sorted
    val relnsMap = relations.groupBy(_.p1).mapValues{
      _.groupBy(_.p2).mapValues(_.head)
    }
    println(part1(relnsMap))
    println(part2(relnsMap))
  }

  def part1(relnsMap: Map[String, Map[String, HappinessReln]]): Int = {
    val people = relnsMap.keySet.toList
    val happinesses = people.permutations.toList.map{
      perm => perm -> overallHappiness(perm, relnsMap)
    }
    val maxHappiness = happinesses.map(_._2).max
    maxHappiness
  }

  def part2(relnsMap: Map[String, Map[String, HappinessReln]]): Int = {
    val me = "Zoidberg"
    val people = me :: relnsMap.keySet.toList
    val withMe = {
      val meToThem = Map(me -> people.map(p => p -> HappinessReln(me, p, 0)).toMap)
      relnsMap.map{
        case (p, map) =>
          p -> (map + (me -> HappinessReln(p, me, 0)))
      } ++ meToThem
    }
    val happinesses = people.permutations.toList.map{
      perm => perm -> overallHappiness(perm, withMe)
    }
    val maxHappiness = happinesses.map(_._2).max
    maxHappiness
  }

  def parseLine(line: String): HappinessReln = {
    line.split(" ") match {
      case Array(p1, _, "gain", delta, _, _, _, _, _, _, p2) =>
        HappinessReln(p1, p2 dropRight 1, delta.toInt)
      case Array(p1, _, "lose", delta, _, _, _, _, _, _, p2) =>
        HappinessReln(p1, p2 dropRight 1, -delta.toInt)
    }
  }

  def overallHappiness(perm: List[String], relns: Map[String, Map[String, HappinessReln]]): Int = {
    val pairwise = perm.sliding(2).map{
      case List(p1, p2) => relns(p1)(p2).delta + relns(p2)(p1).delta
    }
    pairwise.sum + relns(perm.head)(perm.last).delta + relns(perm.last)(perm.head).delta
  }

}
