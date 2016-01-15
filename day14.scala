package advent.day14

object Main {

  case class Reindeer(name: String, speed: Int, flyTime: Int, restTime: Int) {
    def cycleTime = flyTime + restTime
    def cycleDistance = speed * flyTime
  }

  def main(args: Array[String]) {
    val deer = io.Source.fromFile(args(0)).getLines.map(parse).toList
    val time = 2503
    println(part1(deer, time))
    println(part2(deer, time))
  }

  def part1(deer: List[Reindeer], time: Int): Int = {
    val winningDistance = deer.map(d => distanceTravelled(d, time)).max
    winningDistance
  }

  def part2(deer: List[Reindeer], time: Int): Int = {
    val zeroScorecard = deer.map(_ -> 0).toMap
    val endScorecard = (zeroScorecard /: (1 to time).toList) {
      case (oldCard, newTime) =>
        val dists = deer.map(d => d -> distanceTravelled(d, newTime)).toMap
        val furthestDist = dists.values.max
        val leaders = deer filter (d => dists(d) == furthestDist)
        val newScores = leaders.map(d => d -> (oldCard(d) + 1)).toMap
        oldCard ++ newScores
    }
    endScorecard.values.max
  }

  def distanceTravelled(deer: Reindeer, time: Int): Int = {
    val fullCycles = time / deer.cycleTime
    val fullCycleDistance = fullCycles * deer.cycleDistance
    val leftOverTime = time - (fullCycles * deer.cycleTime)
    val remainingFlyTime = leftOverTime min deer.flyTime
    val leftOverDistance = remainingFlyTime * deer.speed
    fullCycleDistance + leftOverDistance
  }

  def parse(line: String): Reindeer = line split (" ") match {
    case Array(name, _, _, speed, _, _, flyTime, _, _, _, _, _, _, restTime, _) =>
      Reindeer(name, speed.toInt, flyTime.toInt, restTime.toInt)
  }

}
