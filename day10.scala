package advent.day10

object Main {

  def main(args: Array[String]) {
    var phrase = io.Source.fromFile(args(0)).getLines.next
    1 to 40 foreach { _ => phrase = lookAndSay(phrase) }
    println(phrase)
  }

  def lookAndSay(in: String): String = {
    ???
  }

}
