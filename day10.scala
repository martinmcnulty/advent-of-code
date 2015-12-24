package advent.day10

object Main {

  def main(args: Array[String]) {
    var phrase = io.Source.fromFile(args(0)).getLines.next
    1 to 40 foreach { _ => phrase = lookAndSay(phrase) }
    println(phrase.length)
    41 to 50 foreach { _ => phrase = lookAndSay(phrase) }
    println(phrase.length)
  }

  def lookAndSay(in: String): String = {
    @annotation.tailrec
    def _say(cs: List[Char], acc: List[Char]): List[Char] = cs match {
      case Nil => acc.reverse
      case a :: _ =>
        val (run, rest) = cs.span(_ == a)
        _say(rest, a :: run.length.toString.toList ++ acc)
    }
    _say(in.toList, Nil) mkString ""
  }

}
