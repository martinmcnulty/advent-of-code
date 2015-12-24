package advent.day11

object Main {

  def main(args: Array[String]) {
    def test(pw: String) = println(f"$pw%8s: hasRun? ${hasRun(pw)}%5s, hasForbidden? ${hasForbidden(pw)}%5s, hasTwoDoubles? ${hasTwoDoubles(pw)}%5s, isValid? ${isValid(pw)}%5s")
    test("hijklmmn")
    val pw = io.Source.fromFile(args(0)).getLines.next
    val n1 = next(pw)
    val n2 = next(n1)
    println(n1)
    println(n2)
  }

  def next(pw: String) = {
    var n = pw
    do {
      n = incr(n)
    } while (! isValid(n))
    n
  }

  val runs: Set[String] = 'a'.to('x').map{ c => List(c, (c+1).toChar, (c+2).toChar) mkString "" }.toSet
  println(runs)

  def hasRun(pw: String) = {
    runs exists (pw containsSlice _)
  }

  def hasForbidden(pw: String) = {
    pw.contains('i') || pw.contains('o') || pw.contains('l')
  }

  def hasTwoDoubles(pw: String) = {
    def tailAfterNextDouble(cs: List[Char]): Option[List[Char]] = cs match {
      case Nil                      => None
      case a :: b :: tail if a == b => Some(tail)
      case _ :: tail                => tailAfterNextDouble(tail)
    }
    tailAfterNextDouble(pw.toList).map(tailAfterNextDouble(_).isDefined).getOrElse(false)
  }

  def isValid(pw: String) = hasRun(pw) && ! hasForbidden(pw) && hasTwoDoubles(pw)

  def incr(pw: String): String = {
    def _inc(cs: List[Char]): List[Char] = cs match {
      case 'z' :: rest => 'a' :: _inc(rest)
      case o :: rest   => (o+1).toChar :: rest
    }
    _inc(pw.toList.reverse).reverse.mkString("")
  }

}
