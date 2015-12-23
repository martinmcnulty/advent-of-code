package advent.day8

object Main {

  def main(args: Array[String]) {
    val lines = io.Source.fromFile(args(0)).getLines.toList
    val litChars = lines.map(_.length).sum
    val strChars = lines.map(countStringChars).sum
    val encoded = lines map encode
    val encChars = encoded.map(_.length).sum
    println(litChars - strChars)
    println(encChars - litChars)
  }

  def countStringChars(literal: String): Int = {
    @annotation.tailrec
    def _count(cs: List[Char], acc: Int): Int = cs match {
      case Nil                            => acc
      case '\\' :: '"' :: rest            => _count(rest, acc + 1)
      case '\\' :: '\\' :: rest           => _count(rest, acc + 1)
      case '\\' :: 'x' :: _ :: _ :: rest  => _count(rest, acc + 1)
      case _ :: rest                      => _count(rest, acc + 1)
    }
    _count(literal.toList.tail.dropRight(1), 0)
  }

  def encode(s: String): String = {
    def _enc(cs: List[Char], acc: List[Char]): String = cs match {
      case Nil => acc.reverse mkString ""
      case '\\' :: rest => _enc(rest, '\\' :: '\\' :: acc)
      case '"' :: rest  => _enc(rest, '\\' :: '"' :: acc)
      case c :: rest    => _enc(rest, c :: acc)
    }
    '"' + _enc(s.toList, Nil) + '"'
  }
}
