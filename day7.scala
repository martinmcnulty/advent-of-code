package advent.day7

object Main {

  sealed trait ConstOrWire
  case class Const(v: Int) extends ConstOrWire
  case class Wire(w: String) extends ConstOrWire

  sealed trait WireValue
  case class Atom(cw: ConstOrWire) extends WireValue
  case class Not(w: ConstOrWire) extends WireValue
  case class LeftShift(w: ConstOrWire, s: Int) extends WireValue
  case class RightShift(w: ConstOrWire, s: Int) extends WireValue
  case class And(w1: ConstOrWire, w2: ConstOrWire) extends WireValue
  case class Or(w1: ConstOrWire, w2: ConstOrWire) extends WireValue

  def main(args: Array[String]) {

    def trim(i: Int) = 0xffff & i

    def evalW(w: String, cnxns: Map[String, WireValue], memo: collection.mutable.Map[String, Int]): Int = {
      memo.get(w).getOrElse{
        val result = cnxns(w) match {
          case Atom(cw)         => eval(cw, cnxns, memo)
          case Not(w)           => trim(~eval(w, cnxns, memo))
          case LeftShift(w, s)  => trim(eval(w, cnxns, memo) << s)
          case RightShift(w, s) => trim(eval(w, cnxns, memo) >>> s)
          case And(w1, w2)      => trim(eval(w1, cnxns, memo) & eval(w2, cnxns, memo))
          case Or(w1, w2)       => trim(eval(w1, cnxns, memo) | eval(w2, cnxns, memo))
        }
        memo(w) = result
        result
      }
    }
    def eval(cw: ConstOrWire, cnxns: Map[String, WireValue], memo: collection.mutable.Map[String, Int]): Int = {
      cw match {
        case Const(v) => v
        case Wire(w)  => evalW(w, cnxns, memo)
      }
    }

    val connections = parse(io.Source.fromFile(new java.io.File(args(0))).getLines.toList)
    val memo = collection.mutable.Map.empty[String, Int]
    val a = evalW("a", connections, memo)
    println(a)
    val connections2 = connections + ("b" -> Atom(Const(a)))
    memo.clear()
    println(evalW("a", connections2, memo))
  }

  def parse(lines: List[String]): Map[String, WireValue] = {
    def cw(s: String) = if (s(0).isDigit) Const(s.toInt) else Wire(s)
    lines.map{
      _ split " -> " match {
        case Array(lhs, rhs) =>
          val wireVal = lhs split " " match {
            case Array(v)              => Atom(cw(v))
            case Array("NOT", w)       => Not(cw(w))
            case Array(w, "LSHIFT", s) => LeftShift(cw(w), s.toInt)
            case Array(w, "RSHIFT", s) => RightShift(cw(w), s.toInt)
            case Array(w1, "AND", w2)  => And(cw(w1), cw(w2))
            case Array(w1, "OR", w2)   => Or(cw(w1), cw(w2))
          }
          rhs -> wireVal
      }
    }(collection.breakOut)
  }

}
