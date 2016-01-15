package advent.day15

object Main {

  case class Ingredient(name: String, properties: Map[String, Int])

  case class Recipe(is: Map[Ingredient, Int])

  def main(args: Array[String]) {
    val ingredients = io.Source.fromFile(args(0)).getLines map parse
    var max = 0
    val maxTeaspoons = 10
    val props = ingredients flatMap (_.properties.keySet)
    val blankRecipe = Recipe(Map.empty)
  }

  def parse(line: String): Ingredient = {
    line split ": " match {
      case Array(name, props) =>
        val pairs = props split ", "
        val m = (pairs map { _ split " " match {
          case Array(prop, value) => prop -> value.toInt
        }}).toMap
        Ingredient(name, m)
    }
  }

}
