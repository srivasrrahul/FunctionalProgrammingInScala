import scala.util.parsing.combinator._
import scala.language.postfixOps

//Abstract Syntax Tree
case class AccountSpec(acc : String)
trait PriceType
case object Min extends PriceType
case object Max extends PriceType
trait PriceSpecType
case class PriceSpecMinMax(priceType : PriceType,price : Int) extends PriceSpecType
case class PriceSpecFixed(price : Int) extends PriceSpecType

//Parser and DSL
class OrderDsl extends JavaTokenParsers {


  def string_literal : Parser[String] = """[a-z]+""".r ^^ {_.toString}
  def numeric_literal : Parser[Int] = """[0-9]+""".r ^^ {_.toInt}

  def parse_account_spec : Parser[String] = "for" ~> "account" ~> string_literal
  def min_max : Parser[PriceType] = "min" ^^^ Min | "max" ^^^ Max
  def price_spec_min_max : Parser[PriceSpecMinMax] =  min_max ~ numeric_literal ^^ {case m ~ d => PriceSpecMinMax(m,d.toInt)}
  def price_spec_at_fixed : Parser[PriceSpecFixed] = numeric_literal ^^ {case x => PriceSpecFixed(x)}
  def price_spec : Parser[PriceSpecType] = "at" ~> (price_spec_at_fixed | price_spec_min_max)

}



object Exec extends OrderDsl {
  def main(args: Array[String]): Unit = {
    val s = """at min 10"""

    println(parse(price_spec,s))
  }
}
