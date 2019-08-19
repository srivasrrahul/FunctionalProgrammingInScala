import scala.util.parsing.combinator._

case class Price(p : Float)
case class Country(c : String)
trait CoeffeeRoast
case class Regular(withFrench : Boolean = false) extends CoeffeeRoast
case class Italian(withFrench : Boolean = false) extends CoeffeeRoast



case class CoeffeeName(name : String,old_name : String = "")

case class Coeffee(name : CoeffeeName,roast : CoeffeeRoast, country : Country, price : Price)

class CoeffeeDSL extends JavaTokenParsers {

  //def float_literal : Parser[Float] = """[+-]?([0-9]*[.])?[0-9]+""".r ^^ {_.toFloat}
  def country_literal : Parser[String] = """[A-Z][a-z]+""".r
  def price : Parser[Price] = floatingPointNumber ^^ {case p => Price(p.toFloat)}
  def country : Parser[Country] = country_literal ^^ {case c => Country(c)}
  def french : Parser[Boolean] = "/" ~> "French" ^^ {case _ => true}
  def regular_roast : Parser[Regular] = "Regular" ~> opt(french) ^^ {
    case Some(_) => Regular(true)
    case None => Regular(false)
  }
  def italian_roast : Parser[Italian] = "Italian" ~> opt(french) ^^ {
    case Some(_) => Italian(true)
    case None => Italian(false)
  }

  def roast : Parser[CoeffeeRoast] = regular_roast | italian_roast

  def s_literal : Parser[String] = """[A-Z][a-z]+""".r

  def old_name : Parser[String]  = "(" ~> s_literal <~ ")" ^^ {case l => l}


  def name : Parser[CoeffeeName] = s_literal ~ opt(old_name) ^^ {
    case n ~ Some(o) => CoeffeeName(n,o)
    case m ~ None => CoeffeeName(m)
  }

  def coeffee : Parser[Coeffee] = name ~ "," ~ roast ~ "," ~ country ~ "," ~ price ^^ {case n ~ s1 ~ r ~ s2 ~ c ~ s3 ~ p => Coeffee(n,r,c,p)}

}

object CoeffeeDSLExec extends CoeffeeDSL {
  def main(args: Array[String]): Unit = {
    val s = "Caress (Smackin), Italian, Sumatra, 7.95"
    println(parse(coeffee,s))
  }
}