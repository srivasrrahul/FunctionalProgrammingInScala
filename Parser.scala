import scala.util.parsing.combinator._


//class Arith extends JavaTokenParsers {
//  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
//  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
//  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
//}


case class Cond (cond : String) {
  override def toString: String = "Cond is " + cond
}


class BasicIf extends JavaTokenParsers {
//  def if_cond : Parser[Any] = "if"~cond
  //def if_expr : Parser[Any] = "if"
  def cond : Parser[String] = """[a-z]+""".r ^^ { _.toString }
  def parse_cond : Parser[Cond] = "if(" ~ cond ~ ")" ^^ {case if_expr ~ cd ~ if_end => Cond(cd)}
}
//sealed trait AddExpr
//case class AddBasic(x1 : Int , x2 : Int) extends AddExpr  {
//  override def toString: String = "basic : x1 = " + x1 + " " + "x2 = " + x2
//
//}
//case class AddWithExp (x1 : Int, e2 : AddExpr) extends AddExpr {
//  override def toString: String = "exp : x1 = " + x1 + " " + e2.toString
//}

//case class AddExpr (x1 : Int, x2 : Int) {
//  override def toString: String = "x1 = " + x1 + " " + "x2 = " + x2
//}
//case class AddExpr (x1 : Int, e2 : AddExpr) {
//  override def toString: String = "x1 = " + x1 + " " + e2.toString
//}


//class AddExprParser extends JavaTokenParsers {
//  def num : Parser[String] = """[0-9]+""".r
//  def space : Parser[String] = """\s*""".r
//  def add_expr_basic : Parser[AddExpr] = num ~  space  ~ "+" ~ space ~ num ^^ {case x1 ~ s1 ~ add_symbol  ~ s2 ~ x2 => AddBasic(x1.toInt,x2.toInt)}
//  def add_expr : Parser[AddExpr] =  (num ~ "+" ~ add_expr) | add_expr_basic ^^ {
//
//    case a1 ~ add_symbol ~ e2 => AddWithExp(a1.toInt,e2)
//    case x1  => AddBasic(0,1)
//  }
//}
object ParseExpr extends BasicIf {
  def main(args: Array[String]) = {
    //println("input is : " + args(0))
    //println(parse(parse_cond, args(0)))

    println(parse(parse_cond,"if(abc)"))
  }
}