import scala.util.parsing.combinator._
//[1,2,3] or [[1,2],3]

trait Element
case class NestedList (elements : List[Element])
case class IntElement(x : Int) extends Element
case class ElementLst(nestedList: NestedList) extends Element



class NestedListDSL extends JavaTokenParsers {
  def lst : Parser[NestedList] = "[" ~> elements <~ "]" ^^ {case e => NestedList(e)}
  def elements : Parser[List[Element]] = repsep(element,",")  ^^ {case l => l}
  def element : Parser[Element] = int_literal  ^^ {case i => IntElement(i)} | lst ^^ {case l => ElementLst(l)}
  def int_literal : Parser[Int] = """[0-9]+""".r ^^ {_.toInt}
}

object NestedListDSLExec extends NestedListDSL {
  def main(args: Array[String]): Unit = {
    val s = """[1,[2,4],3]"""
    println(parse(lst,s))
  }
}