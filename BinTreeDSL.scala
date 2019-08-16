//DSL definition
//Key words : Node and Nil
//Rest are digits
//Tree = Nil | Node Int Node Node
//Tree = Nil | (Node Int Tree Tree)
import scala.util.parsing.combinator._
//AST
trait BinTree
case object Nil extends BinTree
case class Node(value : Int,left : BinTree,right : BinTree) extends BinTree

class BinTreeDSL extends JavaTokenParsers {
  def numeric_literal : Parser[Int] = """[0-9]+""".r ^^ {_.toInt}
  def nil_literal : Parser[String] = """Nil""".r ^^ {_.toString}
  def parse_tree : Parser[BinTree] = nil | ("(" ~> "Node" ~> numeric_literal ~ parse_tree ~ parse_tree <~ ")") ^^ {
    case x ~ l ~ r => Node(x,l,r)
    case a => Nil
  }
  def nil : Parser[BinTree] = nil_literal ^^^ Nil
}

object ExecBinTreeDSL extends BinTreeDSL {
  def main(args: Array[String]): Unit = {
    val s = """(Node 10 (Node 20 Nil Nil) (Node 30 Nil Nil))"""

    println(parse(parse_tree,s))
  }
}