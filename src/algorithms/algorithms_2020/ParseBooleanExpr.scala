
import scala.collection.mutable.ListBuffer
import util.control.Breaks._

trait BooleanExpr
case object True extends BooleanExpr
case object False extends BooleanExpr
case class NotExpr(val expr : BooleanExpr) extends BooleanExpr
case class AndExpr(val lst : List[BooleanExpr]) extends BooleanExpr
case class OrExpr(val lst : List[BooleanExpr]) extends BooleanExpr
case object InvalidExpr extends  BooleanExpr

trait ParserState
case class ParsedToken(string: String) extends  ParserState
case object FailedParsing extends ParserState


class Parser(val string: String) {
  val maxIndex = string.length-1
  var current = 0


  def updateCurrent(nextParsedValue : Parser) : Unit = {
    current = current + nextParsedValue.current
  }


  def consumeCurrent() : Unit = {
    current = current + 1
  }

  def consumeIfCurrent(char: Char) : Unit = {
    if (string.charAt(current) == char ) {
      current = current + 1
    }else {
      println("Parse error")
      current = current + 1
    }
  }





  def parseTill(delimiter : Char) : ParserState = {
    val parsedTokens = new StringBuilder
    while (current <= maxIndex && string.charAt(current) != delimiter) {
      parsedTokens.append(string.charAt(current))
      current = current + 1
    }

    new ParsedToken(parsedTokens.toString())
  }

  def isPending() : Boolean = {
    current <= maxIndex
  }

  def currentToken() : Char = {
    val currentChar = string.charAt(current)
    currentChar
  }


}
object Solution {
  def eval(booleanExpr: BooleanExpr) : Boolean = {
    booleanExpr match {
      case True => {
        true
      }
      case False => {
        false
      }
      case AndExpr(lst : List[BooleanExpr]) => {
        var result = eval(lst.head)
        lst.tail.foreach(booleanExpr => {
          val localResult = eval(booleanExpr)
          result = result &localResult

        })
        result
      }
      case OrExpr(lst : List[BooleanExpr]) => {
        var result = eval(lst.head)
        lst.tail.foreach(booleanExpr => {
          val localResult = eval(booleanExpr)
          result = result  | localResult

        })

        result
      }
      case NotExpr(expr : BooleanExpr) => {
        val evaluatedValue = eval(expr)
        !evaluatedValue

      }
    }
  }
  def parseBoolExpr(expression: String): Boolean = {

    def parse(parsedValue : String) : (BooleanExpr,Parser) = {
      //println("First char = " + parsedValue.charAt(0))
      val parser = new Parser(parsedValue)

      parser.currentToken() match {
        case 't' => {
          parser.consumeCurrent()
          (True,parser)
        }
        case 'f' => {
          parser.consumeCurrent()
          (False,parser)
        }
        case '!' => {
          parser.consumeCurrent()
          parser.consumeIfCurrent('(')

          val nextParsedString = parsedValue.substring(parser.current)

          val (nextParsedExpr,nextParser) = parse(nextParsedString)
          parser.updateCurrent(nextParser)

          //println("For not current = " + parser.currentToken())

          parser.consumeIfCurrent(')')

          (NotExpr(nextParsedExpr),parser)

        }
        case '&' => {
          parser.consumeCurrent()
          parser.consumeIfCurrent('(')
          val nextParsedString = parsedValue.substring(parser.current)

          val exprLst = new ListBuffer[BooleanExpr]

          val (nextParsedExpr,nextParser) = parse(nextParsedString)

          exprLst.addOne(nextParsedExpr)
          parser.updateCurrent(nextParser)

          while (parser.currentToken() == ',') {
            parser.consumeCurrent()
            val nextParsedString = parsedValue.substring(parser.current)
            val (nextParsedExpr,nextParser) = parse(nextParsedString)
            exprLst.addOne(nextParsedExpr)
            parser.updateCurrent(nextParser)
          }

          //println(parser.currentToken())


          parser.consumeIfCurrent(')')


          (new AndExpr(exprLst.toList),parser)
        }

        case '|' => {
          parser.consumeCurrent();
          parser.consumeIfCurrent('(')
          val nextParsedString = parsedValue.substring(parser.current)

          val exprLst = new ListBuffer[BooleanExpr]

          val (nextParsedExpr,nextParser) = parse(nextParsedString)

          exprLst.addOne(nextParsedExpr)
          parser.updateCurrent(nextParser)

          while (parser.currentToken() == ',') {
            parser.consumeCurrent();
            val nextParsedString = parsedValue.substring(parser.current)
            val (nextParsedExpr,nextParser) = parse(nextParsedString)
            exprLst.addOne(nextParsedExpr)
            parser.updateCurrent(nextParser)
          }

          parser.consumeIfCurrent(')')
          //parser.expect(')')

          (new OrExpr(exprLst.toList),parser)

        }
        case _ => {
          println("Error")
          (InvalidExpr,parser)
        }
      }
    }

    val res = parse(expression)
    val booleanExpr = res._1
    val parsedTill = res._2.current
    //println(expression.substring(parsedTill))
    //println(booleanExpr)
    eval(booleanExpr)
  }

  def main(args: Array[String]): Unit = {
    println(parseBoolExpr("!(&(&(!(&(f)),&(t),|(f,f,t)),&(t),&(t,t,f)))"))
    //println(parseBoolExpr("&(&(f))"))

    //println(parseBoolExpr("!(&(&(!(f),&(t),|(f,f,t)),&(t),&(t,t,f)))"))

    //println(parseBoolExpr("!(&(&(t,t,t),t,t)"))
  }

}