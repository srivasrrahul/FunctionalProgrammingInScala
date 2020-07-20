import scala.collection.mutable

object Solution {
  def checkValidString(s: String): Boolean = {
    val bracketsStack = new mutable.Stack[Int]()
    val starStack = new mutable.Stack[Int]()
    var validString = true
    for (j <- 0 to s.length-1 if validString == true) {
      val ch = s(j)
      ch match {
        case '(' => bracketsStack.push(j)
        case ')' => {
          if (bracketsStack.isEmpty == false) {
            bracketsStack.pop
          }else {
            if (starStack.isEmpty == false) {
              starStack.pop
            }else {
              validString = false
            }
          }
        }
        case '*' => starStack.push(j)
      }
    }

    // println(validString)
    // println(":" + bracketsStack+ ":" + bracketsStack.size)
    // println(starStack)
    if (validString == true) {
      if (bracketsStack.isEmpty) {
        true
      } else {
        if (bracketsStack.isEmpty == false && starStack.isEmpty == false) {
          while (bracketsStack.isEmpty == false && starStack.isEmpty == false && validString == true) {
            val topStarIndex = starStack.pop()
            val topBracketIndex = bracketsStack.pop()
            if (topStarIndex < topBracketIndex) {
              validString = false
            }
          }

          if (bracketsStack.isEmpty) {
            validString
          }else {
            false
          }

        }else {
          false
        }


      }
    }else {
      validString
    }
  }

  def main(args: Array[String]): Unit = {
    println(checkValidString("("))
  }
}