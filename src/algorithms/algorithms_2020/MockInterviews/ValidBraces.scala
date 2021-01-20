import scala.collection.mutable

object Solution {
  def isValid(str: String): Boolean = {
    val s = new mutable.Stack[Char]()

    var valid = true

    for (c <- str if valid == true) {
      //println(c)
      c match {
        case '(' | '[' | '{' => {
          s.push(c)
        }
        case ')' => {
          if (s.isEmpty) {
            valid = false
          }else {
            if (s.pop != '(') {
              valid = false
            }
          }
        }
        case ']' => {
          if (s.isEmpty) {
            valid = false
          }else {
            if (s.pop != '[') {
              valid = false
            }
          }
        }
        case _ => {
          if (s.isEmpty) {
            valid = false
          }else {
            if (s.pop != '{') {
              valid = false
            }
          }
        }

      }
    }

    if (s.isEmpty == false) {
      valid = false
    }

    valid

  }
}