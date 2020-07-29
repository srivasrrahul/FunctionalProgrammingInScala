import scala.collection.mutable.ListBuffer

object Solution {
  def simplifyPath(path: String): String = {
    val strArray = path.split("/")
    //println(strArray.mkString(","))
    var level = 0
    val simplePath = new ListBuffer[String]
    for (str <- strArray) {
      str match {
        case "" => {

        }
        case ".." => {
          if (simplePath.isEmpty == false) {
            simplePath.dropRightInPlace(1)
          }


        }
        case "." => {
          //no op
        }
        case _ => {
          simplePath.append(str)
        }
      }
    }


    "/" ++ simplePath.mkString("/")
  }

  def main(args: Array[String]): Unit = {
    println(simplifyPath("/a//b////c/d//././/.."))
  }
}