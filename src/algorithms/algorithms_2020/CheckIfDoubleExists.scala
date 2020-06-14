import scala.collection.{Searching, mutable}

object Solution {
  def checkIfExist(arr: Array[Int]): Boolean = {
    arr.sortInPlace()
    var found = false
    for (j <- 0 to arr.length-1 if found == false) {
      val y = arr(j)*2
      arr.toSeq.search(y,0,j) match {
        case Searching.Found(foundIndex) => {
          found = true
        }
        case _ => {

        }
      }

      if (found == false) {
        arr.toSeq.search(y,j+1,arr.length) match {
          case Searching.Found(foundIndex) => {
            found = true
          }
          case _ => {

          }
        }

      }

    }

    found


  }
}