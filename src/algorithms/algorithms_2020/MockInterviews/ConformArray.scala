import scala.collection.mutable.ListBuffer

object Solution {
  def canFormArray(arr: Array[Int], pieces: Array[Array[Int]]): Boolean = {
    val target = arr.toList
    def itr(j : Int,lst : List[List[Int]]) : Boolean = {
      //println(j)
      if (j >= pieces.length) {
        val lstBuffer = new ListBuffer[Int]
        for (l <- lst) {
          lstBuffer.appendAll(l)
        }

        lstBuffer.toList == target

      }else {
        var found = false
        for (k <- 0 to lst.length if found == false) {
          val (pre,post) = lst.splitAt(k)

          if (itr(j+1,pre ++ List(pieces(j).toList) ++ post)) {
            found = true
          }
        }

        found
      }
    }

    itr(0,List())
  }
}