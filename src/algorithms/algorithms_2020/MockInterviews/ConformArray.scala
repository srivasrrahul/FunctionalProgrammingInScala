import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def canFormArray(arr: Array[Int], pieces: Array[Array[Int]]): Boolean = {
    val target = arr.toList

    val chunkMap = new mutable.HashMap[Int,Int]()
    for (j <- 0 to pieces.length-1) {
      for (p <- pieces(j)) {
        chunkMap += ((p,j))
      }
    }

    //println(chunkMap)
    var possible = true
    val newArr = new ListBuffer[Int]
    //    for (j <- 0 to arr.length-1 if possible == true) {
    var j = 0
    while (j < arr.length && possible == true) {
      val x = arr(j)
      //println(newArr.toList + " ; " + chunkMap + " ; " + x)

      if (chunkMap.contains(x)) {
        val piece = pieces(chunkMap.get(x).get)
        for (y <- piece) {
          newArr.append(y)
          chunkMap.remove(y)
          j = j + 1
        }
      }else {
        possible = false
      }
    }

    //println(newArr.toList)
    if (possible) {
      newArr.toList == target
    }else {
      false
    }
  }
}