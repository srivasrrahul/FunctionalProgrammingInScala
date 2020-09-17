import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def numTilePossibilities(tiles: String): Int = {
    def seq(index : Int) : Set[String] = {
      if (index < 0) {
        Set()
      }else {
        //two options
        val lst = seq(index-1)
        val retValue = new mutable.HashSet[String]
        for (p <- lst) {
          //Add at all place
          for (j <- 0 to p.length) {
            val (x,y) = p.splitAt(j)
            val combined = x ++ tiles(index).toString ++ y
            retValue.add(combined)
          }
        }

        retValue.addAll(lst) //don't add youself
        retValue.add(tiles(index).toString) //only add you
        retValue.toSet
      }
    }

    val lst = seq(tiles.length-1)
    //println(lst)
    lst.size
  }
}