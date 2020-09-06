import scala.collection.mutable

case class Index(val x : Int,val y : Int,val z : Int)
object Solution {
  def isInterleave(s1: String, s2: String, s3: String): Boolean = {
    val cache = new mutable.HashMap[Index,Boolean]()
    def itr(x : Int,y : Int,z : Int) : Boolean = {
      if (z >= s3.length) {
        if (x >= s1.length && y >= s2.length) {
          true
        }else {
          false
        }
      }else {
        val index = new Index(x,y,z)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val options = new mutable.HashSet[Boolean]()
          if (x < s1.length && s1(x) == s3(z)) {
            options.add(itr(x + 1, y, z + 1))
          }

          if (y < s2.length && s2(y) == s3(z)) {
            options.add(itr(x, y + 1, z + 1))
          }

          cache += ((index,options.contains(true)))
          options.contains(true)
        }
      }
    }

    itr(0,0,0)
  }
}