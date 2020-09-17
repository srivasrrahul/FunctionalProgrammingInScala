import scala.collection.mutable

case class Index(val b : Int,val e : Int)
object Solution {
  def minInsertions(s: String): Int = {

    val cache = new mutable.HashMap[Index,Int]
    def itr(b : Int,e : Int) : Int = {
      if (b == e-1) {
        if (s(b) == s(e)) {
          0
        }else {
          1 //a
        }
      }else {
        if (b == e) {
          0
        }else {
          val index = new Index(b,e)
          if (cache.contains(index)) {
            //println("Cache hit")
            cache.get(index).get
          }else {
            var cost = 0
            if (s(b) == s(e)) {
              cost = itr(b + 1, e - 1)
            } else {
              //Add s(b) to right
              cost = math.min(1 + itr(b + 1, e),1+itr(b,e-1))
            }

            cache += ((index,cost))
            cost
          }
        }
      }
    }

    itr(0,s.length-1)
  }

  def main(args: Array[String]): Unit = {
    println(minInsertions("ausdagdasfdhasd"))
  }
}