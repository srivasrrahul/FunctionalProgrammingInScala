import scala.collection.mutable

case class Index(val j : Int,val state : Char)
object Solution {
  def countVowelPermutation(n: Int): Int = {
    def getNext(current : Char) : List[Char] = {
      current match {
        case 'a' => List('e')
        case 'e' => List('a','i')
        case 'i' => List('a','e','o','u')
        case 'o' => List('i','u')
        case 'u' => List('a')
        case _ => List('a','e','i','o','u')
      }
    }

    val cache = new mutable.HashMap[Index,Option[Long]]()
    def itr(j : Int,state : Char) : Option[Long] = {
      if (j == n) {
        Some(1)
      }else {
        val index = new Index(j,state)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var count : Long = 0
          for (next <- getNext(state)) {
            val res = itr(j + 1, next)
            if (res.isDefined) {
              count = (count+res.get) % (Math.pow(10,9)+7).toInt
            }
          }

          if (count == 0) {
            cache += ((index,None))
            None
          }else {
            cache += ((index,Some(count % (Math.pow(10,9)+7).toInt)))
            Some(count)
          }
        }
      }
    }

    var count = itr(0,'_')
    if (count.isDefined) {
      count.get.toInt
    }else {
      0
    }
  }
}