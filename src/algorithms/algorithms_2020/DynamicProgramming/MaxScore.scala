import scala.collection.mutable

case class Index(val left : Int,val right : Int)
object Solution {
  def maxScore(cardPoints: Array[Int], k: Int): Int = {
    if (k == cardPoints.length) {
      cardPoints.sum
    }else {
      val cache = new mutable.HashMap[Index,Int]()
      def itr(leftIndex : Int,rightIndex : Int,pending :Int) : Int ={
        if (pending == 0) {
          0
        }else {
          val index = new Index(leftIndex,rightIndex)
          if (cache.contains(index)) {
            cache.get(index).get
          }else {
            val left = cardPoints(leftIndex) + itr(leftIndex + 1, rightIndex, pending - 1)
            val right = cardPoints(rightIndex) + itr(leftIndex, rightIndex - 1, pending - 1)
            cache += ((index,math.max(left, right)))
            math.max(left, right)
          }
        }
      }

      itr(0,cardPoints.length-1,k)
    }


  }
}