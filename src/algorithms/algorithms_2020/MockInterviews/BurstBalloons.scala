import scala.collection.mutable

case class Index(val lst : (Int,Int),val lValue : Int,val rValue : Int)
object Solution {
  def maxCoins(nums: Array[Int]): Int = {
    val cache = new mutable.HashMap[Index,Int]()
    def itr(lst : (Int,Int),lValue : Int,rValue : Int) : Int = {
      //println(lst + " " + lValue + " " + rValue)
      if (lst._2==lst._1) {
        nums(lst._1) * lValue * rValue
      }else {
        val index = new Index(lst,lValue,rValue)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var maxCoins = 0
          for (j <- lst._1 to lst._2-1) {
            //val (left, right) = lst.splitAt(j)
            val (left,right) = ((lst._1,j),(j+1,lst._2))
            //Focus on Left
            val leftCoin = itr(left, lValue, nums(right._1))
            //Left is burnt completely
            val rightCoin = itr(right, lValue, rValue)

            val c1 = leftCoin + rightCoin

            //reverse the order
            //left is burst later
            val rightCoin1 = itr(right, nums(left._2), rValue)
            //rigjht  is done
            val leftCoin1 = itr(left, lValue, rValue)
            val c2 = leftCoin1 + rightCoin1

            val maxValue = math.max(c1, c2)
            if (maxValue > maxCoins) {
              maxCoins = maxValue
            }
          }

          cache += ((index,maxCoins))
          maxCoins
        }
      }
    }



    itr((0,nums.length-1),1,1)
  }

  def main(args: Array[String]): Unit = {
    println(maxCoins(Array(3,1,5,8)))
  }
}