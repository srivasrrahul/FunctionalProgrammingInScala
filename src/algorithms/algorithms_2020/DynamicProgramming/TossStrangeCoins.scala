import scala.collection.mutable
case class Index(val currentIndex : Int,val currentTarget : Int)
object Solution {
  def probabilityOfHeads(prob: Array[Double], target: Int): Double = {
    val cache = new mutable.HashMap[Index,Double]()
    def itr(currentIndex : Int,currentTarget : Int) : Double = {
      if (currentIndex < 0) {
        0.0
      }else {
        if (currentIndex+1 < currentTarget) {
          0.0
        } else {
          if (currentIndex == 0) {
            if (currentTarget == 1) {
              prob(currentIndex)
            }else {
              if (currentTarget == 0) {
                1 - prob(currentIndex)
              }else {
                0.0
              }
            }
          }else {
            val index = new Index(currentIndex,currentTarget)
            if (cache.contains(index)) {
              cache.get(index).get
            }else {
              if (currentTarget > 0) {
                val pr1 = prob(currentIndex) * itr(currentIndex - 1, currentTarget - 1)
                val pr2 = (1.0 - prob(currentIndex)) * itr(currentIndex - 1, currentTarget)
                cache += ((index,(pr1+pr2)))
                pr1 + pr2
              } else {
                val pr = (1.0 - prob(currentIndex)) * itr(currentIndex - 1, currentTarget)
                cache += ((index,pr))
                pr
              }
            }
          }
        }
      }
    }

    itr(prob.length-1,target)
  }
}