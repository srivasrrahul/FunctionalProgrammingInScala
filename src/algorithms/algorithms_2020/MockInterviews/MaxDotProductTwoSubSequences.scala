import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Index(val j : Int,val k : Int,val s : Boolean)
object Solution {
  def maxDotProduct(nums1: Array[Int], nums2: Array[Int]): Int = {
    val cache = new mutable.HashMap[Index,Int]()
    def itr(j : Int,k : Int,s : Boolean) : Int = {
      if (j == nums1.length-1 && k == nums2.length-1) {
        if (s == false) {
          nums1(j) * nums2(k)
        }else {
          math.max(nums1(j) * nums2(k),0)
        }
      }else {
        val index = new Index(j,k,s)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          if (j == nums1.length-1) {
            var retValue = 0
            if (s == false) {
              val option1 = nums1(j) * nums2(k)
              val option2 = itr(j,k+1,s)
              retValue = math.max(option1,option2)
            }else {
              val option1 = nums1(j) * nums2(k)
              val option2 = itr(j,k+1,s)
              retValue = math.max(math.max(option1,option2),0)
            }

            cache += ((index, retValue))
            retValue
          }else {
            if (k == nums2.length-1) {
              var retValue = 0
              if (s == false) {
                val option1 = nums1(j) * nums2(k)
                val option2 = itr(j + 1, k, s)
                retValue = math.max(option1,option2)
              }else {
                val option1 = nums1(j) * nums2(k)
                val option2 = itr(j + 1, k, s)
                retValue = math.max(math.max(option1,option2),0)
              }

              cache += ((index, retValue))
              retValue
            }else {
              val option1 = nums1(j) * nums2(k) + itr(j + 1, k + 1,true)
              val option2 = itr(j, k + 1,s)
              val option3 = itr(j + 1, k,s)
              val option4 = itr(j + 1, k + 1,s)
              val retValue = Array(option1, option2, option3, option4).max
              cache += ((index, retValue))

              retValue
            }
          }

        }
      }
    }

    itr(0,0,false)
  }
}