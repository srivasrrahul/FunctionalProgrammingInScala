import scala.collection.Searching
import scala.collection.mutable.ArrayBuffer

object Solution {
  def reversePairs(nums: Array[Int]): Int = {
    var count = 0
    def merge(b : Int,e : Int) : Unit = {
      if (b == e) {
        //already sorted
      }else {
        val mid = b + (e-b)/2
        merge(b,mid)
        merge(mid+1,e)

        //left and right is sorted now

        for (j <- mid+1 to e) {
          val rValue = nums(j)
          val lValue = rValue*2+1

          nums.search(lValue,b,mid+1) match {
            case Searching.Found(foundIndex) => {
              //All elements from  founIndex to mid
              //println("For rValue " + rValue + " " + b + " " + e)
              count = count + (mid-foundIndex+1)
            }
            case Searching.InsertionPoint(insertionPoint) => {

              if (insertionPoint <= mid) {
                //println(" For rValue " + rValue + " " + lValue + " " + b + " " + e + " " + mid + " " + insertionPoint)
                count = count + (mid-insertionPoint+1)
              }
            }
          }
        }

        //now merge
        val arrBuffer = new ArrayBuffer[Int]()

        var j = b
        var k = mid+1

        while (j <= mid && k <= e) {
          if (nums(j) <= nums(k)) {
            arrBuffer.append(nums(j))
            j = j + 1
          }else {
            arrBuffer.append(nums(k))
            k = k + 1
          }
        }

        if (j <= mid) {
          while (j <= mid) {
            arrBuffer.append(nums(j))
            j = j + 1
          }
        }

        if (k <= e) {
          while (k <= e) {
            arrBuffer.append(nums(k))
            k = k + 1
          }
        }


        val sorted = arrBuffer.toArray
        //println("Sorted " + b + " " + e + " " + sorted.mkString(","))
        Array.copy(sorted,0,nums,b,sorted.length)

      }
    }

    if (nums.isEmpty) {
      0
    }else {
      merge(0, nums.length - 1)
      //println(nums.mkString(","))
      count
    }
  }

  def main(args: Array[String]): Unit = {
    println(reversePairs(Array(2,4,3,5,1)))
  }
}