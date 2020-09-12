import scala.collection.Searching
import scala.collection.mutable.ArrayBuffer

object Solution {
  def reversePairs(_nums: Array[Int]): Int = {
    //val test = _nums.clone()
    if (_nums.isEmpty) {
      0
    }else {
      val nums = new Array[Long](_nums.length)
      for (j <- 0 to _nums.length-1) {
        nums(j) = _nums(j)
      }

      def findRange(base : Long,b : Int,e : Int) : (Int,Int) = {
        if (b == e) {
          if(base == nums(b)) {
            (b,e)
          }else {
            (-1,-1)
          }
        }else {
          if (b > e) {
            (-1,-1)
          }else {
            val mid = b + (e-b)/2
            if (nums(mid) > base) {
              findRange(base,b,mid)
            }else {
              if (nums(mid) < base) {
                findRange(base,mid+1,e)
              }else {
                //Its equal
                val (ll,lr) = findRange(base,b,mid) //left always has it
                val (rl,rr) = findRange(base,mid+1,e)

                var leftMost = lr
                if (ll != -1) {
                  leftMost = ll
                }

                var rightMost = lr
                if (rl != -1) {
                  rightMost = rl
                }

                if (rr != -1) {
                  rightMost = rr
                }

                (leftMost,rightMost)
              }
            }
          }
        }
      }
      def findGreaterThan(base : Long,b : Int,e : Int) : Int = {
        nums.search(base,b,e+1) match {
          case Searching.Found(foundIndex) => {
            //println("Here")
            val (l,r) = findRange(base,b,e)
            (e-l+1)
          }
          case Searching.InsertionPoint(ip) => {
            // if (ip > e) {
            //     0
            // }else {
            //     if (ip == b) {
            //        (e-b+1)
            //     }else {
            //         (e-ip+1)
            //     }
            // }
            //println("ip " + ip + " " + base + " " + b + " " + e)
            e-ip+1
          }
        }
      }
      var count = 0
      def merge(b : Int,e : Int) : Unit = {
        //println(b + " " + e + " " + count)
        if (b == e) {
          //already sorted
        }else {
          val mid = b + (e-b)/2
          merge(b,mid)
          merge(mid+1,e)

          //left and right is sorted now

          for (j <- mid+1 to e) {
            val base = 2*nums(j)+1
            val greaterThan = findGreaterThan(base,b,mid)
            count = count + greaterThan
          }

          //now merge
          val arrBuffer = new ArrayBuffer[Long]()

          var j = b
          var k = mid+1

          var right=mid+1
          while (j <= mid && k <= e) {
            if (nums(j) < nums(k)) {
              //                   if (nums(k) < 0) {
              //                     val greaterThan =  findGreaterThan(nums(k)*2+1,j,mid)
              //                       if (greaterThan > 0) {
              //                           //println("GT " + nums(j) + " " + nums(k))
              //                       }
              //                     count = count + greaterThan
              //                     right = k

              //                   }
              arrBuffer.append(nums(j))
              j = j + 1
            }else {
              // if (nums(j) >2*nums(k)) {
              //     count = count + (mid-j+1)
              // }
              // if (right == k && nums(k) < 0) {
              //     //Already handled
              // }else {
              //     val greaterThan =  findGreaterThan(nums(k)*2+1,j,mid)
              //     count = count + greaterThan
              // }



              arrBuffer.append(nums(k))
              k = k + 1
              right = k
            }
          }

          if (j <= mid) {
            while (j <= mid) {
              // if (nums(j) > (2*nums(e))) {
              //     count = count + (e-(mid+1)+1)
              // }
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
          if (sorted.length != (e-b+1)) {
            println("Error")
          }
          //println("Sorted " + b + " " + e + " " + sorted.mkString(","))
          Array.copy(sorted,0,nums,b,sorted.length)
          //println("For " + b  + " " + e  + " " + arrBuffer.mkString(",") + " " + count)
        }
      }

      if (nums.isEmpty) {
        0
      }else {
        merge(0, nums.length - 1)
        //println(nums.mkString(","))
        //println(test.sortInPlace().mkString(","))
        count
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(reversePairs(Array(2,4,3,5,1)))
  }
}