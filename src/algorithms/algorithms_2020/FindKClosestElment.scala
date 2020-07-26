import scala.collection.Searching
import scala.collection.mutable.ArrayBuffer

object Solution {
  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
    arr.search(x) match {
      case Searching.Found(xIndex) => {
        //Search k in both directions
        var left = xIndex-1
        var right = xIndex+1
        val result = new ArrayBuffer[Int]()
        result.append(x)
        while (left >=0 && right < arr.length && result.length < k) {
          val diffLeft = math.abs(arr(left)-arr(xIndex))
          val diffRight = math.abs(arr(right)-arr(xIndex))

          if (diffLeft <= diffRight) {
            result.append(arr(left))
            left = left-1
          }else {
            result.append(arr(right))
            right = right+1
          }
        }

        if (result.length < k) {
          while (left >= 0 && result.length < k) {
            result.append(arr(left))
            left = left-1
          }

          while (right < arr.length && result.length < k) {
            result.append(arr(right))
            right = right+1
          }
        }

        result.sortInPlace().toList
      }
      case Searching.InsertionPoint(insertionPoint) => {
        //println(arr(insertionPoint))
        var left = insertionPoint-1
        var right = insertionPoint
        val result = new ArrayBuffer[Int]()
        while (left >=0 && right < arr.length && result.length < k) {
          val diffLeft = math.abs(arr(left)-x)
          val diffRight = math.abs(arr(right)-x)

          if (diffLeft <= diffRight) {
            result.append(arr(left))
            left = left-1
          }else {
            result.append(arr(right))
            right = right+1
          }
        }

        if (result.length < k) {
          while (left >= 0 && result.length < k) {
            result.append(arr(left))
            left = left-1
          }

          while (right < arr.length && result.length < k) {
            result.append(arr(right))
            right = right+1
          }
        }

        result.sortInPlace().toList
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(findClosestElements(Array(0,0,1,2,3,3,4,7,7,8),3,5))
  }
}