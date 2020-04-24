import scala.collection.mutable.ListBuffer

// This is the interface that allows for creating nested lists.
  // You should not implement it, or speculate about its implementation
  abstract class NestedInteger {

    // Return true if this NestedInteger holds a single integer, rather than a nested list.
    def isInteger: Boolean

    // Return the single integer that this NestedInteger holds, if it holds a single integer
    def getInteger: Int

    // Set this NestedInteger to hold a single integer.
    def setInteger(i: Int) : Unit

    // Return the nested list that this NestedInteger holds, if it holds a nested list
    def getList : List[NestedInteger]

    // Set this NestedInteger to hold a nested list and adds a nested integer to it.
   def add(ni: NestedInteger) : Unit
  }

object Solution {
  def depthSumInverse(nestedList: List[NestedInteger]): Int = {
     def maxDepth(nestedInteger: NestedInteger) : Int = {
       if (nestedInteger.isInteger) {
         1
       }else {
         val subLsts = nestedInteger.getList
         var maxDepthVal = Int.MinValue
         for (subLst <- subLsts) {
           val depth = maxDepth(subLst)
           if (depth > maxDepthVal) {
             maxDepthVal = depth
           }
         }

         1 + maxDepthVal
       }
     }

    var maxDepthVal = 1


    for (nestedInt <- nestedList) {
      val depth = maxDepth(nestedInt)
      if (depth > maxDepthVal) {
        maxDepthVal = depth
      }

    }


    def calculateValue(nestedInteger: NestedInteger,depth : Int) : Int = {
      if (nestedInteger.isInteger == true) {
        nestedInteger.getInteger * depth
      }else {
        val subLsts = nestedInteger.getList
        var sumLst = 0
        for (subLst <- subLsts) {
          sumLst = sumLst + calculateValue(subLst,depth-1)
        }

        sumLst
      }
    }

    var sum = 0

    for (nestedInt <- nestedList ) {
      sum = sum  + calculateValue(nestedInt,maxDepthVal)
    }

    sum

  }


}