
// This is the interface that allows for creating nested lists.
// You should not implement it, or speculate about its implementation
abstract class NestedInteger {

  // Return true if this NestedInteger holds a single integer, rather than a nested list.
  def isInteger: Boolean

  // Return the single integer that this NestedInteger holds, if it holds a single integer
  def getInteger: Int

  // Set this NestedInteger to hold a single integer.
  def setInteger(i: Int)    : Unit

  // Return the nested list that this NestedInteger holds, if it holds a nested list
  def getList : List[NestedInteger]

  // Set this NestedInteger to hold a nested list and adds a nested integer to it.
  def add(ni: NestedInteger) : Unit
}

object Solution {
  def depthSum(nestedList: List[NestedInteger]): Int = {
    def sum(current : List[NestedInteger],depth : Int) : Int = {
      current match {
        case (x::xs) => {
           if (x.isInteger) {
             x.getInteger*depth + sum(xs,depth)
           }else {
             sum(x.getList,depth+1) + sum(xs,depth)
           }

        }
        case nil => {
          0
        }
      }
    }

    sum(nestedList,1)

  }
}