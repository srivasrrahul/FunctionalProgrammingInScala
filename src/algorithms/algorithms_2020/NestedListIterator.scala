
// This is the interface that allows for creating nested lists.
// You should not implement it, or speculate about its implementation
abstract class NestedInteger {

  // Return true if this NestedInteger holds a single integer, rather than a nested list.
  def isInteger: Boolean

  // Return the single integer that this NestedInteger holds, if it holds a single integer
  def getInteger: Int

  // Set this NestedInteger to hold a single integer.
  def setInteger(i: Int): Unit

  // Return the nested list that this NestedInteger holds, if it holds a nested list
  def getList : List[NestedInteger]

  // Set this NestedInteger to hold a nested list and adds a nested integer to it.
  def add(ni: NestedInteger) : Unit
}


class NestedIterator(_nestedList: List[NestedInteger]) {
  def convertToList() : List[Int] = {
    def itr(nestedLst : List[NestedInteger]) : List[Int] = {
      nestedLst match {
        case x::xs => {
          if (x.isInteger) {
            (x.getInteger::(itr(xs)))
          }else {
            val lst = itr(x.getList.toList)
            lst ++ itr(xs)
          }
        }
        case nil => {
          List()
        }
      }
    }

    itr(_nestedList)
  }

  val lst = convertToList()
  val itr = lst.iterator
  def next(): Int = {
    itr.next()
  }

  def hasNext(): Boolean = {
    itr.hasNext
  }
}

/**
 * Your NestedIterator object will be instantiated and called as such:
 * var obj = new NestedIterator(nestedList)
 * var param_1 = obj.next()
 * var param_2 = obj.hasNext()
 */