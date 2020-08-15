import scala.collection.mutable.ListBuffer

class MyHashSet() {

  /** Initialize your data structure here. */

  val arr = new Array[ListBuffer[Int]](1001)
  def getHash(key : Int) : Int = {
    key % 1001
  }
  def add(key: Int) : Unit = {
    val hashedKey = getHash(key)
    if (arr(hashedKey) == null) {
      arr(hashedKey) = new ListBuffer[Int]
      arr(hashedKey).addOne(key)
    }else {
      if (arr(hashedKey).contains(key) == false) {
        arr(hashedKey).addOne(key)
      }
    }
  }

  def remove(key: Int) : Unit = {
    val hashedKey = getHash(key)
    if (contains(key)) {
      if (arr(hashedKey).length == 1) {
        arr(hashedKey).dropRightInPlace(1)
      }else {
        arr(hashedKey).filterInPlace(x => x != key)
      }
    }
  }

  /** Returns true if this set contains the specified element */
  def contains(key: Int): Boolean = {
    val hashedKey = getHash(key)
    if (arr(hashedKey) != null) {
      arr(hashedKey).contains(key)
    }else {
      false
    }
  }

}


/**
 * Your MyHashSet object will be instantiated and called as such:
 * var obj = new MyHashSet()
 * obj.add(key)
 * obj.remove(key)
 * var param_3 = obj.contains(key)
 */