import scala.collection.mutable

class PhoneDirectory(_maxNumbers: Int) {

  /** Initialize your data structure here
        @param maxNumbers - The maximum numbers that can be stored in the phone directory. */

  val set = new mutable.HashSet[Int]()
  for (j <- 0 to _maxNumbers-1) {
    set.add(j)
  }

  /** Provide a number which is not assigned to anyone.
        @return - Return an available number. Return -1 if none is available. */
  def get(): Int = {
    if (set.size == 0) {
      -1
    }else {
      val h = set.head
      set.remove(h)
    }
  }

  /** Check if a number is available or not. */
  def check(number: Int): Boolean = {
    set.contains(number)
  }

  /** Recycle or release a number. */
  def release(number: Int) {
    set.add(number)
  }

}
