import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class RandomizedCollection() {

  /** Initialize your data structure here. */

  val arrBuffer = new ArrayBuffer[Int]()
  val presence = new mutable.HashMap[Int,mutable.HashSet[Int]]() //value and its index

  /** Inserts a value to the collection. Returns true if the collection did not already contain the specified element. */
  def insert(v : Int): Boolean = {
    arrBuffer.append(v)
    val defaultSet = presence.getOrElseUpdate(v,new mutable.HashSet[Int]())
    var retValue = true
    if (defaultSet.size > 0) {
      retValue = false
    }
    defaultSet.add(arrBuffer.length-1)
    //debug()
    retValue
  }

  def debug() : Unit = {
    println(arrBuffer.toList)
    println(presence)
  }
  /** Removes a value from the collection. Returns true if the collection contained the specified element. */
  def remove(v: Int): Boolean = {
    if (presence.contains(v)) {
      val indexesSet = presence.get(v).get
      val index = indexesSet.head

      val lastValue = arrBuffer.last //last value
      arrBuffer(index) = arrBuffer.last //index got updated with last value

      val defaultSet = presence.get(arrBuffer.last).get //last value's presence
      defaultSet.remove(arrBuffer.length-1) //update last values presence with correct index
      defaultSet.add(index)

      if (indexesSet.size == 1) {
        presence.remove(v)
      }

      presence += ((arrBuffer(index),defaultSet))

      arrBuffer.dropRightInPlace(1)

      //debug()

      true

    }else {
      false
    }
  }

  /** Get a random element from the collection. */
  def getRandom(): Int = {
    debug()
    val r = Random.between(0,arrBuffer.length)
    arrBuffer(r)
  }

}
