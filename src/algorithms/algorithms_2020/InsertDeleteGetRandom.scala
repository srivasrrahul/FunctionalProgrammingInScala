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
    //debug()
    if (presence.contains(v)) {
      val indexesSet = presence.get(v).get
      val index = indexesSet.head

      val lastValue = arrBuffer.last //last value
      arrBuffer(index) = arrBuffer.last //index got updated with last value

      if (v == arrBuffer.last) {
        //There are at least two elements
        indexesSet.remove(arrBuffer.length-1)
        if (indexesSet.size == 0) {
          presence.remove(v)
        }
      }else {
        if (indexesSet.size == 1) {
          presence.remove(v)
        }else {
          indexesSet.remove(index)
        }

        val lastSet = presence.get(arrBuffer.last).get
        lastSet.add(index)
        lastSet.remove(arrBuffer.length-1)
      }


      arrBuffer.dropRightInPlace(1)

      //debug()

      true

    }else {
      false
    }
  }

  /** Get a random element from the collection. */
  def getRandom(): Int = {
    //debug()
    val r = Random.between(0,arrBuffer.length)
    arrBuffer(r)
  }

}
