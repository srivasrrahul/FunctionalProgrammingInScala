import scala.collection.mutable

import util.control.Breaks._

class RandomizedSet() {

  /** Initialize your data structure here. */
  val arrBuffer = new scala.collection.mutable.ArrayBuffer[Int]()
  val arrIndex = new mutable.HashMap[Int,Int]()

  /** Inserts a value to the set. Returns true if the set did not already contain the specified element. */
  def insert(v: Int): Boolean = {
    arrIndex.get(v) match {
      case None => {
        val indexToBeInserted = arrBuffer.length
        arrBuffer.append(v)
        arrIndex += ((v,indexToBeInserted))
        true

      }
      case Some(_) => {
        false
      }
    }
  }

  /** Removes a value from the set. Returns true if the set contained the specified element. */
  def remove(v: Int): Boolean = {
    arrIndex.get(v) match {
      case None => {
        false

      }
      case Some(index) => {
        arrBuffer(index) = arrBuffer.last
        arrIndex += ((arrBuffer(index),index))
        arrIndex.remove(v)
        arrBuffer.dropRightInPlace(1)
        true
      }
    }
  }

  /** Get a random element from the set. */
  def getRandom(): Int = {
    val randomIndex = scala.util.Random.between(0,arrBuffer.length)
    arrBuffer(randomIndex)
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val r = new RandomizedSet
    println(r.remove(0))
    println(r.remove(0))
    println(r.insert(0))
    println(r.remove(0))
    println(r.insert(0))

//    r.remove(5)
//    for (j <- 0 to 100) {
//      println(r.getRandom())
//    }


  }
}