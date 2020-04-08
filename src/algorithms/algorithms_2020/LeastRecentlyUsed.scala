import scala.collection.mutable


class LRUCache(_capacity: Int) {
  val capacity = _capacity
  val linkedHashMap = new mutable.LinkedHashMap[Int,Int]() {


  }
  def get(key: Int): Int = {
    linkedHashMap.get(key) match {
      case Some(value : Int) =>  {
        linkedHashMap.remove(key)
        linkedHashMap.addOne((key,value))
        value
      }

      case None => -1
    }
  }

  def put(key: Int, value: Int): Unit =  {
    //println("Key inserted " + key + " Size is " + linkedHashMap.size)
    if (linkedHashMap.size < capacity) {
      linkedHashMap.remove(key) //
      linkedHashMap += ((key,value))
    }else {
      //println("Adding key " + key)
      //println("removed key " + linkedHashMap.head._1)
      if (linkedHashMap.contains(key)) {
        linkedHashMap.remove(key)
        linkedHashMap.addOne((key,value))
      }else {
        linkedHashMap.remove(linkedHashMap.head._1)

        linkedHashMap += ((key, value))
      }
    }
  }



}

object Solution {
  def testCase1() : Unit = {
    val lruCache = new LRUCache(2)
    lruCache.put(1,1)
    lruCache.put(2,2)
    println(lruCache.get(1))
    lruCache.put(3,3)
    println(lruCache.get(2))
    lruCache.put(4,4)
    println(lruCache.get(1))
    println(lruCache.get(3))
    println(lruCache.get(4))
  }

  def testCase2() : Unit = {
    val lruCache = new LRUCache(2)
    lruCache.put(2,6)
    lruCache.put(1,5)
    lruCache.put(1,2)
    println(lruCache.get(1))
    println(lruCache.get(2))
  }
  def main(args: Array[String]): Unit = {
    testCase2()

  }
}