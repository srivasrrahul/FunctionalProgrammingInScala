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
      linkedHashMap += ((key,value))
    }else {
      //println("Adding key " + key)
      //println("removed key " + linkedHashMap.head._1)
      linkedHashMap.remove(linkedHashMap.head._1)
      linkedHashMap += ((key,value))
    }
  }



}

object Solution {
  def main(args: Array[String]): Unit = {
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
}