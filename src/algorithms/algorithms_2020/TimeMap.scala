import scala.collection.mutable.TreeMap
import scala.collection.mutable.HashMap

class TimeMap {

  /** Initialize your data structure here. */
  type TimeStamp = Int
  type TimedMap = TreeMap[Int,String]
  val key_timed_map = new HashMap[String,TimedMap]()

  def get_time_val(t : TimeStamp,timed_map : TimedMap) : String = {
//    val p = timed_map.rangeFrom(t)
//    if (p.size > 0) {
//      p.head._2
//    }else {
//      ""
//    }
    timed_map.minAfter(t) match {
      case Some((k,v)) => v
      case None => ""
    }
  }
  def set(key: String, value: String, timestamp: Int) : Unit = {
    key_timed_map.get(key) match  {
      case None => {
        val timed_map = new TimedMap()(Ordering[Int].reverse)
        timed_map += ((timestamp,value))
        key_timed_map += ((key,timed_map))
      }
      case Some(timed_map) => {
        timed_map += ((timestamp,value))
        key_timed_map += ((key,timed_map))
      }
    }
  }

  def get(key: String, timestamp: Int): String = {
    key_timed_map.get(key) match {
      case None => ""
      case Some(timed_map) => get_time_val(timestamp,timed_map)
    }
  }





}

object Solution {
  def main(args: Array[String]): Unit = {
    println("test")
    val m = new TimeMap
    m.set("foo","bar",1)
    println("Test : " + m.get("foo",1))
    println("Test : " + m.get("foo",3))
    m.set("foo","bar2",4)
    println("Test : " + m.get("foo",1000))
  }
}