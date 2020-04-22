import scala.collection.mutable

class Logger() {

  /** Initialize your data structure here. */

  val msgPrintTime = new mutable.HashMap[String,Int]()
  /** Returns true if the message should be printed in the given timestamp, otherwise returns false.
        If this method returns false, the message will not be printed.
        The timestamp is in seconds granularity. */


  def shouldPrintMessage(timestamp: Int, message: String): Boolean = {
    msgPrintTime.get(message) match {
      case Some(prevPrintTime) => {
        if (timestamp - prevPrintTime >= 10) {
          msgPrintTime += ((message,timestamp))
          true
        }else {
          false
        }
      }
      case None => {
        msgPrintTime += ((message,timestamp))
        true
      }
    }
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val logger = new Logger();

    // logging string "foo" at timestamp 1
    println(logger.shouldPrintMessage(1, "foo")); //returns true;

    // logging string "bar" at timestamp 2
    println(logger.shouldPrintMessage(2,"bar")); //returns true;

    // logging string "foo" at timestamp 3
    println(logger.shouldPrintMessage(3,"foo")); //returns false;

    // logging string "bar" at timestamp 8
    println(logger.shouldPrintMessage(8,"bar")); //returns false;

    // logging string "foo" at timestamp 10
    println(logger.shouldPrintMessage(10,"foo")); //returns false;

    // logging string "foo" at timestamp 11
    println(logger.shouldPrintMessage(11,"foo")); //returns true;

  }
}
