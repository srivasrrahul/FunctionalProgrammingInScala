import scala.collection.mutable

class Logger() {

  /** Initialize your data structure here. */


  val loggedMsg = new mutable.TreeMap[Int,Set[String]]()
  /** Returns true if the message should be printed in the given timestamp, otherwise returns false.
        If this method returns false, the message will not be printed.
        The timestamp is in seconds granularity. */
  def shouldPrintMessage(timestamp: Int, message: String): Boolean = {
    val lastMessages = loggedMsg.rangeUntil(timestamp-9)
    var found = false
    for ((_,printedSet) <- lastMessages if found == false) {
      if (printedSet.contains(message)) {
        found = true
      }
    }


    if (found == false) {
      val defaultSet = loggedMsg.getOrElseUpdate(timestamp,Set())
      loggedMsg += ((timestamp,defaultSet.+(message))) //print now
      true
    }else {
      false
    }
  }

}