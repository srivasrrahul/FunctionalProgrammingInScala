import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def topKFrequent(words: Array[String], k: Int): List[String] = {
    val dictCount = new mutable.HashMap[String,Int]()

    val countOrder = new mutable.TreeMap[Int,mutable.TreeSet[String]]()

    for (word <- words) {
      var prevCount = -1
      dictCount.get(word) match {
        case Some(count) => {
          prevCount = count
          dictCount += ((word,count+1))
        }
        case _ => {
          prevCount = 0
          dictCount += ((word,1))
        }
      }

      val currentCount =  prevCount+1

      if (prevCount == 0) {
        //No removal
        countOrder.get(currentCount) match {
          case Some(wordTree) => {
            wordTree.addOne(word)
          }
          case None => {
            val wordTree = new mutable.TreeSet[String]()
            wordTree.addOne(word)

            countOrder += ((1,wordTree))
          }
        }
      }else {
        countOrder.get(prevCount) match {
          case Some(wordTree) => {
            wordTree.remove(word)
          }
          case None => {
            println("Issue")
          }
        }

        countOrder.get(currentCount) match {
          case Some(wordTree) => {
            wordTree.addOne(word)
          }
          case None => {
            val wordTree = new mutable.TreeSet[String]()
            wordTree.addOne(word)

            countOrder += ((currentCount,wordTree))
          }
        }
      }
    }

    val lst = new ListBuffer[String]
    var countAdded = 0
    while (countAdded < k) {
      val lastTree = countOrder.last._2
      while (lastTree.isEmpty == false && countAdded < k) {
        lst.addOne(lastTree.head)
        lastTree.remove(lastTree.head)
        countAdded = countAdded + 1

      }
      countOrder.remove(countOrder.last._1)
    }

    lst.toList
  }

  def main(args: Array[String]): Unit = {
    println(topKFrequent(Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is"),4))
  }
}