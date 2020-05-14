import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Solution {
  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {

    val nodes = new mutable.HashMap[Int,String]
    val nodesDict = new mutable.HashMap[String,Int]
    var j = 1

    nodes += ((0, beginWord))
    nodesDict += ((beginWord, 0))

    for (word <- wordList) {
      nodes += ((j,word))
      nodesDict += ((word,j))
      j = j + 1
    }

    if (nodesDict.contains(endWord) == false) {
      0
    }else {
      val nonVisitedSet = new mutable.HashSet[Int]()
      for (k <- 0 to j-1) {
        nonVisitedSet.add(k)
      }
      val q = new mutable.Queue[(Int, Int)]()
      q.addOne((0, 0))
      nonVisitedSet.remove(0)

      def diffByOne(word1: String, word2: String): Boolean = {
        if (word1.length != word2.length) {
          false
        } else {
          var diffCount = 0
          for (j <- 0 to word1.length - 1 if diffCount <= 1) {
            if (word1(j) != word2(j)) {
              diffCount = diffCount + 1
            }
          }

          diffCount == 1
        }
      }

      def getNeighboursWord(wordIndex: Int): List[Int] = {
        val retvalue = new ListBuffer[Int]
        val word = nodes.get(wordIndex).get
        val stringBuilder = new StringBuilder(word)
        for (k <- 0 to word.length-1) {
          val ch = stringBuilder(k)
          for (i <- 'a' to 'z') {
            if (i != ch) {
              stringBuilder(k) = i
              val newStr = stringBuilder.toString()
              //println(newStr)
              if (nodesDict.contains(newStr) && nonVisitedSet.contains(nodesDict.get(newStr).get)) {
                retvalue.append(nodesDict.get(newStr).get)
              }
            }
          }
          //replace it back
          stringBuilder(k) = ch

        }
//        for (nonVisited <- nonVisitedSet) {
//          if (diffByOne(nodes.get(nonVisited).get, word)) {
//            retvalue.append(nonVisited)
//          }
//        }


        retvalue.toList


      }

      var pathLength: Option[Int] = None
      val path = new mutable.HashMap[Int, Int]()
      breakable {
        //println(q)
        while (q.isEmpty == false) {
          //println(q)
          val top = q.dequeue()
          if (nodes.get(top._1).get == endWord ) {
            pathLength = Some(top._2)
            break
          }

          //visitedSet.add(top._1)
          nonVisitedSet.remove(top._1)

          val neigbourLst = getNeighboursWord(top._1)
          //println("For top " + top._1 + " " + neigbourLst)
          for (neigbour <- neigbourLst) {
            q.addOne((neigbour, top._2 + 1))
            path += ((neigbour, top._1))
          }
        }
      }

//      println(nodesDict)
//      println(nodes)
//      println(path)
      pathLength match {
        case None => 0
        case Some(len) => len + 1
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val len = ladderLength("hit","cog",List("hot","dot","dog","lot","log"))
    println(len)
  }
}