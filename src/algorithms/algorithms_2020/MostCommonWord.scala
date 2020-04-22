import scala.collection.mutable

object Solution {
  def parseParagraph(para : String,bannedWords : Set[String]) : (mutable.HashMap[Int,mutable.HashSet[String]],mutable.HashMap[String,Int]) = {

    val tokenBuilder = new StringBuilder

    val wordPresent = new mutable.HashMap[String,Int]()
    val countOrder = new mutable.HashMap[Int,mutable.HashSet[String]]()

    def update(string: String) : Unit = {
      var count = 0
      wordPresent.get(string) match {
        case None => {
          wordPresent += ((string,1))
          count = 1
        }
        case Some(localCount) => {
          wordPresent += ((string,localCount+1))
          count = localCount+1
        }
      }

      countOrder.get(count) match {
        case None => {
          val set = new mutable.HashSet[String]()
          set.add(string)
          countOrder += ((count,set))
        }
        case Some(set) => {
          set.add(string)
        }
      }


    }

    for (j <- 0 to para.length-1) {
      val lower = para(j).toLower
      if (lower.isLetter == true) {
        tokenBuilder.append(lower)
      }else {
        if (tokenBuilder.length() > 0) {
          val str = tokenBuilder.toString()
          if (bannedWords.contains(str) == false) {
            update(str)

          }
          tokenBuilder.clear()

        }
      }
    }


    if (tokenBuilder.length() > 0) {
      val str = tokenBuilder.toString()
      if (bannedWords.contains(str) == false) {
        update(str)

      }
      tokenBuilder.clear()
    }

    //println(wordCount)

    (countOrder,wordPresent)


  }
  def mostCommonWord(paragraph: String, banned: Array[String]): String = {
    val bannedWords = banned.toSet
    val (countOrder,wordPresent) = parseParagraph(paragraph,bannedWords)


    //println(wordCount)
    countOrder.last._2.head

  }

  def main(args: Array[String]): Unit = {
    println(mostCommonWord("Bob hit a ball, the hit BALL flew far after it was hit.",Array("hit")))
  }
}