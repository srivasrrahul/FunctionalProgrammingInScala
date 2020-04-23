import scala.collection.mutable

object Solution {
  def shortestDistance(words: Array[String], word1: String, word2: String): Int = {
    val wordIndex = new mutable.HashMap[String,mutable.TreeSet[Int]]()

    for (j <- 0 to words.length-1) {
      wordIndex.get(words(j)) match {
        case None => {
          val s = new mutable.TreeSet[Int]
          s.add(j)

          wordIndex += ((words(j),s))
        }
        case Some(set) => {
          set.add(j)
        }
      }
    }

    val word1Index = wordIndex.get(word1).get
    val word2Index = wordIndex.get(word2).get

    var minDiff = Int.MaxValue

    for (index1 <- word1Index) {
      for (index2 <- word2Index) {
        val diff = scala.math.abs(index1-index2)
        if (diff < minDiff) {
          minDiff = diff
        }
      }
    }

    minDiff
  }

  def main(args: Array[String]): Unit = {
    println(shortestDistance(Array(),"asdads","hi"))
  }
}