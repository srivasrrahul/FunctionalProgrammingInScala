import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    val sortedAnagrams = new mutable.HashMap[String,ListBuffer[String]]()

    for (str <- strs) {
      val sortedStr = str.toSeq.sorted.unwrap
      sortedAnagrams.get(sortedStr) match {
        case Some(lst) => {
          lst.append(str)
        }
        case None => {
          val lst = new ListBuffer[String]
          lst.append(str)
          sortedAnagrams += ((sortedStr,lst))
        }
      }
    }

    val retValue = new ListBuffer[List[String]]
    sortedAnagrams.values.foreach(sortedAnagramsLst => {
      retValue.append(sortedAnagramsLst.toList)
    })

    retValue.toList
  }

  def main(args: Array[String]): Unit = {
    println(groupAnagrams(Array("abc","cba","pqr","rpq","bac")))
  }
}