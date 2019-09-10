import scala.io.Source
import scala.collection.mutable.ListBuffer
class StackAlgo {
  def tail(fileName : String,n : Int) : List[String] = {
    val lst = ListBuffer[String]()
    for (line <- Source.fromFile(fileName).getLines) {
      lst.append(line)
      if (lst.length > n) {
        lst.remove(0)
      }
    }

    lst.toList
  }




}






object StackAlgoTest {
  def main(args: Array[String]): Unit = {
    val stackAlgo = new StackAlgo()
    val fileName = args(0)
    val n = args(1).toInt
    println(stackAlgo.tail(fileName,n))
  }
}