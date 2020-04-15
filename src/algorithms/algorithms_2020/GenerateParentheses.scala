import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def generateParenthesis(n: Int): List[String] = {
    def itr(priorString : String,openCount : Int,pendingOpen : Int,pendingClosed : Int) : List[String] = {
      //println(priorString)
      if (pendingOpen == 0 && pendingClosed == 0) {
        List()
      }else {
        if (pendingOpen == 0) {

          val stringBuilder = new StringBuilder
          stringBuilder.append(priorString)
          for (j <- 0 to pendingClosed-1) {
            stringBuilder.append(")")
          }

          //println("If pending open = 0" + stringBuilder.toString())
          List(stringBuilder.toString())
        }else {

          val retValue = new ListBuffer[String]
          if (openCount == 0) {
            //We'll have to open 1
            val lst = itr(priorString + "(",1,pendingOpen-1,pendingClosed)
            retValue.addAll(lst)
          }else {
            //Two options
            //Option 1 : Close existing open
            val closeOneOpen = itr(priorString + ")",openCount-1,pendingOpen,pendingClosed-1)

            //option 2 : open a new one
            val addOneOpne = itr(priorString + "(",openCount+1,pendingOpen-1,pendingClosed)
            retValue.addAll(closeOneOpen)
            retValue.addAll(addOneOpne)
          }

          retValue.toList
        }
      }
    }
    itr("",0,n,n)
  }

  def main(args: Array[String]): Unit = {
    println(generateParenthesis(4))
  }
}