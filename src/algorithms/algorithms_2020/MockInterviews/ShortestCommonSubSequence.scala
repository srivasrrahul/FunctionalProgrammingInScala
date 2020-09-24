import scala.collection.mutable

case class Index(val x : Int,val y : Int)
object Solution {
  def shortestCommonSupersequence(str1: String, str2: String): String = {
    val cache = new mutable.HashMap[Index,String]()
    def itr(j : Int,k : Int) : String = {
      if (j == 0 || k == 0) {
        //Either of them is 0
        //find the substr
        //println(j + " " + k)
        if (j == 0 && k == 0) {
          if (str1(j) == str2(k)) {
            str1(j).toString
          }else {
            val ret = str1(j).toString ++ str2(k).toString
            //println("ret " + ret)
            ret
          }
        }else {
          var count = 0
          if (j == 0) {
            var found = false
            for (x <- 0 to k) {
              if (str2(x) == str1(j)) {
                found = true
              }
            }

            if (found) {
              str2.substring(0,k+1)
            }else {
              str2.substring(0,k+1) ++ str1(j).toString
            }
          }else {
            var found = false
            for (x <- 0 to j) {
              if (str1(x) == str2(k)) {
                found = true
              }
            }

            if (found) {
              str1.substring(0,j+1)
            }else {
              str1.substring(0,j+1) ++ str2(k).toString
            }
          }
        }
      }else {
        val index = new Index(j,k)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          if (str1(j) == str2(k)) {
            val pending = itr(j - 1, k - 1) //++ str1(j).toString
            val stringBuilder = new StringBuilder
            stringBuilder.appendAll(pending)
            stringBuilder.append(str1(j))
            val retValue = stringBuilder.toString()
            cache += ((index,retValue))
            retValue
          } else {
            val opt1 = itr(j - 1, k)
            val opt2 = itr(j, k - 1)
            if (opt1.length <= opt2.length) {

              val stringBuilder = new StringBuilder
              stringBuilder.appendAll(opt1)

              //val retValue = opt1 ++ str1(j).toString
              val retValue = stringBuilder.append(str1(j)).toString()
              cache += ((index,retValue))
              retValue
            } else {
              val stringBuilder = new StringBuilder
              stringBuilder.appendAll(opt2)
              stringBuilder.append(str2(k))
              val retValue = stringBuilder.toString()
              cache += ((index,retValue))
              retValue
            }
          }
        }
      }
    }

    itr(str1.length-1,str2.length-1)
  }

  def main(args: Array[String]): Unit = {
    println(shortestCommonSupersequence("abac","cab"))

  }
}