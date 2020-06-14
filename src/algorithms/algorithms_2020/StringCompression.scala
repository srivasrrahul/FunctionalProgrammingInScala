import scala.collection.mutable.ListBuffer

object Solution {
  def compress(chars: Array[Char]): Int = {
    if (chars.isEmpty) {
      0
    }else {
      val lst = new ListBuffer[(Char, Int)]
      var prev = chars(0)
      var prevCount = 1
      for (j <- 1 to chars.length - 1) {
        if (chars(j) == prev) {
          prevCount = prevCount + 1
        } else {
          lst.append((prev, prevCount))
          prev = chars(j)
          prevCount = 1
        }
      }


      lst.append((prev, prevCount))
      println(lst)

      var itr = lst
      var j = 0
      while (itr != Nil) {
        if (itr.head._2 == 1) {
          chars(j) = itr.head._1
          j = j + 1
        } else {
          chars(j) = itr.head._1
          j = j + 1
          val intStr = itr.head._2.toString
          for (k <- 0 to intStr.length - 1) {
            chars(j) = intStr(k)
            j = j + 1
          }
        }

        itr = itr.tail
      }

      j

    }


  }

  def main(args: Array[String]): Unit = {
    val arr = Array('a','a','a','b','b')
    val len = compress(arr)

    for (j <- 0 to len-1) {
      print(arr(j) + ",")
    }
  }
}