object Solution {
  def countVowelStrings(n: Int): Int = {
    def next(ch : Char) : List[Char] = {
        ch match {
          case 'a' => List('a','e','i','o','u')
          case 'e' => List('e','i','o','u')
          case 'i' => List('i','o','u')
          case 'o' => List('o','u')
          case 'u' => List('u')
          case _ => {
            List('a','e','i','o','u')
          }
        }

    }

    def itr(prev : Char,j : Int) : Option[Int] = {
      if (j == n) {
        val nextLst = next(prev)
        Some(nextLst.size)
      }else {
        val nextLst = next(prev)
        var count = 0
        for (n <- nextLst) {
          val retValue = itr(n,j+1)
          if (retValue.isDefined) {
            count = count + retValue.get
          }
        }

        if (count > 0) {
          Some(count)
        }else {
          None
        }
      }
    }

    itr('0',1).get
  }

  def main(args: Array[String]): Unit = {
    println(countVowelStrings(33))
  }
}