object Solution {
  def countVowelPermutation(n: Int): Int = {
    def getNext(current : Char) : List[Char] = {
      current match {
        case 'a' => List('e')
        case 'e' => List('a','i')
        case 'i' => List('a','e','o','u')
        case 'o' => List('i','u')
        case 'u' => List('a')
        case _ => List('a','e','i','o','u')
      }
    }

    var count = 0
    def itr(j : Int,state : Char) : Unit = {
      if (j == n) {
        count = count+1
      }else {
        for (next <- getNext(state)) {
          itr(j+1,next)
        }
      }
    }

    itr(0,'_')
    count
  }
}