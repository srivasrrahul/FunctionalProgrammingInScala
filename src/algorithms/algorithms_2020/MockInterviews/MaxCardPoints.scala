object Solution {
  def maxScore(cardPoints: Array[Int], K: Int): Int = {
     def itr(j : Int,k : Int,n : Int) : Int = {
       if (n == 0) {
         0
       }else {
         if (j == k) {
           cardPoints(j)
         }else {
           val opt1 = cardPoints(j) + itr(j+1,k,n-1)
           val opt2 = cardPoints(k) + itr(j,k-1,n-1)
           math.max(opt1,opt2)
         }
       }
     }

    itr(0,cardPoints.length-1,K)
  }
}