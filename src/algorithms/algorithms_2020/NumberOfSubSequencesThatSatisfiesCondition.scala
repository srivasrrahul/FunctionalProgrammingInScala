import scala.collection.mutable.ListBuffer

case class SubSeq(val min : Int,val max : Int)
object Solution {
  def numSubseq(nums: Array[Int], target: Int): Int = {
    var countValid : Long = 0
    def itr(currentIndex : Int) : List[SubSeq] = {
      if (currentIndex < 0) {
        List()
      }else {

        val retValue = new ListBuffer[SubSeq]

        retValue.append(new SubSeq(nums(currentIndex),nums(currentIndex))) //only current

        //subsequnec including only current
        val allSubSeqsPrior = itr(currentIndex-1)

        //inclue self with all prior
        for (subSeq <- allSubSeqsPrior) {
          retValue.append(new SubSeq(math.min(subSeq.min,nums(currentIndex)),math.max(subSeq.max,nums(currentIndex))))
        }

        retValue.appendAll(allSubSeqsPrior) //dont include current
        retValue.toList
      }
    }

    val subSeqsLst = itr(nums.length-1)
    for (subSeq <- subSeqsLst) {
      val min = subSeq.min
      val max = subSeq.max
      if (min+max <= target) {
        countValid = countValid+1
      }
    }

    val largePrime = Math.pow(10,9) + 7
    (countValid % largePrime).toInt
  }
}