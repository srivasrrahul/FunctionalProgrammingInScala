import scala.collection.mutable.ListBuffer

object Solution {
  type DisjointSet = List[Int]
  type Partition = List[DisjointSet]
  def canPartitionKSubsets(nums: Array[Int], K: Int): Boolean = {
    def itr(n : Int,k : Int) : List[Partition] = {
      if (n == k) {
        //there are k disjoint sets of each of size 1 in 1 partition
        val partition = new ListBuffer[DisjointSet]
        for (j <- 1 to n) {
          val disjointSet = List(j)
          partition.append(disjointSet)
        }

        List(partition.toList)
      }else {
        if (k == 1) {
          //there is one disjointset of size n
          val disjointSet = Range(1,n+1).toList
          val partition = List(disjointSet)
          List(partition)
        }else {
          val retValue = new ListBuffer[Partition]
          val partitions1 = itr(n-1,k-1)
          for (partition <- partitions1) {
            val newParition = List(n) :: partition
            retValue.append(newParition)
          }

          val partitions2 = itr(n-1,k)
          for (partition <- partitions2) {
            //println("Source " + partition)
            for (j <- 0 to partition.length-1) {
              val (firstPartDisjointSet,lastPartDisjointSet) = partition.splitAt(j)
              val newDisjointSet = firstPartDisjointSet ++ List(n::lastPartDisjointSet.head) ++ lastPartDisjointSet.tail
              val newParition = List(newDisjointSet)
              retValue.append(newDisjointSet)
            }
          }

          retValue.toList
        }
      }
    }

    val partitions = itr(nums.length,K)
    var equalSumPartition = false
    for (partition <- partitions if equalSumPartition == false) {

      equalSumPartition = true
      var perSetSum : Option[Int] = None
      for (disjointSet <- partition if equalSumPartition == true) {
        var sum = 0
        for (index <- disjointSet) {
          sum = sum + nums(index-1)
        }

        perSetSum match {
          case None => {
            perSetSum = Some(sum)
          }
          case Some(prevSum) => {
            if (prevSum != sum) {
              equalSumPartition = false
            }
          }
        }
      }


    }

    //println(partitions.mkString("\n"))
    equalSumPartition
  }

  def main(args: Array[String]): Unit = {
    val lst = canPartitionKSubsets(Array(4, 3, 2, 3, 5, 2, 1000),4)
    println(lst)

  }
}