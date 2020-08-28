import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val n : Int,val k : Int)
object Solution {
  type DisjointSet = List[Int]
  type Partition = List[DisjointSet]
  def canPartitionKSubsets(nums: Array[Int], K: Int): Boolean = {

    def dp(n : Int,k : Int) : List[Partition] = {
      val matrix = new mutable.HashMap[Index,List[Partition]]()
      for (j <- 1 to n) {
        val index = new Index(j,j)
        val partition = new ListBuffer[DisjointSet]
        for (p <- 1 to j) {
          val disjointSet = List(p)
          partition.append(disjointSet)
        }

        matrix += ((index,List(partition.toList)))
      }

      for (row <- 1 to n) {
        val index = new Index(row,1)
        val disjointSet = Range(1,row+1).toList
        val partition = List(disjointSet)
        matrix += ((index,List(partition)))
      }

      for (row <- 3 to n) {
        for (col <- 2 to row-1) {
          val retValue = new ListBuffer[Partition]
          val takenIndex = new Index(row-1,col-1)
         // val partitions1 = itr(n - 1, k - 1)
          val partitions1 = matrix.get(takenIndex).get //shud never be null
          for (partition <- partitions1) {
            val newParition = List(row) :: partition
            retValue.append(newParition)
          }

          //val partitions2 = itr(n - 1, k)
          val takenIndex1 = new Index(row-1,col)
          //println(takenIndex1)
          val partitions2 = matrix.get(takenIndex1).get
          for (partition <- partitions2) {
            //println("Source " + partition)
            for (j <- 0 to partition.length - 1) {
              val (firstPartDisjointSet, lastPartDisjointSet) = partition.splitAt(j)
              val newDisjointSet = firstPartDisjointSet ++ List(row :: lastPartDisjointSet.head) ++ lastPartDisjointSet.tail
              val newParition = List(newDisjointSet)
              retValue.append(newDisjointSet)
            }
          }

          val newIndex = new Index(row,col)
          matrix += ((newIndex,retValue.toList))
        }
      }

      val finalIndex = new Index(n,k)
      matrix.get(finalIndex).get

    }

    val cache = new mutable.HashMap[Index,List[Partition]]()
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
          val index = new Index(n,k)
          if (cache.contains(index)) {
            cache.get(index).get
          }else {
            val retValue = new ListBuffer[Partition]
            val partitions1 = itr(n - 1, k - 1)
            for (partition <- partitions1) {
              val newParition = List(n) :: partition
              retValue.append(newParition)
            }

            val partitions2 = itr(n - 1, k)
            for (partition <- partitions2) {
              //println("Source " + partition)
              for (j <- 0 to partition.length - 1) {
                val (firstPartDisjointSet, lastPartDisjointSet) = partition.splitAt(j)
                val newDisjointSet = firstPartDisjointSet ++ List(n :: lastPartDisjointSet.head) ++ lastPartDisjointSet.tail
                val newParition = List(newDisjointSet)
                retValue.append(newDisjointSet)
              }
            }

            val finalLst = retValue.toList
            cache += ((index,finalLst))
            finalLst
          }
        }
      }
    }

    //val test = dp(nums.length,K)
    //println(test)
    //val partitions = itr(nums.length,K)
    val partitions = dp(nums.length,K)
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
    val lst = canPartitionKSubsets(Array(4, 3, 2, 3, 5, 2, 1),4)
    println(lst)

  }
}