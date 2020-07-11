import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Node(val index : Int,val indexVal : Int)
object NodeOrdering extends Ordering[Node] {
  override def compare(x: Node, y: Node): Int = {
    x.indexVal.compareTo(y.indexVal)
  }
}
object Solution {
  def maxSumAfterPartitioning(A: Array[Int], K: Int): Int = {
    val pq = mutable.PriorityQueue.empty[Node](NodeOrdering)
    for (j <- 0 to A.length-1) {
      val node = new Node(j,A(j))
      pq.addOne(node)
    }

    def findPossibleSets(source : Int,visited : Set[Int]) : List[Range] = {
      val lstBuffer = new ListBuffer[Range]
      def path(range: Range) : Unit = {
        //println(range)
        if (range.size <= K) {
          lstBuffer.append(range)

          val first = range.head
          val last = range.last
          if (first-1 >=0 && visited.contains(first-1) == false) {
            path(Range(first-1,last+1))
          }

          if (last+1 < A.length && visited.contains(last+1) == false) {
            path(Range(first,last+2))
          }
        }


      }

      path(Range(source,source+1))

      lstBuffer.toList
    }


    //println(pq)
    var maxSum = Int.MinValue
    def findSums(localPq : mutable.PriorityQueue[Node],visited : Set[Int],priorSum : Int) : Unit = {
      if (localPq.isEmpty == true) {
        //sumLst.append(priorSum)
        if (priorSum > maxSum) {
          maxSum = priorSum
        }
      }else {
        val top = localPq.dequeue()
        if (visited.contains(top.index)) {
          findSums(localPq.clone(),visited,priorSum)
        }else {
          val paths = findPossibleSets(top.index,visited)
          //println(paths)
          for (path <- paths) {
            val currentSum = top.indexVal*path.size
            val localSet = new mutable.HashSet[Int]()
            for (x <- path) {
              localSet.add(x)
            }
            localSet.addAll(visited)
            findSums(localPq.clone(),localSet.toSet,priorSum+currentSum)
          }
        }
      }
    }
    //println(A.length)
    findSums(pq.clone(),Set(),0)


    maxSum

  }

  def main(args: Array[String]): Unit = {
    println(maxSumAfterPartitioning(Array(1,15,7,9,2,5,10),3))
    //println(maxSumAfterPartitioning(Array(20779,436849,274670,543359,569973,280711,252931,424084,361618,430777,136519,749292,933277,477067,502755,695743,413274,168693,368216,677201,198089,927218,633399,427645,317246,403380,908594,854847,157024,719715,336407,933488,599856,948361,765131,335089,522119,403981,866323,519161,109154,349141,764950,558613,692211),26))
  }
}