import scala.collection.mutable

case class Index(val rowId : Int,val pos : Int)

object Solution {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val rev = triangle.reverse
    val map = new mutable.HashMap[Index,Int]()
    val head = rev.head
    var j = 0
    var rowId = 0
    head.map(x => {
      val index = new Index(rowId,j)
      map += ((index,x))
      j = j + 1
    })

    rowId = rowId+1

    var itr = rev.tail
    while (itr != Nil) {
      //println(itr.head)
      val localHead = itr.head
      j = 0
      localHead.map(x => {
        val index = new Index(rowId,j)
        //println(index)
        val option1Index = new Index(rowId-1,j)
        val option2Index = new Index(rowId-1,j+1)
        val s1 = x + map.get(option1Index).get
        val s2 = x + map.get(option2Index).get
        map += ((index,math.min(s1,s2)))
        j = j + 1
      })

      itr = itr.tail
      rowId = rowId+1
    }

    map.get(new Index(triangle.length-1,0)).get

  }

  def main(args: Array[String]): Unit = {
    println(minimumTotal(List(List(2),List(3,4),List(6,5,7),List(4,1,8,3))))
  }

}