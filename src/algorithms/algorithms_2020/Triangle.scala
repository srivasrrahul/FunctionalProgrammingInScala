import scala.collection.mutable

case class Index(val rowId : Int,val pos : Int)
object Solution {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val cache = new mutable.HashMap[Index,Set[Int]]()
    def pathSum(prevPos : Int, lst : List[List[Int]],rowId : Int) : Set[Int] = {
      lst match {
        case (x::Nil) => {
          if (prevPos +1 < x.length) {
            Set(x(prevPos),x(prevPos+1))
          }else {
            Set(x(prevPos))
          }
        }
        case (head::xs) => {
          //println(prevPos)
          val index = new Index(rowId,prevPos)
          cache.get(index) match {
            case None => {
              val a = head(prevPos)
              val sumLsts1 = pathSum(prevPos, xs,rowId+1)

              if (prevPos + 1 < head.length) {
                val b = head(prevPos + 1)
                val sumLsts2 = pathSum(prevPos + 1, xs,rowId+1)

                val updatedLst1 = sumLsts1.map(x => a + x)
                val updatedLst2 = sumLsts2.map(x => b + x)
                val retValue = updatedLst1.union(updatedLst2)
                cache += ((index,retValue))
                retValue

              } else {
                val retValue = sumLsts1.map(x => a + x)
                cache += ((index,retValue))
                retValue
              }
            }
            case Some(existsingVal) => {
              existsingVal
            }
          }

        }
        case _ => {
          Set()
        }
      }
    }


    val s = pathSum(0,triangle,0)
    //println(s)
    s.min
  }

  def main(args: Array[String]): Unit = {
    println(minimumTotal(List(List(2),List(3,4),List(6,5,7),List(4,1,8,3))))
  }

}