import scala.collection.mutable.ListBuffer

object Solution {
  def permute(nums: Array[Int]): List[List[Int]] = {
    def itr(n : Int) : List[List[Int]] = {
      if (n == nums.length-1) {

        List(List(nums(n)))
      }else {
        val pendingLst = itr(n+1)

        def updateLst(newInt : Int,oldPermu : List[Int]) : List[List[Int]] = {
          val retValue = new ListBuffer[List[Int]]

          retValue.addOne(newInt :: oldPermu)

          var oldLst = List[Int]()
          def addLst(lst : List[Int]) : Unit = {
            lst match {
              case x::xs => {
                oldLst = oldLst ++ List(x)
                retValue.append(oldLst ++ List(newInt) ++ xs)
                addLst(xs)
              }
              case Nil => {
//                retValue.append(oldLst ++ List(newInt))
//                retValue.append(oldLst)

              }
            }
          }


          addLst(oldPermu)
          //println("Added " + retValue.toList.mkString(","))

          retValue.toList

        }

        val retValue = new ListBuffer[List[Int]]
        for (p <- pendingLst) {
          retValue.addAll(updateLst(nums(n),p))
        }

        retValue.toList

      }
    }

    itr(0)
  }

  def main(args: Array[String]): Unit = {
    println(permute(Array(1,2,3)))
  }
}