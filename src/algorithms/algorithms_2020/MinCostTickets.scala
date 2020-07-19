import scala.collection.mutable
case class Id(val index : Int,val ticketMaValidity : Int,val totalCostTillNow : Int)
object Solution {
  def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {
    val cache = new mutable.HashMap[Id,Int]()
    def itr(index : Int,ticketMaxValidity : Int,totalCostTillNow : Int) : Int = {
      if (index == days.length) {
        totalCostTillNow
      }else {
        //four options
        //buy 1 day
        //buy 7 day
        //buy 30 day
        //not buy at all
        val id = new Id(index,ticketMaxValidity,totalCostTillNow)
        if (cache.contains(id)) {
          cache.get(id).get
        }else {
          val opt1 = itr(index + 1, scala.math.max(ticketMaxValidity, days(index)), totalCostTillNow + costs(0))
          val opt2 = itr(index + 1, scala.math.max(ticketMaxValidity, days(index) + 6), totalCostTillNow + costs(1))
          val opt3 = itr(index + 1, scala.math.max(ticketMaxValidity, days(index) + 29), totalCostTillNow + costs(2))

          val minOpt = Array(opt1, opt2, opt3).min

          //println("for day " + days(index) + " " +opt1 + " " + opt2 + " " + opt3)

          if (days(index) <= ticketMaxValidity) {
            val opt4 = itr(index + 1, ticketMaxValidity, totalCostTillNow)
            val minValue = scala.math.min(opt4, minOpt)
            cache += ((id,minValue))
            minValue
          } else {
            cache += ((id,minOpt))
            minOpt
          }
        }

      }
    }

    itr(0,0,0)
  }

  def main(args: Array[String]): Unit = {
    println(mincostTickets(Array(1,4,6,7,8,20),Array(2,7,15)))
  }
}