import scala.collection.mutable

object Solution {
  def minDays(n: Int): Int = {
    val cache = new mutable.HashMap[Int,Int]()
    def itr(current : Int) : Int = {
      println(current)
      if (cache.contains(current)) {
        cache.get(current).get
      }else {
        current match {
          case 1 => 1
          case _ => {
            var opt1: Option[Int] = None
            if (current % 2 == 0) {
              opt1 = Some(itr(current / 2))
            }

            var opt2: Option[Int] = None
            if (current % 3 == 0) {
              opt2 = Some(itr(current / 3))
            }

            var res = 0
            (opt1, opt2) match {
              case (None, None) => res = 1+itr(current - 1)
              case (Some(c1), None) => res = 1+math.min(c1, itr(current - 1))
              case (None, Some(c2)) => res = 1+math.min(c2, itr(current - 1))
              case (Some(c1), Some(c2)) =>
                res = 1 + Array(c1, c2, itr(current - 1)).min
            }

            cache += ((current,res))
            res
          }

        }
      }
    }

    itr(n)
  }

  def main(args: Array[String]): Unit = {
    println(minDays(9209408))
  }
}