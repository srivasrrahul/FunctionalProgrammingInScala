import util.control.Breaks._
import scala.collection.mutable.HashMap
object Solution {
  def divisorGame(N: Int): Boolean = {

    val cache = new HashMap[Int,Boolean]
    def play(y : Int) : Boolean = {
      cache.get(y) match {
        case Some(z) => z
        case None => {
          y match {
            case 1 => {
              false
            }
            case 2 => {
              true
            }
            case _ => {
              var result = false
              for (j <- 1 to y-1) {
                if (y % j == 0) {
                  val res = play(y-j)
                  if (res == false) {
                    result = true
                  }
                }
              }

              cache += (y -> result)
              result
            }
          }
        }
      }
    }

    //alex(N)
    play(N)
  }




  def main(args: Array[String]): Unit = {
    println(divisorGame(1000))
  }
}