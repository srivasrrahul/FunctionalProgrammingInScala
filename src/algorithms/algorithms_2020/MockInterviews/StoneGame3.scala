import scala.collection.mutable

case class Index(val j : Int,val a : Int,val b : Int)
object Solution {
  def stoneGameIII(stoneValue: Array[Int]): String = {

    def getValue(j : Int) : Int = {
      if (j >= stoneValue.length) {
        0
      }else {
        stoneValue(j)
      }
    }

    val cache = new mutable.HashMap[Index,Int]()
    def itr(j : Int,playerId : Int, aScore : Int,bScore : Int) : Int = {
      if (j >= stoneValue.length) {
        if (playerId == 0) {
          if (aScore > bScore) {
            0
          } else {
            if (aScore < bScore) {
              2
            } else {
              1
            }
          }
        } else {
          if (bScore > aScore) {
            0
          } else {
            if (bScore < aScore) {
              2
            } else {
              1
            }
          }
        }
      } else {
        val index = new Index(j, aScore, bScore)
        if (cache.contains(index)) {
          cache.get(index).get
        } else {
          var res = 0
          if (playerId == 0) {
            val res1 = itr(j + 1, 1, aScore + getValue(j), bScore)
            val res2 = itr(j + 2, 1, aScore + getValue(j) + getValue(j + 1), bScore)
            val res3 = itr(j + 3, 1, aScore + getValue(j) + getValue(j + 1) + getValue(j + 2), bScore)


            if (res1 == 2 || res2 == 2 || res3 == 2) {
              res = 0 //won as other side lost from here
            } else {
              if (res1 == 1 || res2 == 1 || res3 == 1) {
                res = 1 //draw
              } else {
                res = 2 //lost
              }
            }
          } else {
            val res1 = itr(j + 1, 0, aScore, bScore + getValue(j))
            val res2 = itr(j + 2, 0, aScore, bScore + getValue(j) + getValue(j + 1))
            val res3 = itr(j + 3, 0, aScore, bScore + getValue(j) + getValue(j + 1) + getValue(j + 2))

            if (res1 == 2 || res2 == 2 || res3 == 2) {
              res = 0 //won as 1 lost
            } else {
              if (res1 == 1 || res2 == 1 || res3 == 1) {
                res = 1 //draw
              } else {
                res = 2 //lost
              }
            }


          }

          cache += ((index,res))
          res
        }
      }
    }


    val res = itr(0,0,0,0)
    if (res == 0) {
      "Alice"
    }else {
      if (res == 1) {
        "Tie"
      }else {
        "Bob"
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(stoneGameIII(Array(1,2,3,7)))
  }
}