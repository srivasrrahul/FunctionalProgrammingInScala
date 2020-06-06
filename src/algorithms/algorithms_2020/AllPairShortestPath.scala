object Solution {
  def findTheCity(n: Int, edges: Array[Array[Int]], distanceThreshold: Int): Int = {
    val matrix = Array.ofDim[Int](n,n)

    for (i <- 0 to n-1) {
      for (j <- 0 to n-1) {
        if (i == j) {
          matrix(i)(j) = 0
        }else {
          matrix(i)(j) = Int.MaxValue
        }
      }
    }

    for (edge <- edges) {
      //if (edge(2) <= distanceThreshold) {
        matrix(edge(0))(edge(1)) = edge(2)
        matrix(edge(1))(edge(0)) = edge(2)
      //}

    }


    for (k <- 0 to n-1) {
      for (i <- 0 to n-1) {
        for (j <- 0 to n-1) {
          var option1 : Option[Int] = None
          if (matrix(i)(j) != Int.MaxValue) {
            option1 = Some(matrix(i)(j))
          }

          var option2 : Option[Int] = None
          if (matrix(i)(k) != Int.MaxValue && matrix(k)(j) != Int.MaxValue) {
            option2 = Some(matrix(i)(k) + matrix(k)(j))
          }

          (option1,option2) match {
            case (None,None) => {

            }
            case (_,None) => {

            }
            case (None,Some(c2)) =>{
              matrix(i)(j) = c2
            }
            case (Some(c1),Some(c2)) => {
              if (c2 < c1) {
                matrix(i)(j) = c2
              }
            }
          }
        }
      }
    }

    var minCountCity = 0
    var minCount = Int.MaxValue
    for (i <- 0 to n-1) {
      var cityCovered = 0
      println(matrix(i).mkString(","))
      for (j <- 0 to n-1) {
        if (i != j && matrix(i)(j) <= distanceThreshold) {
          cityCovered = cityCovered + 1
        }
      }

      if (cityCovered <= minCount) {
        minCountCity = i
        minCount = cityCovered
      }

    }

    minCountCity

  }

  def main(args: Array[String]): Unit = {
    println(findTheCity(4,Array(Array(0,1,3977),Array(2,3,8807),Array(0,2,2142),Array(1,3,1201)),8174))
    //val array = Array(Array(0,1,2),Array(0,4,8),Array(1,2,3),Array(1,4,2),Array(2,3,1),Array(3,4,1))
    //println(findTheCity(5,array,2))
  }
}