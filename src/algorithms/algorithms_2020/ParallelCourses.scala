import scala.collection.mutable

class Graph(val n : Int) {
  val edges = new mutable.HashMap[Int,mutable.HashSet[Int]]()
  for (j <- 0 to n-1) {
    edges += ((j,new mutable.HashSet[Int]()))
  }

  def addEdge(edge : Array[Int]) : Unit = {
    val defaultSet = edges.get(edge(0)-1).get
    defaultSet.add(edge(1)-1)
  }

  def getNeigbours(source : Int) : Set[Int] = {
    edges.get(source).get.toSet
  }

  def isEdge(source : Int,dest : Int) : Boolean = {
    edges.get(source).get.contains(dest)
  }
}
object Solution {
  def minimumSemesters(N: Int, relations: Array[Array[Int]]): Int = {
    //check if its a dag
    //if its a dig do topological sort
    val graph = new Graph(N)
    for (relation <- relations) {
      graph.addEdge(relation)
    }

    var time = 0
    val preTime = new Array[Int](N)
    val postTime = new Array[(Int,Int)](N)
    for (j <- 0 to postTime.length-1) {
      postTime(j) = (0,j)
    }
    def getNewTime() : Int = {
      val newTime = time + 1
      time = newTime
      newTime
    }

    val visited = new mutable.HashSet[Int]()
    var cycleExists = false
    def explore(source : Int) : Unit = {
      visited.add(source)
      preTime(source) = getNewTime()
      val neigbours = graph.getNeigbours(source)
      for (neigbour <- neigbours) {
        if (visited.contains(neigbour) == false) {
          explore(neigbour)
        }else {
          if (postTime(neigbour)._1 == 0) {
            cycleExists = true
          }
        }
      }

      postTime(source) = (getNewTime(),source)
    }

    for (j <- 0 to N-1) {
      if (visited.contains(j) == false) {
        explore(j)
      }
    }

    if (cycleExists == false) {
      postTime.sortInPlace()(new Ordering[(Int,Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          y._1.compareTo(x._1)
        }
      })

      var semesters = 1
      val currentSemesterCourses = new mutable.HashSet[Int]()
      currentSemesterCourses.add(postTime(0)._2)

      println(postTime.mkString(","))
      for (j <- 1 to postTime.length-1) {
        val (currentPostTime,courseId) = postTime(j)
        var connectivity = false
        for (currentSemesterCourse <- currentSemesterCourses if connectivity == false) {
          if (graph.isEdge(currentSemesterCourse,courseId) == true) {
            connectivity = true
          }
        }

        if (connectivity == true) {
          //can't add course in current semester
          //new semester starts
          currentSemesterCourses.clear()
          currentSemesterCourses.add(courseId)
          semesters = semesters + 1
        }else {
          currentSemesterCourses.add(courseId) //we can add in current semester
        }


      }

      semesters
    }else {
      -1
    }

  }
}