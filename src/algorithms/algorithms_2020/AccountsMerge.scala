import scala.collection.mutable
import scala.collection.mutable.ListBuffer



case class Account(val name : String,val emailAddress : mutable.TreeSet[String])

class Graph(val count : Int) {
  val nodes = new Array[ListBuffer[Int]](count)
  for (j <- 0 to count-1) {
    nodes(j) = new ListBuffer[Int]
  }

  def addEdge(source : Int,dest : Int) : Unit = {

      nodes(source).addOne(dest)
      nodes(dest).addOne(source)


  }

  def print() : Unit = {
    for (j <- 0 to nodes.length-1) {
      var s = "Neibour of " + j + " "

      for (neigbour <- nodes(j)) {
        s = s + "," + neigbour.toString
      }
      println(s)
    }
  }
  def paths() : List[List[Int]] = {
    val visited = new mutable.HashSet[Int]()

    def explore(current : Int,path : ListBuffer[Int]) : Unit = {
      path.addOne(current)
      visited.add(current)
      for (neighbour <- nodes(current)) {
        if (visited.contains(neighbour) == false) {
          explore(neighbour,path)
        }

      }
    }

    val retValue = new ListBuffer[List[Int]]
    for (j <- 0 to count-1) {
      if (visited.contains(j) == false) {
        val path = new ListBuffer[Int]
        explore(j,path)
        retValue.addOne(path.toList)
      }
    }

    retValue.toList
  }
}

object Solution {

  def serializeAccount(account: Account) : List[String] = {
    val lstBuffer = new ListBuffer[String]
    lstBuffer.addOne(account.name)
    lstBuffer.addAll(account.emailAddress)
    lstBuffer.toList
  }

  def mergeSets(emailIdSet : mutable.ArrayBuffer[Account]) : List[List[String]] = {
    val graph = new Graph(emailIdSet.length)
    for (j <- 0 to emailIdSet.length-1) {
      for (k <- j+1 to emailIdSet.length-1) {
        if (emailIdSet(j).emailAddress.intersect(emailIdSet(k).emailAddress).size > 0) {
          graph.addEdge(j,k)
        }
      }
    }

    graph.print()
    val paths = graph.paths()

    val result = new ListBuffer[List[String]]
    for (path <- paths) {
      val commonEmails = new mutable.TreeSet[String]()
      for (node <- path) {
        commonEmails.addAll(emailIdSet(node).emailAddress)
      }
      result.addOne(commonEmails.toList)
    }

    result.toList
  }


  def accountsMerge(accounts: List[List[String]]): List[List[String]] = {
    val accountMap = new mutable.HashMap[String,scala.collection.mutable.ArrayBuffer[Account]]()

    var mergeList = new Array[List[Int]](accounts.length)

    var j = 0
    for (account <- accounts) {
      val emailIds = new mutable.TreeSet[String]()
      emailIds.addAll(account.tail)
      accountMap.get(account.head) match {
        case Some(arrBuffer) => {
          //lstBuffer.addOne(new Account(account.head,emailIds))
          arrBuffer.addOne(new Account(account.head,emailIds))
        }
        case None => {
          val arrBuffer = new scala.collection.mutable.ArrayBuffer[Account]
          arrBuffer.addOne(new Account(account.head,emailIds))

          accountMap += ((account.head,arrBuffer))

        }
      }
    }

    val result = new scala.collection.mutable.ListBuffer[List[String]]
    for (accountM <- accountMap) {
      val sortedEmailSets = mergeSets(accountM._2)
      for (sortedEmailSet <- sortedEmailSets) {
        result.addOne(accountM._1 :: sortedEmailSet)
      }

    }

    result.toList


  }

  def main(args: Array[String]): Unit = {
    val john1 = List("John","johnsmith@mail.com", "john00@mail.com")
    val john2 = List("John","johnnybravo@mail.com")
    val john3 = List("John", "johnsmith@mail.com", "john_newyork@mail.com")
    val mary = List("Mary", "mary@mail.com")

    val d1 = List("D","d0","d1")
    val d2 = List("D","d3","d4")
    val d3 = List("D","d4","d5")
    val d4 = List("D","d2","d3")
    val d5 = List("D","d1","d2")

    //val lst = List(john1,john2,john3,mary)
    val lst = List(d1,d2,d3,d4,d5)

    println(accountsMerge(lst).mkString("\n"))
  }
}