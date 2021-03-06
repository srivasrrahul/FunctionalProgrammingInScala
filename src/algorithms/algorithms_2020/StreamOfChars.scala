import scala.collection.mutable
import scala.collection.mutable.ListBuffer
trait Node {

}
class TrieNode(val currentChar : Char) extends Node {
  val next = new mutable.HashMap[Char,TrieNode]()
  var isLeaf = false
  def addChar(char: Char) : TrieNode = {
    val nextNode = next.getOrElseUpdate(char,new TrieNode(char))
    nextNode
  }
  def getChar(char: Char) : TrieNode = {
    next.getOrElse(char,null) //null indicates no element
  }

  def setLeaf() : Unit = {
    isLeaf = true
  }

  def getLeaf() : Boolean = {
    isLeaf
  }
}
class RootNode extends Node {
  //There is no current value
  val next = new mutable.HashMap[Char,TrieNode]()
  def addChar(char: Char) : TrieNode = {
    val nextNode = next.getOrElseUpdate(char,new TrieNode(char))
    nextNode
  }
  def getChar(char: Char) : TrieNode = {
    next.getOrElse(char,null) //null indicates no element
  }
  def addString(key: String) : Unit = {
    var current : TrieNode = addChar(key(0))
    for (j <- 1 to key.length-1) {
      current = current.addChar(key(j))
    }

    //println(current)
    current.setLeaf()
  }

  def checkString(key : String) : Boolean = {
    var current : TrieNode = getChar(key(0))
    for (j <- 1 to key.length-1 if current != null) {
      current = current.getChar(key(j))
    }

    if (current != null) {
      current.isLeaf
    }else {
      false
    }
  }

  def checkString(key : List[Char]) : Boolean = {
    var current : TrieNode = getChar(key.head)
    //println(current)
    var retValue = false
    if (current != null) {
      retValue = current.getLeaf()
    }
    for (k <- key.tail if current != null && retValue == false) {
      //println("checking " + k)
      current = current.getChar(k)
      if (current != null) {
        retValue = current.getLeaf()
      }
    }

    retValue
//    if (current != null) {
//      //println(current.getLeaf())
//      current.getLeaf()
//    }else {
//      false
//    }
  }
}
class StreamChecker(_words: Array[String]) {
  val root = new RootNode
  def formTrie() : Unit = {
    for (word <- _words) {
      root.addString(word.reverse)
    }
  }

  formTrie()
  var lst : List[Char] = List()
  def query(letter: Char): Boolean = {
    lst = letter :: lst
    root.checkString(lst)
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val streamChecker = new StreamChecker(Array("cd","f","kl"))
    println(streamChecker.query('a'))
    println(streamChecker.query('d'))
    println(streamChecker.query('c'))
    println(streamChecker.query('d'))
    println(streamChecker.query('e'))
    println(streamChecker.query('f'))
    println(streamChecker.query('g'))
    println(streamChecker.query('h'))
    println(streamChecker.query('i'))
    println(streamChecker.query('j'))
    println(streamChecker.query('k'))
    println(streamChecker.query('l'))
  }
}