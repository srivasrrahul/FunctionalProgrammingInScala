import scala.collection.mutable
import scala.util.control.Breaks._

case class TrieChar(val char : Char,var isTerminated : Boolean,val next : mutable.HashMap[Char,TrieChar])
class Trie() {

  /** Initialize your data structure here. */
  val root = new mutable.HashMap[Char,TrieChar]()

  /** Inserts a word into the trie. */
  def insert(word: String) : Unit = {
    root.get(word(0)) match {
      case None => {
        val trieChar = new TrieChar(word(0),false,new mutable.HashMap[Char,TrieChar]())
        root += ((word(0),trieChar))
      }
      case _ => {

      }
    }

    if (word.length == 1) {
      root.get(word(0)).get.isTerminated = true
    }else {
      var current = root.get(word(0)).get

      var j = 1

      while (j < word.length) {
        current.next.get(word(j)) match {
          case None => {
            val trieChar = new TrieChar(word(j),false,new mutable.HashMap[Char,TrieChar]())
            if (j == word.length-1) {
              trieChar.isTerminated = true
            }

            current.next += ((word(j),trieChar))
            current = trieChar
          }
          case Some(nextTrieChar) => {
            if (j == word.length-1) {
              nextTrieChar.isTerminated = true
            }
            current = nextTrieChar
          }
        }

        j = j + 1
      }
    }
  }

  /** Returns if the word is in the trie. */
  def search(word: String): Boolean = {
    val first = root.get(word(0))

    first match {
      case None => false
      case Some(trieChar) => {
        if (word.length == 1) {
          if (trieChar.isTerminated) {
            true
          }else {
            false
          }
        }else {
          var j = 1

          var retValue = false
          var current = trieChar
          breakable {
            while (j < word.length) {
              val next = current.next.get(word(j))
              next match {
                case None => {
                  retValue = false
                  break
                }
                case Some(nextTrieChar) => {
                  if (j == word.length - 1) {
                    if (nextTrieChar.isTerminated == false) {
                      retValue = false
                      break
                    } else {
                      retValue = true
                      break
                    }
                  } else {
                    current = nextTrieChar
                  }
                }
              }

              j = j + 1
            }
          }

          retValue
        }
      }
    }
  }

  /** Returns if there is any word in the trie that starts with the given prefix. */
  def startsWith(word: String): Boolean = {
    val first = root.get(word(0))

    first match {
      case None => false
      case Some(trieChar) => {
        var j = 1

        var retValue = true
        var current = trieChar
        breakable {
          while (j < word.length) {
            val next = current.next.get(word(j))
            next match {
              case None => {
                retValue = false
                break
              }
              case Some(nextTrieChar) => {
                current = nextTrieChar
              }
            }

            j = j + 1
          }
        }

        retValue
      }
    }
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val trie = new Trie
    trie.insert("a")
    //trie.insert("ra")
    //trie.insert("a")
    println(trie.search("a"))
    println(trie.search("a"))
    println(trie.startsWith("a"))
  }
}