import scala.collection.mutable

import scala.util.control.Breaks._
case class TrieChar(val char: Char,var isTerminated: Boolean,val next : mutable.HashMap[Char,TrieChar])
class WordDictionary {
  /** Initialize your data structure here. */

  val rootNode = new mutable.HashMap[Char,TrieChar]()

  /** Adds a word into the data structure. */
  def addWord(word: String): Unit =  {
    var head = rootNode.get(word(0))

    head match {
      case None => {
        val newTrieChar = new TrieChar(word(0),false,new mutable.HashMap[Char,TrieChar]())


        rootNode += ((word(0),newTrieChar))
      }
      case Some(trieChar)=> {


      }
    }

    if (word.length == 1) {
      rootNode.get(word(0)).get.isTerminated = true
    }

    var current = rootNode.get(word(0)).get

    var j = 1
    while (j < word.length) {
      current.next.get(word(j)) match {
        case None => {
          val newTrieChar = new TrieChar(word(j),false,new mutable.HashMap[Char,TrieChar]())
          if (j == word.length-1) {
            newTrieChar.isTerminated = true
          }

          current.next += ((word(j),newTrieChar))
        }
        case Some(trieChar: TrieChar) => {
          if (j == word.length-1) {
            trieChar.isTerminated = true
          }
        }
      }

      current = current.next.get(word(j)).get
      j = j + 1
    }
  }

  /** Returns if the word is in the data structure. A word could contain the dot character '.' to represent any one letter. */
  def search(word: String): Boolean = {
    def exploreTrie(currentIndex : Int,current : TrieChar) : Boolean = {
      if (currentIndex == word.length) {
        current.isTerminated
      }else {
        word(currentIndex) match {
          case '.' => {
            var result = false
            breakable {
              current.next.foreachEntry((ch, nextTrieChar) => {
                result = result | exploreTrie(currentIndex + 1, nextTrieChar)
                if (result) {
                  break
                }
              })
            }

            result
          }
          case _ => {
            val nextTrieChar = current.next.get(word(currentIndex))
            nextTrieChar match {
              case None => false
              case Some(next) => {
                exploreTrie(currentIndex + 1, next)
              }
            }
          }
        }
      }
    }

    word(0) match {
      case '.' => {
        var result = false
        rootNode.foreachEntry((x,trieChar) => {
          result = result | exploreTrie(1,trieChar)
        })
        result
      }
      case _ => {
        rootNode.get(word(0)) match {
          case None => {
            false
          }
          case Some(trieChar) => {
            exploreTrie(1,trieChar)
          }
        }
      }
    }
  }


}

object Solution {
  def main(args: Array[String]): Unit = {
    val dict = new WordDictionary
    dict.addWord("bet")
    dict.addWord("be")
    println(dict.search("...."))
  }
}