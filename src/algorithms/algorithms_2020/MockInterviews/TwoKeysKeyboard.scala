import scala.collection.mutable

case class State(val buffer : Int,val notePad : Int) {
  def copy() : State = {
    //copy all in notePad
    val newBuffer = notePad
    new State(newBuffer,notePad)
  }

  def paste() : State = {
    new State(buffer,notePad+buffer)
  }
}
object Solution {
  def minSteps(n: Int): Int = {

    val cache = new mutable.HashMap[State,Int]()
    def itr(state: State,visited : Set[State]) : Int = {
      //println(state)
      if (state.notePad == n) {
        0
      }else {
        if (state.notePad < n && state.buffer < n && visited.contains(state) == false) {
          if (cache.contains(state)) {
            //println("cache hit")
            cache.get(state).get
          }else {
            val retValue = math.min(itr(state.copy(), visited.+(state)), itr(state.paste(), visited.+(state)))
            if (retValue != Int.MaxValue) {
              cache += ((state,retValue+1))
              retValue + 1
            } else {
              cache += ((state,retValue))
              retValue
            }
          }
        }else {
          Int.MaxValue
        }

      }
    }

    itr(new State(0,1),Set())
  }

  def main(args: Array[String]): Unit = {
    println(minSteps(3))
  }
}