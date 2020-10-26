import scala.collection.mutable

trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Point(val x : Int,val y : Int)
case class State(val direction: Direction,val point: Point)

object Solution {
  def isRobotBounded(instructions: String): Boolean = {
    def stateHandler(event : String,state: State) : State = {
      event match {
        case "G" => {
          state.direction match {
            case North => {
              new State(North,new Point(state.point.x,state.point.y+1))
            }
            case South => {
              new State(South,new Point(state.point.x,state.point.y-1))
            }
            case East => {
              new State(East,new Point(state.point.x+1,state.point.y))
            }
            case West => {
              new State(West,new Point(state.point.x-1,state.point.y))
            }
          }
        }
        case "L" => {
          state.direction match {
            case North => {
              new State(West, state.point)
            }
            case South => {
              new State(East, state.point)
            }
            case East => {
              new State(North, state.point)
            }
            case West => {
              new State(South, state.point)
            }
          }
        }
        case _ => {
          state.direction match {
            case North => {
              new State(East, state.point)
            }
            case South => {
              new State(West, state.point)
            }
            case East => {
              new State(South, state.point)
            }
            case West => {
              new State(North, state.point)
            }
          }
        }
      }
    }

    val visited = new mutable.HashSet[State]()

    var state = new State(North,new Point(0,0))

    visited.add(state)

    for (instruction <- instructions) {
      val newState = stateHandler(instruction.toString,state)


      state = newState
    }

    //println(state)

    var found = false
    state.direction match {
      case North => {
        if (state.point == new Point(0,0)) {
          found = true
        }
      }
      case _ => {
        found = true
      }
    }

    found

  }
}