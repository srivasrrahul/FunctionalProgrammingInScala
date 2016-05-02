import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import BankAccount._

/**
 * Created by rasrivastava on 4/21/16.
 */
class BankActor extends Actor{

  override def receive: Receive = {
    case Deposit(amount) => {
      println("Deposit " + amount)
      balance += amount
      println("Current Balance " + balance)
      sender() ! Done
    }

    case Withdraw(amount) if amount <= balance => {
      balance -= amount
      sender() ! Done
    }

    case _ => sender() ! Failed
  }

  var balance = BigInt(0)
}

class TestActor extends Actor {


  val bankaccount = context.actorOf(Props[BankActor], name = "bankaccount")
  bankaccount ! Deposit(100)
  bankaccount ! Deposit(200)
  bankaccount ! Withdraw(300)

  override def receive: Actor.Receive = {
    case Done => {
      println("success")

    }

    case Failed => {
      println("failure")

    }
  }
}
