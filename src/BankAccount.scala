/**
 * Created by Rahul on 4/21/16.
 */
object BankAccount {
  case class Deposit(amount : BigInt) {
    require(amount > 0)
  }

  case class Withdraw(amount : BigInt) {
    require(amount>0)
  }

  case object Done
  case object Failed

}
