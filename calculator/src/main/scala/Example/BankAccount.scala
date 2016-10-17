package Example

import calculator.Var

/**
  * Created by baohg on 17/10/2016.
  */
class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    if (amount < 0 && amount < balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("ABCXYZ")
  }
}

