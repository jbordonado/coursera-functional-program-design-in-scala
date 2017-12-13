import week3.{BankAccount, BankAccountProxy}

object account {
  val acct = new BankAccount
  acct deposit 50
  acct withdraw 20
  acct withdraw 20

  val acctProxy = new BankAccountProxy(acct)
  acctProxy deposit 100
  acctProxy withdraw 50
}