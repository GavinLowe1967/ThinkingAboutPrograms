package thinkingAboutPrograms.account

/** Each implementation of this trait represents a mapping from Names
  * (Strings) to Amounts (Ints), giving a person's balance (in pence).
  * State: account : Name -|-> Amount
  * Init:  account = {}.  */
trait Account{
  /** The type of member's names. */
  type Name = String

  /** The type of a monetary amount, e.g. a balance or the amount of a
    * credit or debit. */
  type Amount = Int

  /** Create an account for name, with zero balance.
    * pre: name not in dom account
    * post account = account_0 union {name -> 0}. */
  def create(name: Name): Unit

  /** Get the balance for name.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def lookup(name: Name): Amount

  /** Credit name's account with amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) + amount}. */
  def credit(name: Name, amount: Amount): Unit

  /** Debit name's account by amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) - amount}. */
  def debit(name: Name, amount: Amount): Unit

  /** Does this contain an account for name?
    * post: account = account_0 && returns (name in dom account). */
  def contains(name: Name): Boolean

  /** Delete the account for name. 
    * pre: name in dom account
    * post: account = account_0 - {name -> account_0(name)}.
    * This is an optional operation, not provided by all implementations. */
  def delete(name: Name): Unit 

}
