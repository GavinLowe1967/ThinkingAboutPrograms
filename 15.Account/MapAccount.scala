package thinkingAboutPrograms.account

/** An implementation of Account, using a Map internally. */
class MapAccount extends Account{
  private val map = new scala.collection.mutable.HashMap[Name, Amount]

  /** Create an account for name, with zero balance.
    * pre: name not in dom account
    * post account = account_0 union {name -> 0}. */
  def create(name: Name) = {
    require(!map.contains(name)); map += name -> 0
  }

  /** Get the balance for name.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def lookup(name: Name): Amount = {
    require(map.contains(name)); map(name)
  }

  /** Credit name's account with amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) + amount}. */
  def credit(name: Name, amount: Amount) = {
    require(map.contains(name))
    val oldBal = map(name); map += name -> (oldBal+amount)
  }

  /** Debit name's account by amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) - amount}. */
  def debit(name: Name, amount: Amount) = {
    require(map.contains(name))
    val oldBal = map(name); map += name -> (oldBal-amount)
  }

  /** Does this contain an account for name?
    * post: account = account_0 && returns (name in dom account). */
  def contains(name: Name): Boolean = map.contains(name)

  /** Delete the account for name. 
    * pre: name in dom account
    * post: account = account_0 - {name -> account_0(name)}.
    * This is an optional operation, not provided by all implementations. */
  def delete(name: Name) = {
    require(map.contains(name)); map.remove(name)
  }
}
