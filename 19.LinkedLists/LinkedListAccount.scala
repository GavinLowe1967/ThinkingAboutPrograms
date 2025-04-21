package thinkingAboutPrograms.account

/** Representing the phone book using a linked list. */
class LinkedListAccount extends Account{
  /** The type of nodes from which the linked list is constructed. */
  private class Node(val name: Name, val next: Node){
    var balance = 0
  }

  /** The linked list storing the account. */
  private var list: Node = null
  // list represents the mapping composed of (n.name -> n.balance) maplets,
  // when n is a node reached by following 0 or more next references.

  /** Return the Node that matches name, or null if no such Node exists. */
  private def find(name: Name): Node = {
    var n = list
    while(n != null && n.name != name) n = n.next
    n
  }

  /** Create an account for name, with zero balance.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def create(name: Name) = {
    require(find(name) == null); list = new Node(name, list)
  }

  /** Get the balance for name.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def lookup(name: Name): Amount = {
    val n = find(name); require(n != null); n.balance
  }

  /** Credit name's account with amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) + amount}. */
  def credit(name: Name, amount: Amount) = {
    val n = find(name); require(n != null); n.balance += amount
  }

  /** Debit name's account by amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) - amount}. */
  def debit(name: Name, amount: Amount) = {
    val n = find(name); require(n != null); n.balance -= amount
  }

  /** Does this contain an account for name?
    * post: account = account_0 && returns (name in dom account). */
  def contains(name: Name): Boolean = find(name) != null

  /** Delete the account for name. 
    * pre: name in dom account
    * post: account = account_0 - {name -> account_0(name)}.
    *  Not implemented in this version. */
  def delete(name: Name) = ???
}


