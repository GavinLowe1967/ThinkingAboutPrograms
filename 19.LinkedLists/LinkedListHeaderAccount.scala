package thinkingAboutPrograms.account

/** Representing the phone book using a linked list. */
class LinkedListHeaderAccount extends Account{
  /** The type of nodes from which the linked list is constructed. */
  private class Node(val name: Name, var next: Node){
    var balance = 0
  }

  /** Dummy header for the linked list storing the account. */
  private var list: Node = new Node("???", null)
  // list represents the mapping composed of (n.name -> n.balance) maplets,
  // when n is a node reached by following 1 or more next references.

  /** Return the Node before the one that matches name, or the last node if no
    * such Node exists. */
  private def find(name: Name): Node = {
    var n = list
    while(n.next != null && n.next.name != name) n = n.next
    n
  }

  /** Create an account for name, with zero balance.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def create(name: Name) = {
    require(find(name).next == null); list.next = new Node(name, list.next)
  }

  /** Get the balance for name.
    * pre: name in dom account
    * post: account = account_0 && returns account(name). */
  def lookup(name: Name): Amount = {
    val n = find(name); require(n.next != null); n.next.balance
  }

  /** Credit name's account with amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) + amount}. */
  def credit(name: Name, amount: Amount) = {
    val n = find(name); require(n.next != null); n.next.balance += amount
  }

  /** Debit name's account by amount.
    * pre: name in dom account
    * post: account = account_0 (+) {name -> account_0(name) - amount}. */
  def debit(name: Name, amount: Amount) = {
    val n = find(name); require(n.next != null); n.next.balance -= amount
  }

  /** Does this contain an account for name?
    * post: account = account_0 && returns (name in dom account). */
  def contains(name: Name): Boolean = find(name).next != null

  /** Delete the account for name. 
    * pre: name in dom account
    * post: account = account_0 - {name -> account_0(name)}. */
  def delete(name: Name) = {
    val n = find(name); require(n.next != null); n.next = n.next.next
  }
}


