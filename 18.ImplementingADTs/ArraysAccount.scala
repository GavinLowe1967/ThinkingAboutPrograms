package thinkingAboutPrograms.account

/** Representing the phone book using a pair of arrays. */
class ArraysAccount extends Account{
  private val Max = 1000 // max number of names we can store
  private val names = new Array[String](Max)
  private val balances = new Array[Int](Max)
  private var count = 0
  /* These variables together represent the mapping
   * { names(i) -> balances(i) | i <- [0..count) }
   * DTI: count <= Max && the entries in names[0..count) are distinct. */

  /** Return the index i < count s.t. names(i) = name; or return count if no
    * such index exists. */
  private def find(name: String): Int = {
    // Invariant: name not in names[0..i) && i <= count
    var i = 0
    while(i < count && names(i) != name) i += 1
    i
  }  

  /** Create an account for name, with zero balance. */
  def create(name: String): Unit = {
    require(find(name) == count)
    require(count < Max, "No room for additional account.")
    names(count) = name; balances(count) = 0; count += 1
  }

  /** Get the balance for name. */
  def lookup(name: String): Int = {
    val i = find(name); require(i < count); balances(i)
  }

  /** Credit name's account with amount. */
  def credit(name: String, amount: Int): Unit = {
    val i = find(name); require(i < count); balances(i) += amount
  }

  /** Debit name's account by amount. */
  def debit(name: String, amount: Int): Unit = {
    val i = find(name); require(i < count); balances(i) -= amount
  }

  /** Does this contain an account for name? */
  def contains(name: String): Boolean = find(name) < count

  /** Delete the account for name. */
  def delete(name: String): Unit = {
    val i = find(name); require(i < count)
    // Overwrite with entry in position count-1
    names(i) = names(count-1); balances(i) = balances(count-1); count -= 1
  }
}
