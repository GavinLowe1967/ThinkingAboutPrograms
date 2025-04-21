package thinkingAboutPrograms.account
import scala.util.Random

object AccountTest{
  var iters = 1000 // number of iterations per run
  var reps = 100000 // number of repetitions

  /** Names to use for the tests. */
  val names: Array[String] = 
    scala.io.Source.fromFile("names.txt").getLines().toArray

  /** Number of names. */
  val numNames = names.length

  /** Do a single test run on account. */
  def doTest(account: Account) = {
    // Do we allow deletion?  Not on LinkedListAccount
    val allowDelete = !account.isInstanceOf[LinkedListAccount]

    // A map, with which we compare results. 
    val map = new scala.collection.mutable.HashMap[String, Int]

    for(i <- 0 until iters){
      val name = names(Random.nextInt(numNames))
      val isIn = account.contains(name)
      assert(isIn == map.contains(name))
      if(isIn){
        assert(account.lookup(name) == map(name))
        Random.nextInt(3) match {
          case 0 => // credit
            val amount = Random.nextInt(1000)
            account.credit(name, amount); map(name) = map(name)+amount
          case 1 => // debit
            val amount = Random.nextInt(1000)
            account.debit(name, amount); map(name) = map(name)-amount
          case 2 => // delete
            if(allowDelete){ account.delete(name); map -= name }
        }
      }
      else{ // create  
        account.create(name); map += name -> 0 
      }
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments 
    var accountType = "map"
    var i = 0
    while(i < args.length){
      args(i) match{
        case "--map" => accountType = "map"; i += 1
        case "--arrays" => accountType = "arrays"; i += 1
        case "--linkedlist" => accountType = "linkedlist"; i += 1
        case "--linkedlistheader" => accountType = "linkedlistheader"; i += 1
        case "--reps" => reps = args(i+1).toInt; i += 2
        case "--iters" => iters = args(i+1).toInt; i += 2
      }
    }

    for(i <- 0 until reps){
      val account: Account = accountType match{
        case "map" => new MapAccount
        case "arrays" => new ArraysAccount
        case "linkedlist" => new LinkedListAccount
        case "linkedlistheader" => new LinkedListHeaderAccount
      }
      doTest(account)
      if(i%1000 == 0) print(".")
    }
    println()
  }

}

