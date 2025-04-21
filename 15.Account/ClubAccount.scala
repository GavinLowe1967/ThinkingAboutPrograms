package thinkingAboutPrograms.account

import scala.io.StdIn.{readLine,readInt}

/** Main program for the club account example. */
object ClubAccount{
  def main(args: Array[String]) = {
    // Create the Account object, based on the command line argument (if any). 
    val account: Account = 
      if(args.isEmpty) new MapAccount
      else args(0) match{
        case "--map" => new MapAccount
        case "--arrays" => new ArraysAccount
        case "--linkedlist" => new LinkedListAccount
        case "--linkedlistheader" => new LinkedListHeaderAccount
      }
    var done = false // are we finished yet?

    while(!done){
      val cmd = readLine("create, get, credit, debit, delete or quit? ")
      cmd match{
        case "create" =>
          val name = readLine("Name to create? ")
          if(account.contains(name)) println(name+" already has an account.")
          else account.create(name) 
        case "get" => 
          val name = readLine("Name to get? ")
          if(account.contains(name)) println("Balance is "+account.lookup(name))
          else println(name+" not found")
        case "credit" => 
          val name = readLine("Name to credit? ")
          print("Amount to credit? "); val amount = readInt()
          if(account.contains(name)) account.credit(name,amount)
          else println(name+" not found")
        case "debit" => 
          val name = scala.io.StdIn.readLine("Name to debit? ")
          print("Amount to debit? "); val amount = readInt()
          if(account.contains(name)) account.debit(name,amount)
          else println(name+" not found")
        case "quit" => done = true
        case "delete" =>  
          val name = readLine("Name to delete? ")
          if(account.contains(name)) account.delete(name)
          else println(name+" not found")
        case _ => 
          println("Please type `create', `get', `credit', `debit', `delete' or `quit'.")
      } // end of match
    } // end of while
  }
}
