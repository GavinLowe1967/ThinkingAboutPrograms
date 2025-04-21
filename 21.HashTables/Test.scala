package thinkingAboutPrograms.hashMap

import scala.util.Random

/** A test for the hash maps. */
object Test{
  // # iterations per run; number of repetitions
  val iters = 40000; val reps = 1000
  // Type of map to test
  var mapType = "HashMap" 

  /** Do a single test. */
  def doTest = {
    // The Map we're testing 
    val map: thinkingAboutPrograms.interfaces.Map[String,Int] = mapType match{
      case "HashMap" => new HashMap[String,Int]
      case "OpenHashMap" => new OpenHashMap[String, Int]
      case "OpenHashMap0" => new OpenHashMap0[String, Int]
      // case "BinaryTree" => 
      //   new thinkingAboutPrograms.binaryTrees.BinaryTreeMap[String,Int]
      // case "BinaryTreeRec" => 
      //   new thinkingAboutPrograms.binaryTrees.BinaryTreeMapRec[String,Int]
    }
    // Specification map
    val sMap = scala.collection.mutable.HashMap[String,Int]()
    for(i <- 0 until iters){
      // We take keys to be strings of length 2, since these produce a fair
      // number of hash collisions.
      val st = Random.nextPrintableChar().toString+Random.nextPrintableChar()
      val choice = Random.nextInt(3)
      if(choice == 0){ 
	// add st -> d to both maps
        val d = Random.nextInt(100)
        map.update(st, d); sMap += (st -> d)
      }
      else if(choice == 1){
	// Check maps agree on st
        assert(map.get(st) == sMap.get(st),
               st+"\t"+map.get(st)+"\t"+sMap.get(st))
	val isIn = map.contains(st)
	assert(isIn == sMap.contains(st))
	if(isIn) assert(map(st) == sMap(st))
      }
      else if(mapType != "OpenHashMap0"){
	// Remove st from both maps
        val od1 = map.remove(st); val od2 = (sMap.remove(st) != None)
        assert(od1 == od2, st+"\t"+od1+"\t"+od2)
      }
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments.
    var i = 0
    while(i < args.length) args(i) match{
      // case "--binaryTree" => mapType = "BinaryTree"; i += 1
      // case "--binaryTreeRec" => mapType = "BinaryTreeRec"; i += 1
      case "--openHashMap" => mapType = "OpenHashMap"; i += 1
      case "--openHashMap0" => mapType = "OpenHashMap0"; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }
    // Run the tests.
    for(r <- 0 until reps){
      doTest; if(r%10 == 0) print(".")
    }
    println()
  }
}


