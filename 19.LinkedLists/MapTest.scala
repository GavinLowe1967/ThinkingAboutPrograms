package thinkingAboutPrograms.map

import scala.util.Random

object MapTest{
  val iters = 50000
  val reps = 1000
  val maxKey = 20
  val maxValue = 100

  def main(args: Array[String]) = {

    for(r <- 0 until reps){
      val map1 = new LinkedListHeaderMap[Int, Int]
      val map2 = scala.collection.mutable.Map[Int, Int]()
      for(i <- 0 until iters){
        val r = Random.nextInt(4)
        val key = Random.nextInt(maxKey)
        if(r == 0){ // update both maps
          val value = Random.nextInt(maxValue)
          map1.update(key, value); map2.update(key,value)
        }
        else if(r == 1){ // contains and apply
          if(map1.contains(key))
            assert(map2.contains(key) && map1(key) == map2(key))
          else assert(!map2.contains(key))
        }
        else if(r == 2) // get
          assert(map1.get(key) == map2.get(key))
        else // remove
          assert(map1.remove(key) == map2.remove(key).nonEmpty)
      } // end of inner for loop
      if(r%100 == 0) print(".")
    }
  }


}
