package thinkingAboutPrograms.binaryTrees

/** Some simple tests on the maps. */
object TreeMapTest{
  def main(args: Array[String]) = {
    val keys = List(
      "one", "two", "three", "four", "five", "six", "seven",
      "eight", "nine", "ten")
    val pairs = keys.zip(1 to (keys.length))
    // We'll call recursive versions on mapRec, and iterative versions on map.
    val map = new BinaryTreeMap[String, Int]
    val mapRec = new BinaryTreeMapRec[String, Int]
    // Add elements of pairs
    for((st, n) <- pairs){ mapRec.update(st, n); map.update(st, n) }
    map.print; println(); mapRec.print
    // Check membership
    for((st, n) <- pairs)
      assert(mapRec.get(st) == Some(n) && map.get(st) == Some(n))
    assert(map.get("zero") == None && map.get("eleven") == None)
    assert(mapRec.get("zero") == None && mapRec.get("eleven") == None)
    // Delete
    assert(mapRec.remove("one") && mapRec.remove("seven") &&
      mapRec.remove("three"))
    assert(!mapRec.remove("zero") && !mapRec.remove("eleven") &&
      !mapRec.remove("seven"))
    assert(map.remove("one") && map.remove("seven") && map.remove("three"))
    assert(!map.remove("zero") && !map.remove("eleven") &&
      !map.remove("seven"))
    println(); mapRec.print; println(); map.print
  }

}
