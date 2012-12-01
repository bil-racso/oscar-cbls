package oscar.cp.mem.measures

object Hypervolume {

  def hypervolumeNadir(set: Array[(Int, Int)], nadir: (Int, Int)): Double = {
    var prevObj2 = nadir._2
    var volume = 0.0
    for (i <- 0 until set.size) {
      val dObj2 = prevObj2 - set(i)._2
      val dObj1 = nadir._1 - set(i)._1
      prevObj2 = set(i)._2
      volume += (dObj2 * dObj1) 
    }
    (volume/nadir._1)/nadir._2
  }
  
  def join(s1 : Array[(Int, Int)], s2 : Array[(Int, Int)]): Array[(Int, Int)] = ((s1.toSet) union (s2.toSet)).toArray

  def main(args: Array[String]) {

    val s1 = (SetParser.parseSet("kroAB100.txt")).sortBy(_._1)
    val s2 = (SetParser.parseSet("refAB100.txt")).sortBy(_._1)   
    val joinSet = join(s1, s2).sortBy(_._1)
    
    val nadir = (16935934, 16706668)//s2.last._1, s2.head._2)

    val h1 = hypervolumeNadir(s1, nadir)
    val h2 = hypervolumeNadir(s2, nadir)
    val h3 = hypervolumeNadir(joinSet, nadir)

    // Must be : 226.11
    println(nadir)
    println("Hypervolume: " + (h1))
    println("Hypervolume: " + (h2))
    println("Hypervolume: " + (h3))
    println("Hypervolume: " + (h3-h1))
    
  }
}

