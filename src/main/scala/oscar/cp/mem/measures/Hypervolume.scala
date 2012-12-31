package oscar.cp.mem.measures

object Hypervolume {

  def hypervolumeNadir(set: Array[(Int, Int)], nadir: (Int, Int), V: (Double, Double)): Double = {
    var prevObj2 = nadir._2
    var volume = 0.0
    for (i <- 0 until set.size) {
      val dObj2 = prevObj2 - set(i)._2
      val dObj1 = nadir._1 - set(i)._1
      prevObj2 = set(i)._2
      volume += ((dObj2 * dObj1))
    }
    volume
  }
  
  def join(set : Array[(Int, Int)], ref : Array[(Int, Int)]): Array[(Int, Int)] = {  
    var joinSet = set
    for (r <- ref) joinSet = joinSet.filter(s => s._1 >= r._1 && s._2 >= r._2)
    joinSet = joinSet union ref
    return joinSet
  }

  def main(args: Array[String]) {
    
    val rhSet1 = (SetParser.parseSet("setPointAB10Bunch.txt")).sortBy(_._1)  
    val rhSet2 = (SetParser.parseSet("setPointAB15c2.txt")).sortBy(_._1) 
    val rhSet3 = (SetParser.parseSet("setPointAB15c3.txt")).sortBy(_._1) 
    
    val nadir = (180000, 180000)
    val V = (180000.0, 180000.0)

    println("rh\t : " + hypervolumeNadir(rhSet1, nadir, V)/100000000)   
    println("rh\t : " + hypervolumeNadir(rhSet2, nadir, V)/100000000)
    println("rh\t : " + hypervolumeNadir(rhSet3, nadir, V)/100000000)
  }
}

