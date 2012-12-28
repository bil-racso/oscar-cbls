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
    
    val fullSets = (SetParser.parseMultiSet("ListNDS_2PPLS_Kroab100_2.txt", 20).map(_.sortBy(_._1)))
    val refSet = (SetParser.parseSet("refAB100.txt")).sortBy(_._1)  
    
    val nadir = (180000, 180000)//(refSet.last._1, refSet.head._2)
    val ideal = (refSet.head._1, refSet.last._2)
    val V = (180000.0, 180000.0)
    
    val fullH = for (i <- 0 until fullSets.size) yield {      
      hypervolumeNadir(fullSets(i), nadir, V)
    }
    
    println(hypervolumeNadir(refSet, nadir, V))
    println(fullH.sum/fullH.size)  
  }
}

