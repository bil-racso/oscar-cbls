package oscar.cp.mem.measures

object Hypervolume {

  def hypervolumeNadir(set: Array[(Int, Int)], nadir: (Int, Int)): Double = {
    var prevObj2 = nadir._2
    var volume = 0.0
    for ((obj1, obj2) <- set) {
      val dObj2 = prevObj2 - obj2
      val dObj1 = nadir._1 - obj1
      prevObj2 = obj2
      val v = (dObj2 * dObj1).toDouble / 100000000
      if (v > 0) volume += v
    }
    volume
  }

  def main(args: Array[String]) {

    val rhSet1 = (SetParser.parseSet("setPointABtest.txt")).sortBy(_._1)
    val rhSet2 = (SetParser.parseSet("setPointACtest.txt")).sortBy(_._1)
    val rhSet3 = (SetParser.parseSet("setPointADtest.txt")).sortBy(_._1)
    val rhSet4 = (SetParser.parseSet("setPointBCtest.txt")).sortBy(_._1)
    val rhSet5 = (SetParser.parseSet("setPointBDtest.txt")).sortBy(_._1)
    val rhSet6 = (SetParser.parseSet("setPointCDtest.txt")).sortBy(_._1)

    val nadir = (180000, 180000)

    println("rh\t : " + hypervolumeNadir(rhSet1, nadir))
    println("rh\t : " + hypervolumeNadir(rhSet2, nadir))
    println("rh\t : " + hypervolumeNadir(rhSet3, nadir))
    println("rh\t : " + hypervolumeNadir(rhSet4, nadir))
    println("rh\t : " + hypervolumeNadir(rhSet5, nadir))
    println("rh\t : " + hypervolumeNadir(rhSet6, nadir))
  }
}

