package oscar.cp.mem.measures

import oscar.cp.mem.pareto.Pareto

object Hypervolume {

  def hypervolume(set: IndexedSeq[(Int, Int)], nadir: (Int, Int) = (0, 0), scale: Int = 1): Double = { 
    val sortedSet = set.sortBy(_._1)
    var prevObj1 = nadir._1 
    var volume = 0.0 
    for (s <- sortedSet) {
      val dObj1 = s._1 - prevObj1
      val dObj2 = (s._2-nadir._2)
      val v = (dObj1.toDouble/scale) * (dObj2.toDouble/scale)
      prevObj1 = s._1
      volume += (if (v > 0) v else 0)
    }
    volume
  }
  
  def hypervolumeMin(set: IndexedSeq[(Int, Int)], ref: (Int, Int), scale: Int = 1): Double = { 
    val (r1, r2) = ref
    val sortedSet = set.sortBy(_._1)
    var prevObj2 = r2   
    var volume = 0.0 
    for (s <- sortedSet) {
      val (obj1, obj2) = s
      val dObj1 = (r1 - obj1).abs
      val dObj2 = (prevObj2 - obj2).abs
      val v = ((dObj1.toDouble / scale) * (dObj2.toDouble / scale)).abs
      prevObj2 = obj2
      volume += v
    }
    volume
  }
  
  def coverage(set: Iterable[(Int, Int)], refSet: Iterable[(Int, Int)], max: Boolean = true): Int = {
    var nDom = 0
    for (s <- set) {
      if (refSet.exists(r => if (max) dominateMax(r, s) else dominateMin(r, s))) {
        nDom += 1
      }
    }
    nDom
  }
  
  
  
  def dominateMin(s1: (Int, Int), s2: (Int, Int)): Boolean = {
    val (x1, y1) = s1
    val (x2, y2) = s2    
    if (x1 < x2 && y1 <= y2) true
    else if (x1 <= x2 && y1 < y2) true
    else false
  }
  
  def dominateMax(s1: (Int, Int), s2: (Int, Int)): Boolean = {
    val (x1, y1) = s1
    val (x2, y2) = s2    
    if (x1 > x2 && y1 >= y2) true
    else if (x1 >= x2 && y1 > y2) true
    else false
  }
  
  def main(args: Array[String]) {
    
    val ref = (4, 4)
    val p1 = (1, 3)
    val p2 = (3, 2)
    
    val set = Array(p1, p2)
    
    println (hypervolumeMin(set, ref))
  }
}


