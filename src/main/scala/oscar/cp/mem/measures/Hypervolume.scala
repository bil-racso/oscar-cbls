package oscar.cp.mem.measures

import oscar.cp.mem.pareto.Pareto

object Hypervolume {

  def hypervolume[Sol](set: Pareto[Sol]): Double = {
    var prevObj2 = set.nadir(1)
    var volume = 0.0
    val sortedSet = set.sortByObj(0)
    for (s <- sortedSet) {
      val obj1 = s(0)
      val obj2 = s(1)
      val dObj2 = prevObj2 - obj2
      val dObj1 = set.nadir(0) - obj1
      prevObj2 = obj2
      val v = ((dObj2.toDouble / 10000) * (dObj1.toDouble / 10000))
      if (v > 0) volume += v
    }
    volume
  }
}

