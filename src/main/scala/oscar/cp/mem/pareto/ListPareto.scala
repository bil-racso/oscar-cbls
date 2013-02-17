package oscar.cp.mem.pareto

class ListPareto[Sol](val nObjs: Int) extends Pareto[Sol] {
  
  private var sols: List[MOSol[Sol]] = List()

  var nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue) 
  var ideal: Array[Int] = Array.fill(nObjs)(Int.MinValue)

  def insert(sol: MOSol[Sol]): Int = { 
    if (sols.exists(_ dominates sol)) -1
    else {     
      var newList: List[MOSol[Sol]] = List()
      for (s <- sols if !sol.dominates(s)) newList = s::newList 
      val nRemoved = sols.size - newList.size
      sols = sol :: newList
      nRemoved
    }
  }
  
  def getDominant(sol: Array[Int]): Option[MOSol[Sol]] = {
    val dummySol = MOSol(null, sol)
    for (s <- sols if s dominates dummySol) return Some(s)
    None
  }

  def size: Int = sols.size

  def foreach[B](f: (MOSol[Sol]) => B): Unit = sols.foreach(f)

  def removeAll(): Unit = { sols = List() }

  def sortByObj(obj: Int): List[MOSol[Sol]] = sols
}