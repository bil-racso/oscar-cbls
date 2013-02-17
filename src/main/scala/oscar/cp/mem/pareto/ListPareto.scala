package oscar.cp.mem.pareto

class ListPareto[Sol](val nObjs: Int) extends Pareto[Sol] {
  
  private var sols: List[MOSol[Sol]] = List()

  
  /** Access to the nadir point
   * 
   */ 
  var nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue) 
  
  /** Access to the ideal point
   * 
   */ 
  var ideal: Array[Int] = Array.fill(nObjs)(Int.MinValue)

  /** Insert a new solution in the pareto front
   *  The result is the number of dominated solution
   *  -1 if the solution is dominated
   */
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
  
  
  /** Return a solution which dominates sol 
   *  None if it does not exist
   * 
   */
  def getDominant(sol: Array[Int]): Option[MOSol[Sol]] = {
    val dummySol = MOSol(null, sol)
    for (s <- sols if s dominates dummySol) return Some(s)
    None
  }
    
  /** The number of solution in the pareto front
   * 
   */
  def size: Int = sols.size
  
  /** Maps the pareto front into a list according to the mapping function f
   * 
   */
  def map[B](f: (MOSol[Sol]) => B): List[B] = sols.map(f)
  
  /** Maps the pareto front into a string in which each solution is separated by s
   * 
   */
  def mkString(s: String): String = sols.mkString(s)
  
  /** Applies the function f to each solution in the pareto front
   * 
   */
  def foreach[B](f: (MOSol[Sol]) => B): Unit = sols.foreach(f)
  
  /** Removes all the solution in the pareto front. It is thus empty
   * 
   */
  def removeAll(): Unit = {sols = List()}
  
  /** Returns the list of solution satisfying the condition f
   * 
   */
  def filter(f: (MOSol[Sol]) => Boolean): List[MOSol[Sol]] = sols.filter(f)
  
  /** Maps the pareto front into a list 
   *  no assumption on the order of the element in the list
   * 
   */
  def toList: List[MOSol[Sol]] = sols
  
  /** Returns the minimal solution, according to f, in the pareto front
   * 
   */
  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    var min = sols.head
    var minVal = f(min)
    for (s <- sols.tail) {
      val v = f(s)
      if (v < minVal) {
        minVal = v
        min = s
      }
    }
    min    
  }
  
  /** Maps the pareto front into a list in which the solutions are sorted according to f
   * 
   */
  def sortBy(f: (MOSol[Sol]) => Int): List[MOSol[Sol]] = sols.sortBy(f)
  
  /** Maps the pareto front into a list in which the solutions are sorted according to
   *  the value of the objective obj
   */
  def sortByObj(obj: Int): List[MOSol[Sol]] = sols
}