package oscar.cp.mem.pareto

abstract class Pareto[Sol] {
  
  /** Returns the number of objectives
   * 
   */
  def nObjs: Int
  
  /** Returns a range over the objectives
   * 
   */
  val Objs = 0 until nObjs
  
  /** Access to the nadir point
   * 
   */
  def nadir: Array[Int]
  
  /** Access to the ideal point
   * 
   */
  def ideal: Array[Int]

  /** Insert a new solution in the pareto front
   *  The result is the number of dominated solution
   *  -1 if the solution is dominated
   */
  def insert(sol: MOSol[Sol]): Int
    
  /** Removes all the solution in the pareto front. It is thus empty
   * 
   */
  def removeAll(): Unit
  
  /** Return a solution which dominates sol 
   *  None if it does not exist
   * 
   */
  def getDominant(sol: Array[Int]): Option[MOSol[Sol]]

  /** Return true if the solution is dominated false otherwise
   * 
   */
  def isDominated(sol: Array[Int]): Boolean = getDominant(sol).isDefined
    
  /** The number of solution in the pareto front
   * 
   */
  def size: Int
  
  /** Is the pareto front empty
   * 
   */
  def isEmpty: Boolean = (size == 0)
     
  /** Applies the function f to each solution in the pareto front
   * 
   */
  def foreach[B](f: (MOSol[Sol]) => B): Unit
  
  /** Maps the pareto front into a list according to the mapping function f
   * 
   */
  def map[B](f: (MOSol[Sol]) => B): List[B] = {
    var mapping = List[B]()
    this.foreach(s => mapping = f(s) :: mapping)
    mapping
  }
  
  /** Maps the pareto front into a string in which each solution is separated by s
   * 
   */
  def mkString(str: String): String = {
    var string = ""
    var n = this.size
    this.foreach(s => {
      if (n > 1) string += (s + str)
      else string += s
    })
    string
  }
  
  /** Returns the list of solution satisfying the condition f
   * 
   */
  def filter(f: (MOSol[Sol]) => Boolean): List[MOSol[Sol]] = {
    var filtered = List[MOSol[Sol]]()
    this.foreach(s => if (f(s)) filtered = s :: filtered)
    filtered
  }
  
  /** Maps the pareto front into a list 
   *  no assumption on the order of the element in the list
   * 
   */
  def toList: List[MOSol[Sol]] = {
    var list = List[MOSol[Sol]]()
    this.foreach(s => list = s :: list)
    list
  }
  
  /** Returns the minimal solution, according to f, in the pareto front
   * 
   */
  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    if (this.isEmpty) throw new NoSuchElementException("empty set")
    else {
      var min: MOSol[Sol] = null
      var minVal = Int.MaxValue
      this.foreach(sol => {
        val v = f(sol)
        if (v < minVal) {
          minVal = v
          min = sol
        }
      })
      min
    }
  }
  
  /** Returns the maximal solution, according to f, in the pareto front
   * 
   */
  def max(f: (MOSol[Sol]) => Int): MOSol[Sol] = min(-f(_))  
  
  /** Maps the pareto front into a list in which the solutions are sorted according to f
   * 
   */
  def sortBy(f: (MOSol[Sol]) => Int): List[MOSol[Sol]] = this.toList.sortBy(f)
  
  /** Maps the pareto front into a list in which the solutions are sorted according to
   *  the value of the objective obj
   */
  def sortByObj(obj: Int): List[MOSol[Sol]]
}