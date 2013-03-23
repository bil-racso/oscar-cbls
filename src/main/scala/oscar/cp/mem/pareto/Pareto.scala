package oscar.cp.mem.pareto

import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.core.CPSol

abstract class Pareto[Sol](protected val maxObj: Array[Boolean]) {
  
  // Inner node structure
  protected case class ParetoSol(objValues: IndexedSeq[Int], sol: Sol)
  
  // Comparison function of each objective
  val isBetter: Array[(Int, Int) => Boolean] = Array.tabulate(maxObj.size)(i => {
    if (maxObj(i)) (_:Int) > (_:Int) else (_:Int) < (_:Int)
  })
  
  var observers: List[ParetoObserver] = List()
  
  def addObserver(obs: ParetoObserver) {
    observers = obs :: observers
  }
  
  def notifyObservers() = observers.foreach(_.update())
  
  /** Returns true if sol1 dominates sol2
   * 
   *  Note: this relation is stronger than the Pareto dominance in the fact that
   *  a solution dominates itself.
   */
  def dominate(sol1: IndexedSeq[Int], sol2: IndexedSeq[Int]): Boolean = dominate0(sol1, sol2, 0)
  
  /** Returns true if sol1 dominates sol2
   * 
   *  Note: this relation is stronger than the Pareto dominance in the fact that
   *  a solution dominates itself.
   */
  def dominate(sol1: ParetoSol, sol2: ParetoSol): Boolean = dominate0(sol1.objValues, sol2.objValues, 0)
  
  // If it exists an objective i such that sol2(i) is better than sol1(i), then 
  // sol1 does not dominate sol2
  private def dominate0(sol1: IndexedSeq[Int], sol2: IndexedSeq[Int], i: Int): Boolean = {
    if (i == nObjs) true
    else if (isBetter(i)(sol2(i), sol1(i))) false
    else dominate0(sol1, sol2, i+1)
  }

  /** Returns the number of objectives
   * 
   */
  def nObjs: Int = maxObj.size
  
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
  def insert(sol: Sol, objValues: Int*): Boolean = insert(sol, objValues.toIndexedSeq)
  
  /** Insert a new solution in the pareto front
   *  The result is the number of dominated solution
   *  -1 if the solution is dominated
   */
  def insert(sol: Sol, objValues: IndexedSeq[Int]): Boolean
    
  /** Removes all the solution in the pareto front. It is thus empty
   * 
   */
  def removeAll(): Unit
  
  /** Return a solution which dominates sol 
   *  None if it does not exist
   * 
   */
  def getDominant(sol: Array[Int]): Option[Sol]

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
  def foreach[B](f: (Sol) => B): Unit
  
  /** Maps the pareto front into a list according to the mapping function f
   * 
   */
  def map[B](f: (Sol) => B): List[B] = {
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
  def filter(f: (Sol) => Boolean): List[Sol] = {
    var filtered = List[Sol]()
    this.foreach(s => if (f(s)) filtered = s :: filtered)
    filtered
  }
  
  /** Maps the pareto front into a list of solutions.
   * 
   *  Note: no assumption on the order of the element in the list
   */
  def toList: List[Sol] = {
    var list = List[Sol]()
    this.foreach(s => list = s :: list)
    list
  }
  
  /** Maps the pareto front into a list of solutions in the objective space.
   *  
   *  Note: no assumption on the order of the element in the list
   */
  def objectiveSols: List[IndexedSeq[Int]]
  
  /** Returns the minimal solution, according to f, in the pareto front
   * 
   */
  def min(f: (Sol) => Int): Option[Sol] = {
    if (this.isEmpty) throw new NoSuchElementException("empty set")
    else {
      var min: Option[Sol] = None
      var minVal = Int.MaxValue
      this.foreach(sol => {
        val v = f(sol)
        if (v < minVal) {
          minVal = v
          min = Some(sol)
        }
      })
      min
    }
  }
  
  /** Returns the maximal solution, according to f, in the pareto front
   * 
   */
  def max(f: (Sol) => Int): Option[Sol] = min(-f(_))  
  
  /** Maps the pareto front into a list in which the solutions are sorted according to f
   * 
   */
  def sortBy(f: (Sol) => Int): List[Sol] = this.toList.sortBy(f)
  
  /** Maps the pareto front into a list in which the solutions are sorted according to
   *  the value of the objective obj
   */
  def sortByObj(obj: Int): List[Sol]
  
  override def equals(otherPareto:Any) = {
    val other = otherPareto.asInstanceOf[Pareto[Any]]
    if (size == other.size) {
      objectiveSols.forall(o => other.objectiveSols.exists(p => p == o))
    } else false
  }
}
