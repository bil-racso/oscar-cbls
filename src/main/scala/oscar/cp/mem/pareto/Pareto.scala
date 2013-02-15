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
  
  /** Return true if the solution is dominated false otherwise
   * 
   */
  def isDominated(point: Array[Int]): Boolean
    
  /** The number of solution in the pareto front
   * 
   */
  def size: Int
  
  /** Is the pareto front empty
   * 
   */
  def isEmpty: Boolean = (size == 0)
  
  /** Maps the pareto front into a list according to the mapping function f
   * 
   */
  def map[B](f: (MOSol[Sol]) => B): List[B]
  
  /** Maps the pareto front into a string in which each solution is separated by s
   * 
   */
  def mkString(s: String): String
  
  /** Applies the function f to each solution in the pareto front
   * 
   */
  def foreach[B](f: (MOSol[Sol]) => B): Unit
  
  /** Removes all the solution in the pareto front. It is thus empty
   * 
   */
  def removeAll(): Unit
  
  /** Returns the list of solution satisfying the condition f
   * 
   */
  def filter(f: (MOSol[Sol]) => Boolean): List[MOSol[Sol]]
  
  /** Maps the pareto front into a list 
   *  no assumption on the order of the element in the list
   * 
   */
  def toList: List[MOSol[Sol]]
  
  /** Returns the minimal solution, according to f, in the pareto front
   * 
   */
  def min(f: (MOSol[Sol]) => Int): MOSol[Sol]
  
  /** Returns the maximal solution, according to f, in the pareto front
   * 
   */
  def max(f: (MOSol[Sol]) => Int): MOSol[Sol] = min(-f(_))  
  
  /** Maps the pareto front into a list in which the solutions are sorted according to f
   * 
   */
  def sortBy(f: (MOSol[Sol]) => Int): List[MOSol[Sol]]
  
  /** Maps the pareto front into a list in which the solutions are sorted according to
   *  the value of the objective obj
   */
  def sortByObj(obj: Int): List[MOSol[Sol]]
}