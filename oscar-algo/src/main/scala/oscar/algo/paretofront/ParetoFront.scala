package oscar.algo.paretofront

import scala.collection.mutable.Queue

abstract class ParetoFront[U: Numeric, T <: ParetoElement[U]] {
    
  /** Returns true if the Pareto front contains no element; false otherwise */
  def isEmpty: Boolean
  
  /** Attempts to insert an element in the Pareto front and modifies the tree accordingly */
  def insert(element: T): Unit
  
  /** Returns a Set containing the evaluations of the non-dominated elements in the Pareto front */
  def getEvaluations(): Set[Array[Double]]
  
  /** Queue keeping the elements in the order in which they were inserted */
  val priorityQueue = new Queue[T]()
  
  /** Returns a random element from the archive */
  def randomElement: T
  
  /** score = (nbPoints candidate dominates in Pareto front) + (nbPoints candidate is dominated by in Pareto front) */
  def score[T1 <: ParetoElement[U]](candidate: T1): Int
  
  /** Returns true if e1 [dominates more points in/is dominated by less points in]
    * the Pareto front than e2.
    * 
    * Returns
    *   true if (e1.dominance(e2) + score(e1) - score(e2)) > 0
    *   false otherwise
    */
  def cmpWithArchive[T1 <: ParetoElement[U], T2 <: ParetoElement[U]](e1: T1, e2: T2): Boolean = {
    (e1.dominance(e2) + score(e1) - score(e2)) > 0
  }
  
  /** Returns true if the Pareto front contains elem */
  def contains[T1 <: ParetoElement[U]](elem: T1): Boolean
  
  /** Function called every time an element is removed from the Pareto front */
  var onElementRemoved: (T) => Unit = {element: T => }
  
  /** Function called every time an element is added to the Pareto front */
  var onElementAdded: (T) => Unit = {element: T => }
}