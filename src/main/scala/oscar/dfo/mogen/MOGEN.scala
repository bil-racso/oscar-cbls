package oscar.dfo.mogen

import oscar.util.mo.ParetoFront
import oscar.util.mo.MOOPoint

/** Framework for the Nelder-Mead algorithm used to find the optimum
  * of a derivative free optimization (DFO) problem.
  *
  * @constructor Create a new Nelder-Mead framework for the problem specified by its function, the
  *              comparison function for evaluations and the domain around the specified
  *              starting point
  * @param f Function to optimize
  * @param nbObjectives The number of objectives to optimize
  * @param compare The comparison function to compare evaluations
  * @param startP The starting point from which the algorithm will begin
  * @param dom The domain of the search space
  * @throws IllegalArgumentException If the starting point doesn't lie within the
  *                                     domain or if the starting point and the domain don't have
  *                                     the same dimensions 
  * @author Cyrille Dejemeppe
  **/
class MOGEN[T, E <% Ordered[E]](var evalFunction: T => Array[E], frontType: String = "LinearList") {
  /** Function defining the feasible region of the problem */
  var feasibleRegion: T => Boolean = ((coordinate: T) => true)
  /** Functions of evaluation to optimize */
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive: ParetoFront[T, E] = _
  
  def initArchive(points: Array[T], frontType: String = "LinearList") = {
    for (point <- points) {
      val newPoint = MOOPoint(point, evalFunction)
    }
  }
}