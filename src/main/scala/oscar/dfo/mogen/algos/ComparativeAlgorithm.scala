package oscar.dfo.mogen.algos

import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOOComparator
import oscar.util.mo.ParetoFront
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOEvaluator

trait ComparativeAlgorithm {
  /** Performs one iteration of the algorithm starting from state and comparing points with comparator and currentArchive.
   * 
   * The points in the current archive which are dominated by candidate points are removed from the archive
   * 
   * @param state The state from which the algorithm performs the iteration
   * @param currentArchive The archive to which candidate points are compared
   * @param feasReg The feasible region of the considered problem
   * @param comparator The comparator used to compare candidate points with current points
   * @return A tuple which first element is the state updated after the iteration and
   *         the second element is the set of candidate points non dominated by the archive
   */
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]]
  
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E]
  
  /** Function to be called after the algorithm performed a successful iteration */
  var onImprovement: () => Unit = () => {}
}