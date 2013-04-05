package oscar.dfo.mogen.algos

import oscar.util.mo.FeasibleRegion

trait ComparativeAlgorithm {
  /** Performs the optimization algorithm once from the starting point.
    * 
    * @param tol The tolerance which defines the desired level of precision
    * @param evalLimit The maximum number of evaluations before stopping the algorithm
    * @param timeLimit The maximum time to spend before stopping the algorithm
    * @return A tuple whose first element is the optimal point found and whose
    *         second element is the evaluations of the functions at this point */
  def optimize(tol: Double, evalLimit: Int, timeLimit: Int): (Array[Double], Array[Double])
  
  def singleIteration[T, E](feasReg: FeasibleRegion[T, E], cmpFunction: Boolean)
  
  /** Function to be called after the algorithm performed a successful iteration */
  var onImprovement: () => Unit = () => {}
}