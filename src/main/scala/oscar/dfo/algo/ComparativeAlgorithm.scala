package oscar.dfo.algo

import oscar.dfo.utils.DFOPoint

trait ComparativeAlgorithm {
  
  /** Solves an optimization problem defined by its starting state and its comparison function.
    * 
    * @param startingState The state at which the algorithm resolution starts
    * @param comparisonFun The comparison function defining the optimization problem
    * @return A DFOPoint representing the approximation of the optimum */
  def solve(startingState: ComparativeAlgorithmState, comparisonFun: (DFOPoint, DFOPoint) => DFOPoint): DFOPoint
  
  /** Solves an optimization problem defined by its starting state and its comparison function.
    * 
    * @param startingState The state at which the algorithm iteration starts
    * @param comparisonFun The comparison function defining the optimization problem
    * @return A pair which first element is the new state obtained after one iteration
    *         starting from startingState and which second element is a list containing
    *         the new points discovered during the iteration */
  def iterate(startingState: ComparativeAlgorithmState, comparisonFun: (DFOPoint, DFOPoint) => DFOPoint): (ComparativeAlgorithmState, List[DFOPoint])
}