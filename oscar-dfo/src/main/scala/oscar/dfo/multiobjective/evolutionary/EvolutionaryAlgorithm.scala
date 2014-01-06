package oscar.dfo.multiobjective.evolutionary

import oscar.dfo.utils.FeasibleRegion
import oscar.dfo.utils.RandomGenerator
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MOOComparator

trait EvolutionaryAlgorithm[E] {
  /** The evaluator used for the optimization problem */
  val evaluator: MOEvaluator[E]
  
  /** The comparator used for the optimization problem */
  val comparator: MOOComparator[E]
  
  /** The size of the population */
  val populationSize: Int
  
  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  
  /** The population used to build new points */
  var population = List[EvolutionaryElement[E]]()
  
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = List[EvolutionaryElement[E]]()
  
  /** Adding feasibility functions passed as argument to the feasible region.
    * 
    * Doesn't modify feasibility functions already present in the feasible region.
    * 
    * @param feasibilityFunctions A list of functions taking coordinates as
    * 		 input and returning a Boolean. Each function is added to feasible
    *        region.
    */
  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]): Unit = {
    for (newFun <- feasibilityFunctions)
      feasibleRegion.addFunction(newFun)
  }
  
  /** Initializes the population of the SPEA2 algorithm.
    * 
    * Every point in the population will have random coordinates where each
    * dimension value is selected randomly among the intervals passed as parameter.
    * 
    * @param startInterval An array of pairs (lb, ub). For any pair (lb_i, ub_i) at
    * 		 index i, lb_i is the lower bound of the coordinate of dimension i+1 and
    *    	 ub_i is the upper bound. i.e. startInterval defines an hyper-box in which
    *      	 every point in the initial population will be.
    */
  def initPopulation(startIntervals: Array[(Double, Double)]): Unit = {
    for (i <- 1 to populationSize) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      val newElement = EvolutionaryElement(evaluator.eval(newCoordinates, feasibleRegion))
    }
  }
}