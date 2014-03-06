package oscar.dfo.multiobjective.evolutionary

import oscar.dfo.utils.FeasibleRegion
import oscar.util.RandomGenerator
import oscar.dfo.utils.MOEvaluator

trait EvolutionaryAlgorithm {
  /** The evaluator used for the optimization problem */
  val evaluator: MOEvaluator
  
  /** The size of the population */
  val populationSize: Int
  
  /** The size of the population */
  val archiveSize: Int
  
  /** The probability of mutation for an element in the population */
  val mutationProba: Double
  
  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  
  /** The population used to build new points */
  var population = List[EvolutionaryElement]()
  
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = List[EvolutionaryElement]()
  
  /** The minimal and maximal mutation applied to each dimension */
  var mutationIntervals = Array[(Double, Double)]()
  
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
    population = List[EvolutionaryElement]()
    for (i <- 1 to populationSize) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      val newElement = EvolutionaryElement(evaluator.eval(newCoordinates, feasibleRegion))
      population ::= newElement
    }
    mutationIntervals = Array.tabulate(startIntervals.length)(i => {
      val intervalSize = (startIntervals(i)._2 - startIntervals(i)._1) / 20
      (-intervalSize, intervalSize)
    })
  }
  
  /** Tournament selection over k random individuals from the population.
    * 
    * k elements are randomly selected from the population and the best
    * (the one with the smallest fitness) from these k elements is
    * returned. 
    * 
    * @param fitnessValues A list of pairs (e, f) where e is an evolutionary
    * 		 element and f is its associated fitness value
    * @param k The number of random elements among which the best element
    * 		 must be returned
    * @return An evolutionary element chosen by a k-ary tournament selection
    */
  def kAryTournamentSelection(fitnessValues: List[(EvolutionaryElement, Double)], k: Int): EvolutionaryElement = {
    def getFitness(element: EvolutionaryElement): Double = {
      def getFitnessAux(fitVals: List[(EvolutionaryElement, Double)]): Double = {
        fitVals match {
          case head :: tail => {
            if (head._1 == element) head._2
            else getFitnessAux(tail)
          }
          case Nil => throw new Exception("There was no element matching one from the population")
        }
      }
      getFitnessAux(fitnessValues)
    }
    var bestElem = (population(0), getFitness(population(0)))
    for (i <- 1 to k) {
      val randElem = population(RandomGenerator.nextInt(populationSize))
      val fitEl = getFitness(randElem)
      if (fitEl < bestElem._2) bestElem = (randElem, fitEl)
    }
    bestElem._1
  }
  
  /** Tournament selection over 2 random individuals from the population.
    * 
    * 2 elements are randomly selected from the population and the best
    * (the one with the smallest fitness) from these 2 elements is
    * returned. 
    * 
    * @param fitnessValues A list of pairs (e, f) where e is an evolutionary
    * 		 element and f is its associated fitness value
    * @return An evolutionary element chosen by a binary tournament selection
    */
  def binaryTournamentSelection(fitnessValues: List[(EvolutionaryElement, Double)]) = kAryTournamentSelection(fitnessValues, 2)
  
  /** Performs a crossover of the two parents to get two new children.
    * 
    * This is a one-point crossover and the crossover point is
    * selected randomly.
    * 
    * @param parent1 The first parent for the crossover
    * @param parent2 The second parent for the crossover
    * @return A pair of two evolutionary elements (e1, e2) generated
    */
  def crossover(parent1: EvolutionaryElement, parent2: EvolutionaryElement): (EvolutionaryElement, EvolutionaryElement) = {
    val crossoverPoint = RandomGenerator.nextInt(parent1.nCoordinates + 1)
    val child1Coordinates = parent1.getCoordinates.take(crossoverPoint) ++ parent2.getCoordinates.drop(crossoverPoint)
    val child2Coordinates = parent2.getCoordinates.take(crossoverPoint) ++ parent1.getCoordinates.drop(crossoverPoint)
    (EvolutionaryElement(evaluator.eval(child1Coordinates, feasibleRegion)), EvolutionaryElement(evaluator.eval(child2Coordinates, feasibleRegion)))
  }
  
  /** Performs a mutation over the element with the probability mutationProba.
    * 
    * The mutation is performed randomly on each dimension according to a .
    * 
    * @param element The element to be mutated
    * @return A new evolutionary element which is a mutation of the original one
    */
  def mutation(element: EvolutionaryElement): EvolutionaryElement = {
    def getRandPerturb(index: Int): Double = {
      element.getCoordinates(index) + (mutationIntervals(index)._1 +
          RandomGenerator.nextDouble * (mutationIntervals(index)._2 - mutationIntervals(index)._1))
    }
    if (RandomGenerator.nextDouble < mutationProba) {
	  val newCoordinates = Array.tabulate(element.nCoordinates)(i => {
	    if (RandomGenerator.nextDouble < mutationProba) getRandPerturb(i)
	    else element.getCoordinates(i)
	  })
	  EvolutionaryElement(evaluator.eval(newCoordinates, feasibleRegion))
    }
    else element
  }
  
  /** 
    * 
    */
  def updatePopulation(fitnessValues: List[(EvolutionaryElement, Double)]): Unit = {
    var newPopulation = List[EvolutionaryElement]()
    for (i <- 1 to populationSize by 2) {
      val parent1 = archive(RandomGenerator.nextInt(archiveSize))
      val parent2 = archive(RandomGenerator.nextInt(archiveSize))
      val (child1, child2) = crossover(parent1, parent2)
      newPopulation ::= mutation(child1)
      newPopulation ::= mutation(child2)
    }
    population = newPopulation
  }
}