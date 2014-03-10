package oscar.dfo.multiobjective.algos

import oscar.dfo.multiobjective.MODFOptimizer
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.MOEvaluator
import oscar.util.RandomGenerator
import oscar.algo.paretofront.ParetoFront
import oscar.dfo.multiobjective.mogen.MOGEN
import oscar.dfo.utils.FeasibleRegion
import oscar.algo.paretofront.LinearList
import oscar.dfo.multiobjective.mogen.MOGENTriplet
import oscar.dfo.multiobjective.mogen.algos.ComparativeAlgorithm
import oscar.dfo.utils.Utils

class DirectMultiSearch(val evaluator: MOEvaluator) {
  
  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = LinearList[Double, DirectMultiSearchElement]()
  /** The iterate selection heuristic */
  var selectionHeuristic: () => DirectMultiSearchElement = archive.fairSelect
  /** The polling direction heuristic */
  var pollingHeuristic: (Int) => Array[Array[Double]] = DirectMultiSearch.unitPolling
  /** The starting intervals of the points */
  var startIntervals = Array[(Double, Double)]()
  /** The probability of performing a search step */
  var searchStepProba = 0.1
  /** The algorithms used by MOGEN and their proportion of usage */
  var initialAlpha = 1.0
  /** The increasing step size parameter */
  var increaseFactor = 2.0
  /** The decreasing step parameter */
  var decreaseFactor = 0.5

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)
  
  def initRandomArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], alpha: Double): Unit = {
    this.startIntervals = startIntervals
    this.initialAlpha = alpha
    for (i <- 0 until maxNbPoints) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newDMSElement = DirectMultiSearchElement(evaluator.eval(newCoordinates, feasibleRegion), initialAlpha)
        archive.insert(newDMSElement)
        if (archive.contains(newDMSElement)) MOGEN.onArchiveChanged(archive)
        if (archive.size >= maxNbPoints) return
      }
    }
  }
  
  def initSinglePointArchive(startIntervals: Array[(Double, Double)], alpha: Double): Unit = {
    this.startIntervals = startIntervals
    this.initialAlpha = alpha
    val newCoordinates = startIntervals.map(elem => (elem._1 + elem._2) / 2.0)
    if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
      val newDMSElement = DirectMultiSearchElement(evaluator.eval(newCoordinates, feasibleRegion), initialAlpha)
      archive.insert(newDMSElement)
      if (archive.contains(newDMSElement)) MOGEN.onArchiveChanged(archive)
    }
  }
  
  def initLineArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], alpha: Double): Unit = {
    this.startIntervals = startIntervals
    this.initialAlpha = alpha
    for (i <- 0 until maxNbPoints) {
      val newCoordinates = startIntervals.map(elem => elem._1 + (i.toDouble / (maxNbPoints - 1).toDouble) * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newDMSElement = DirectMultiSearchElement(evaluator.eval(newCoordinates, feasibleRegion), initialAlpha)
        archive.insert(newDMSElement)
        if (archive.contains(newDMSElement)) MOGEN.onArchiveChanged(archive)
        if (archive.size >= maxNbPoints) return
      }
    }
  }

  def optimizeMOO(maxIters: Int, maxEvals: Int = Int.MaxValue): Set[MOOPoint] = {
    var nbIterations = 1
    while (nbIterations <= maxIters && evaluator.nCallToEvalFunction <= maxEvals) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    archive.toSet.map((e: DirectMultiSearchElement) => e.getMOOPoint)
  }
  
  def performIteration(iterationNumber: Int) {
    if (RandomGenerator.nextDouble <= searchStepProba) searchStep
    val currentElement = selectIterate
    DirectMultiSearch.onIterateSelected(currentElement)
    val newPoints = currentElement.getPollCoordinates(pollingHeuristic(currentElement.nCoordinates))
    var archiveChanged = false
    for (newPoint <- newPoints) {
      val newElement = DirectMultiSearchElement(evaluator.eval(newPoint, feasibleRegion), currentElement.alpha * increaseFactor)
      archive.insert(newElement)
      if (archive.contains(newElement)) {
        archiveChanged = true
        MOGEN.onArchiveChanged(archive)
      }
    }
    if (archive.contains(currentElement)) {
      archive.priorityQueue.enqueue(currentElement)
    }
    if (archiveChanged) {
      currentElement.alpha *= increaseFactor
    }
    else {
      currentElement.alpha *= decreaseFactor
    }
  }
  
  def searchStep {
    var archiveChanged = false
    if (archive.size > 1) {
	  val point1 = archive.randomElement
	  val point2 = archive.randomElement
	  val randMultiplier = RandomGenerator.nextDouble
	  val newCoords = Array.tabulate(point1.nCoordinates)(i => point1.coordinates(i) + randMultiplier * point1.coordinates(i) + (1.0 - randMultiplier) * point2.coordinates(i))
	  if (feasibleRegion.isFeasible(newCoords)) {
	    val newElement = DirectMultiSearchElement(evaluator.eval(newCoords, feasibleRegion), initialAlpha)
		archive.insert(newElement)
		if (archive.contains(newElement)) MOGEN.onArchiveChanged(archive)
	  }
    }
    else {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newElement = DirectMultiSearchElement(evaluator.eval(newCoordinates, feasibleRegion), initialAlpha)
        archive.insert(newElement)
        if (archive.contains(newElement)) MOGEN.onArchiveChanged(archive)
      }
    }
  }

  def selectIterate: DirectMultiSearchElement = {
    val newIterate = selectionHeuristic()
    newIterate
  }
}

object DirectMultiSearch {  
  def apply(evaluator: MOEvaluator) = new DirectMultiSearch(evaluator)
  
  def randomPolling(nDimensions: Int): Array[Array[Double]] = {
    val positiveDirections = Array.tabulate(nDimensions)(i => Utils.randomNormalizedVector(nDimensions))
    val negativeDirections = positiveDirections.map(direction => direction.map(e => e * -1))
    positiveDirections ++ negativeDirections
  }
  
  def unitPolling(nDimensions: Int): Array[Array[Double]] = {
    val positiveDirections = Array.tabulate(nDimensions)(i => Array.tabulate(nDimensions)(j => if (i == j) 1.0 else 0.0))
    val negativeDirections = Array.tabulate(nDimensions)(i => Array.tabulate(nDimensions)(j => if (i == j) -1.0 else 0.0))
    positiveDirections ++ negativeDirections
  }
  
  var onIterateSelected: (DirectMultiSearchElement) => Unit = {triplet: DirectMultiSearchElement => }
  var onArchiveChanged: (ParetoFront[_, _]) => Unit = {newArchive: ParetoFront[_, _] => }
}
