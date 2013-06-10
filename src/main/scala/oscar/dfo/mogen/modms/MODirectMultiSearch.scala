package oscar.dfo.mogen.modms

import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOEvaluator
import oscar.util.mo.ParetoFront
import oscar.util.mo.FeasibleRegion
import oscar.dfo.mogen.MOGENTriplet
import oscar.dfo.mogen.MOGEN
import oscar.dfo.mogen.utils.ArrayUtils
import oscar.util.mo.RandomGenerator
import oscar.util.mo.LinearList

class MODirectMultiSearch[E <% Ordered[E]](
		val evaluator: MOEvaluator[E],
		val comparator: MOOComparator[E],
		val beta1: Double,
		val beta2: Double,
		val gamma: Double
	) {

	/** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  val archive = LinearList[E]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[E] => MODMSElement[E] = MODirectMultiSearch.fairSelect[E]

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)

  def initArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], alpha: Double): Unit = {
    for (i <- 1 to maxNbPoints) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newElement = MODMSElement(evaluator.eval(newCoordinates, feasibleRegion), alpha)
        archive.insert(newElement, comparator)
        if (archive.size >= maxNbPoints) return
      }
    }
  }
  
  def optimizeMOO(maxIters: Int): Set[MOOPoint[E]] = {
    MODirectMultiSearch.onArchChan(archive)
    var nbIterations = 1
    while (nbIterations <= maxIters) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    archive.toSet
  }
  
  def performIteration(iterationNumber: Int): Unit = {
    val currentElement = selectIterate
    MODirectMultiSearch.onIterSel(currentElement)
    val basis = getRandomBasis(currentElement.getMOOPoint)
    val newPoints = getPollPoints(currentElement, basis)
    var archiveChanged = false
    for (newPoint <- newPoints) {
      newPoint.iter = iterationNumber
      val newMODMSElement = MODMSElement(newPoint, currentElement.getAlpha)
      increaseStepSizes(newMODMSElement)
      archiveChanged = archiveChanged || archive.insert(newMODMSElement, comparator)
    }
    archive.insert(currentElement, comparator)
    if (archiveChanged) {
      increaseStepSizes(currentElement)
      MODirectMultiSearch.onArchChan(archive)
      //println("Archive changed")
    }
    else {
      decreaseStepSizes(currentElement)
      //println("Archive didn't change")
    }
    println("MODMS: nb iterations: " + evaluator.nbCallToEvalFunction)
  }

  def selectIterate: MODMSElement[E] = {
    val newIterate = selectionHeuristic(archive)
    if (archive.removeElement(newIterate)) newIterate
    else throw new IllegalArgumentException("The archive didn't contain the iterate... It cannot be!")
  }

  def getRandomBasis(point: MOOPoint[E]): Array[Array[Double]] = {
  	val positiveBasis = Array.tabulate(point.nbCoordinates){
  		i => ArrayUtils.normalize(Array.tabulate(point.nbCoordinates)(j => 2 * RandomGenerator.nextDouble - 1))
  	}
  	Array.tabulate(point.nbCoordinates * 2){ i =>
  		if (i < point.nbCoordinates) positiveBasis(i)
  		else ArrayUtils.arrayProd(positiveBasis(i - point.nbCoordinates), -1.0)
  	}
  }

  def getPollPoints(point: MODMSElement[E], basis: Array[Array[Double]]): Array[MOOPoint[E]] = {
  	Array.tabulate(basis.length)(i => evaluator.eval(ArrayUtils.arraySum(point.getMOOPoint.coordinates, ArrayUtils.arrayProd(basis(i), point.getAlpha)), feasibleRegion))
  }
  
  def increaseStepSizes(point: MODMSElement[E]) = point.updateAlpha(gamma)
  
  def decreaseStepSizes(point: MODMSElement[E]) = point.updateAlpha(beta1 + RandomGenerator.nextDouble * (beta2 - beta1))

}

object MODirectMultiSearch {
  def apply[E <% Ordered[E]](
      evaluator: MOEvaluator[E],
      comparator: MOOComparator[E],
      beta1: Double = 0.5,
      beta2: Double = 0.5,
      gamma: Double = 1.0
	): MODirectMultiSearch[E] = new MODirectMultiSearch(evaluator, comparator, beta1, beta2, gamma)
  
  def fairSelect[E](archive: ParetoFront[E]): MODMSElement[E] = {
    archive.head match {
      case element: MODMSElement[E] => element
      case _ => throw new IllegalArgumentException("A MODMS archive must only contain MODMS elements")
    }
  }
  
  var onIterSel: (MODMSElement[_]) => Unit = {element: MODMSElement[_] => }
  var onArchChan: (ParetoFront[_]) => Unit = {newArchive: ParetoFront[_] => }
  
  def onIterateSelected(newFun: MODMSElement[_] => Unit) {
	onIterSel = newFun
  }
  
  def onArchiveChanged(newFun: ParetoFront[_] => Unit) {
	onArchChan = newFun
  }
}