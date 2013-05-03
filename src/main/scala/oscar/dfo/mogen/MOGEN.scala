package oscar.dfo.mogen

import scala.reflect.ClassTag

import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.util.mo.ArchiveElement
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.LinearList
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.util.mo.RandomGenerator
import oscar.visual.PlotDFOPareto2D
import oscar.visual.VisualFrame
import scala.util.continuations._

class MOGEN[E <% Ordered[E]](var evaluator: MOEvaluator[E], comparator: MOOComparator[E], suspendable: Boolean = false) {

  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = LinearList[E]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[E] => MOGENTriplet[E] = MOGEN.hyperPlaneSelect//MOGEN.fairSelect

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)

  def initArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], algorithms: List[(ComparativeAlgorithm, Double)]): Unit = {
    for (i <- 1 to 100) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newAlgo = getRandomAlgo(algorithms)
        val newAlgoState = newAlgo.getInitialState(newCoordinates, startIntervals, evaluator, feasibleRegion, comparator)
        for (newPoint <- newAlgoState.getPoints) {
          val newTriplet = MOGENTriplet(newPoint, newAlgo, newAlgoState.getNewState(newPoint, comparator))
          archive.insert(newTriplet, comparator)
          if (archive.size >= maxNbPoints) return
        }
      }
    }
  }

  def getRandomAlgo(algorithms: List[(ComparativeAlgorithm, Double)]): ComparativeAlgorithm = {
    val sumOfProbas = algorithms.foldLeft(0.0)((acc, newPair) => acc + newPair._2)
    val randNum = RandomGenerator.nextDouble * sumOfProbas
    var accSum = 0.0
    for ((algo, proba) <- algorithms) {
      accSum += proba
      if (accSum > randNum) return algo
    }
    algorithms(0)._1
  }

  def optimizeMOO(maxIters: Int): Set[MOOPoint[E]] = {
    MOGEN.onArchChan(archive)
    var nbIterations = 1
    while (nbIterations <= maxIters) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    println("NB EVALS: " + evaluator.nbCallToEvalFunction)
    archive.toSet
  }
  
  def performIteration(iterationNumber: Int): Unit = {
    val currentTriplet = selectIterate
    MOGEN.onIterSel(currentTriplet)
    val newPoints = currentTriplet.getAlgorithm.singleIteration(currentTriplet.getAlgorithmState, archive, feasibleRegion, comparator, evaluator)
    var archiveChanged = false
    for (newPoint <- newPoints) {
      newPoint.iter = iterationNumber
      val newTriplet = MOGENTriplet(newPoint, currentTriplet.getAlgorithm, currentTriplet.getAlgorithmState.getNewState(newPoint, comparator))
      archiveChanged = archiveChanged || archive.insert(newTriplet, comparator)
      //if (archive.insert(newTriplet, comparator) && visu) {paretoPlot.update(archive); Thread.sleep(500)}
    }
    archive.insert(currentTriplet, comparator)
    if (archiveChanged) MOGEN.onArchChan(archive)
  }

  def selectIterate: MOGENTriplet[E] = {
    val newIterate = selectionHeuristic(archive)
    if (archive.removeElement(newIterate)) newIterate
    else throw new IllegalArgumentException("The archive didn't contain the iterate... It cannot be!")
  }

}

object MOGEN {

  def apply[E <% Ordered[E]](evaluator: MOEvaluator[E], comparator: MOOComparator[E], visu: Boolean = false) = new MOGEN(evaluator, comparator)

  def fairSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    archive.head match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplets")
    }
  }
  
  def randomSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    archive.randomElement match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplets")
    }
  }
  
  def hyperPlaneSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    val points = archive.getExtremePoints
    //TODO Change these lines here to get the multi-dimensional case
    val num = (points(0)._1.getMOOPoint.coordinates(1) - points(0)._2.getMOOPoint.coordinates(1))
    val den = (points(0)._1.getMOOPoint.coordinates(0) - points(0)._2.getMOOPoint.coordinates(0))
    val slope = if (den != 0) num / den else 0
    val d = points(0)._1.getMOOPoint.coordinates(1) - slope * points(0)._1.getMOOPoint.coordinates(0)
    val randX1 = points(0)._1.getMOOPoint.coordinates(0) + (points(0)._2.getMOOPoint.coordinates(0) - points(0)._1.getMOOPoint.coordinates(0)) * RandomGenerator.nextDouble
    val newCoordinates = Array.tabulate(points(0)._1.getMOOPoint.nbCoordinates)(i => if (i == 0) randX1 else slope * randX1 + d)
    archive.getClosest(newCoordinates) match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplets")
    }
  }
  
  
  
  var onIterSel: (MOGENTriplet[_]) => Unit = {triplet: MOGENTriplet[_] => }
  var onArchChan: (ParetoFront[_]) => Unit = {newArchive: ParetoFront[_] => }
  
  def onIterateSelected(newFun: MOGENTriplet[_] => Unit) {
	onIterSel = newFun
  }
  
  def onArchiveChanged(newFun: ParetoFront[_] => Unit) {
	onArchChan = newFun
  }
}