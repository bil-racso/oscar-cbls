package oscar.dfo.mogen

import scala.reflect.ClassTag
import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.dfo.mogen.algos.ComparativeAlgorithmState
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.util.mo.RandomGenerator
import oscar.util.mo.LinearList
import oscar.util.mo.MOEvaluator
import oscar.util.mo.ArchiveElement
import oscar.dfo.mogen.algos.NelderMeadState
import oscar.visual.VisualFrame
import oscar.visual.PlotDFOPareto2D

class MOGEN[E <% Ordered[E]](var evaluator: MOEvaluator[E], comparator: MOOComparator[E], val visu: Boolean) {

  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = LinearList[E]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[E] => MOGENTriplet[E] = MOGEN.fairSelect
  /** The visualisation pareto plot */
  val paretoPlot = PlotDFOPareto2D[E](nbPareto = 1, objMax1 = false, objMax2 = false)
  /** The frame that will be used to contain the pareto plot */
  if (visu) {
    val f = new VisualFrame("MOGEN", 1, 2)
    f.add(paretoPlot)
    f.pack()
  }

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)

  def initArchive(nbPoints: Int, startIntervals: Array[(Double, Double)], algorithms: List[(ComparativeAlgorithm, Double)]) = {
    var pointCreationCounter = 0
    while (archive.isEmpty && pointCreationCounter < nbPoints) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newAlgo = getRandomAlgo(algorithms)
        val newAlgoState = newAlgo.getInitialState(newCoordinates, startIntervals, evaluator, feasibleRegion, comparator)
        for (newPoint <- newAlgoState.getPoints) {
          val newTriplet = MOGENTriplet(newPoint, newAlgo, newAlgoState.getNewState(newPoint, comparator))
          archive.insert(newTriplet, comparator)
        }
      }
      pointCreationCounter += 1
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
    var nbIterations = 1
    while (nbIterations <= maxIters) {
      println("=" * 20 + " " * 5 + "ITERATION " + nbIterations + " " * 5 + "=" * 20)
      val currentTriplet = selectIterate
      if (visu) {paretoPlot.highLightIterate(currentTriplet); Thread.sleep(500)}
      val newPoints = currentTriplet.getAlgorithm.singleIteration(currentTriplet.getAlgorithmState, archive, feasibleRegion, comparator, evaluator)
      for (newPoint <- newPoints) {
        newPoint.iter = nbIterations
        val newTriplet = MOGENTriplet(newPoint, currentTriplet.getAlgorithm, currentTriplet.getAlgorithmState.getNewState(newPoint, comparator))
        if (archive.insert(newTriplet, comparator) && visu) {paretoPlot.update(archive); Thread.sleep(500)}
      }
      archive.insert(currentTriplet, comparator)
      nbIterations += 1
    }
    println("NB EVALS: " + evaluator.nbCallToEvalFunction)
    archive.toSet
  }

  def selectIterate: MOGENTriplet[E] = {
    val newIterate = selectionHeuristic(archive)
    if (archive.removeElement(newIterate)) newIterate
    else throw new IllegalArgumentException("The archive didn't contain the iterate... It cannot be!")
  }

}

object MOGEN {

  def apply[E <% Ordered[E]](evaluator: MOEvaluator[E], comparator: MOOComparator[E], visu: Boolean = false) = new MOGEN(evaluator, comparator, visu)

  def fairSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    archive.head match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
  def randomSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    archive.randomElement match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
}