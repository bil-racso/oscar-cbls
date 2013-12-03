/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.dfo.mogen

import scala.reflect.ClassTag
import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.utils._
import oscar.dfo.visual.PlotDFOPareto2D
import oscar.visual.VisualFrame

class MOGEN[E <% Ordered[E]](var evaluator: MOEvaluator[E], comparator: MOOComparator[E]) {

  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = LinearList[E]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[E] => MOGENTriplet[E] = MOGEN.fairSelect

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
    var nbIterations = 1
    while (nbIterations <= maxIters) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    println("NB EVALS: " + evaluator.nbCallToEvalFunction)
    archive.toSet
  }
  
  def performIteration(iterationNumber: Int) {
    val currentTriplet = selectIterate
    MOGEN.onIterateSelected(currentTriplet)
    println("What?")
    //if (visu) {paretoPlot.highLightIterate(currentTriplet); Thread.sleep(500)}
    val newPoints = currentTriplet.getAlgorithm.singleIteration(currentTriplet.getAlgorithmState, archive, feasibleRegion, comparator, evaluator)
    var archiveChanged = false
    for (newPoint <- newPoints) {
      newPoint.iter = iterationNumber
      val newTriplet = MOGENTriplet(newPoint, currentTriplet.getAlgorithm, currentTriplet.getAlgorithmState.getNewState(newPoint, comparator))
      archiveChanged = archiveChanged || archive.insert(newTriplet, comparator)
      //if (archive.insert(newTriplet, comparator) && visu) {paretoPlot.update(archive); Thread.sleep(500)}
    }
    archive.insert(currentTriplet, comparator)
    if (archiveChanged) MOGEN.onArchiveChanged(archive)
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
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
  
  def randomSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    archive.randomElement match {
      case triplet: MOGENTriplet[E] => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
  
  var onIterateSelected: (MOGENTriplet[_]) => Unit = {triplet: MOGENTriplet[_] => println("KKKKKKKKKKKKK")}
  var onArchiveChanged: (ParetoFront[_]) => Unit = {newArchive: ParetoFront[_] => }
}
