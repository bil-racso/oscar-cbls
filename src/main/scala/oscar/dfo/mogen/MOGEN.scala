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
import oscar.util.mo.ArchiveUtils

class MOGEN[E <% Ordered[E]](var evaluator: MOEvaluator[E], comparator: MOOComparator[E], suspendable: Boolean = false) {

  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  val archive = LinearList[E]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[E] => MOGENTriplet[E] = MOGEN.combinationWithProbas(List(/*(MOGEN.hyperPlaneSelect, 10.0),*/ (MOGEN.mostIsolatedSelect, 15.0), (MOGEN.fairSelect, 15.0), (MOGEN.randomSelect, 1.0)))//MOGEN.randomSelect//MOGEN.hyperPlaneSelect//MOGEN.fairSelect

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)
  
  def setSelectionHeuristic(heuristic: ParetoFront[E] => MOGENTriplet[E]): Unit = {
    selectionHeuristic = heuristic
  }

  def initArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], algorithms: List[(ComparativeAlgorithm, Double)]): Unit = {
    for (i <- 1 to maxNbPoints) {
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

  def optimizeMOO(maxEvals: Int, maxIters: Int = Int.MaxValue): Set[MOOPoint[E]] = {
    MOGEN.onArchChan(archive)
    var nbIterations = 1
    while (evaluator.nbCallToEvalFunction <= maxEvals && nbIterations <= maxIters) {
      performIteration(nbIterations)
      nbIterations += 1
    }
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
    }
    archive.insert(currentTriplet, comparator)
    if (archiveChanged) MOGEN.onArchChan(archive)
  }

  def selectIterate: MOGENTriplet[E] = {
    val newIterate = selectionHeuristic(archive)
    if (archive.removeElement(newIterate)) newIterate
    else throw new IllegalArgumentException("The archive didn't contain the iterate... It cannot be!")
  }
  
  def printAlgosProportion = {
    println("Archive size: " + archive.size)
    val algoMap = scala.collection.mutable.HashMap[ComparativeAlgorithm, Int]()
    for (elem <- archive.getElements) {
      elem match {
        case triplet: MOGENTriplet[E] => {
          algoMap.get(triplet.getAlgorithm) match {
            case Some(algoCount) => algoMap.update(triplet.getAlgorithm, algoCount + 1)
            case None => algoMap.put(triplet.getAlgorithm, 1)
          }
        }
        case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplets")
      }
    }
    for ((algo, algoCount) <- algoMap) {
      println()
    }
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
  
  def mostIsolatedSelect[E](archive: ParetoFront[E]): MOGENTriplet[E] = {
    val mostDistantConsecutivePoints = ArchiveUtils.getMostDistantConsecutivePoints(archive)
    val randNum = RandomGenerator.nextDouble * mostDistantConsecutivePoints.length
    for (i <- 0 until mostDistantConsecutivePoints.length) {
      if (randNum < i + 1) {
        return mostDistantConsecutivePoints(i).asInstanceOf[MOGENTriplet[E]]
      }
    }
    return mostDistantConsecutivePoints(mostDistantConsecutivePoints.length - 1).asInstanceOf[MOGENTriplet[E]]
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
  
  def combinationWithProbas[E](selectFunctions: List[((ParetoFront[E]) => MOGENTriplet[E], Double)]): (oscar.util.mo.ParetoFront[E]) => oscar.dfo.mogen.MOGENTriplet[E] = {
    val probaSum = selectFunctions.foldLeft(0.0)((acc, newPair) => acc + newPair._2)
    def probaSelect(archive: ParetoFront[E]): MOGENTriplet[E] = {
      val randNum = RandomGenerator.nextDouble
      var sumOfProbas = 0.0
      for (pair <- selectFunctions) {
        sumOfProbas += pair._2
        if (sumOfProbas >= randNum * probaSum) {
          return pair._1(archive)
        }
      }
      selectFunctions(0)._1(archive)
    }
    probaSelect
  }
  
  
  
  var onIterSel: (MOGENTriplet[_]) => Unit = {triplet: MOGENTriplet[_] => }
  var onArchChan: (ParetoFront[_]) => Unit = {newArchive: ParetoFront[_] => }
  
  def onIterateSelected(newFun: MOGENTriplet[_] => Unit) {
	onIterSel = newFun
  }
  
  def onArchiveChanged[E](newFun: ParetoFront[_] => Unit) {
	onArchChan = newFun
  }
}