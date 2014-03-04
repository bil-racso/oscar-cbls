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
package oscar.dfo.multiobjective.mogen.algos

import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.multiobjective.mogen.algos.states.DirectionalDirectSearchState
import oscar.algo.paretofront.ParetoFront
import oscar.dfo.multiobjective.mogen.MOGENTriplet
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.FeasibleRegion
import oscar.algo.paretofront.ParetoElement
import oscar.dfo.utils.Utils

object DirectionalDirectSearch extends ComparativeAlgorithm {
  var tolerance = math.pow(10.0, -5.0)
  
  /** The polling direction heuristic */
  var pollingHeuristic: (Int) => Array[Array[Double]] = unitPolling
  
  def singleIteration[T <: ParetoElement[Double]](state: ComparativeAlgorithmState, currentArchive: ParetoFront[Double, T], feasReg: FeasibleRegion, evaluator: MOEvaluator): List[MOOPoint] = {
    state match {
      case ddsState: DirectionalDirectSearchState => {
        if (ddsState.alpha < tolerance) {
          ddsState.reinitialize
        }
        val newPointCoordinates = ddsState.getPollCoordinates(pollingHeuristic(ddsState.bestPoint.nCoordinates))
        var potentialPoints = List[MOOPoint]()
        for (coords <- newPointCoordinates) {
          val newPoint = evaluator.eval(coords, feasReg)
          if (currentArchive.cmpWithArchive(newPoint, ddsState.bestPoint)) {
            potentialPoints ::= newPoint
          }
        }
        if (!potentialPoints.isEmpty) {
          ddsState.increaseStepSize
        }
        else {
          ddsState.decreaseStepSize
        }
        potentialPoints
      }
      case _ => throw new IllegalArgumentException("The Directional Direct-Search algorithm can only be used with a state for Directional Direct-Search");
    }
  }
  
  
  def getInitialState(coordinates: Array[Double], stepSizeIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): ComparativeAlgorithmState = {
    DirectionalDirectSearchState(evaluator.eval(coordinates, feasReg), (stepSizeIntervals(0)._1 + stepSizeIntervals(0)._2) / 2)
  }
  
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
}
