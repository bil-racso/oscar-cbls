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

object DirectionalDirectSearch extends ComparativeAlgorithm {
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[T <: ParetoElement[Double]](state: ComparativeAlgorithmState, currentArchive: ParetoFront[Double, T], feasReg: FeasibleRegion, evaluator: MOEvaluator): List[MOOPoint] = {
    state match {
      case ddsState: DirectionalDirectSearchState => {
        println("-" * 80)
        println("BestPoint: " + ddsState.bestPoint)
        println("Basis: \n" + ddsState.currentBasis.map(e => e.mkString("(", ", ", ")")).mkString("\n"))
        println("StepSize: " + ddsState.stepSizes.mkString(","))
        if (ddsState.getSmallestStepSize < tolerance) {
          ddsState.reinitialize
        }
        for (i <- 0 until ddsState.basisSize) {
          val newPoint = ddsState.getNewPoint(i, evaluator, feasReg)
          if (currentArchive.cmpWithArchive(newPoint, ddsState.bestPoint)) {
            ddsState.updateBasis
            ddsState.increaseStepSizes
            return List(newPoint)
          }
        }
        ddsState.updateBasis
        ddsState.decreaseStepSizes
        List[MOOPoint]()
      }
      case _ => throw new IllegalArgumentException("The Directional Direct-Search algorithm can only be used with a state for Directional Direct-Search");
    }
  }
  
  
  def getInitialState(coordinates: Array[Double], stepSizeIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): ComparativeAlgorithmState = {
    DirectionalDirectSearchState(evaluator.eval(coordinates, feasReg), stepSizeIntervals)
  }
}
