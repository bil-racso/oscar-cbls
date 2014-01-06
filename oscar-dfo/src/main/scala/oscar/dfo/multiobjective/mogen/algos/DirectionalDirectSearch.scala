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

import oscar.dfo.utils._
import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.multiobjective.mogen.algos.states.DirectionalDirectSearchState

object DirectionalDirectSearch extends ComparativeAlgorithm {
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case ddsState: DirectionalDirectSearchState[E] => {
        for (i <- 0 until ddsState.basisSize) {
          val newPoint = ddsState.getNewPoint(i, evaluator, feasReg)
          if (comparator.cmpWithArchive(newPoint, ddsState.bestPoint, currentArchive)) {
            ddsState.updateBasis
            ddsState.increaseStepSizes
            return List(newPoint)
          }
        }
        ddsState.updateBasis
        ddsState.decreaseStepSizes
        List[MOOPoint[E]]()
      }
      case _ => throw new IllegalArgumentException("The Directional Direct-Search algorithm can only be used with a state for Directional Direct-Search");
    }
  }
  
  
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], stepSizeIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    DirectionalDirectSearchState(evaluator.eval(coordinates, feasReg), stepSizeIntervals)
  }
}
