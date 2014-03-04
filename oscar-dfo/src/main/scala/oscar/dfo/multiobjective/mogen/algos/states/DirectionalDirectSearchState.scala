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
package oscar.dfo.multiobjective.mogen.algos.states

import oscar.util.RandomGenerator
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.FeasibleRegion
import oscar.dfo.utils.Utils

class DirectionalDirectSearchState(initPoint: MOOPoint, var alpha: Double) extends ComparativeAlgorithmState {
  var bestPoint = initPoint
  val initAlpha = alpha
  
  def getBestPoint = bestPoint
  
  def getPoints = List(bestPoint)
  
  def getNewState(newBestPoint: MOOPoint): ComparativeAlgorithmState = {
    DirectionalDirectSearchState(newBestPoint, alpha)
  }
  
  /** Returns the points found by the algorithm */
  def getPollCoordinates(basis: Array[Array[Double]]): Array[Array[Double]] = {
    Array.tabulate(basis.length)(i => {
      Array.tabulate(initPoint.nCoordinates)(j => initPoint.coordinates(j) + alpha * basis(i)(j))
    })
  }
  
  def reinitialize = {
    alpha = initAlpha
  }
  
  def increaseStepSize = {
    alpha *= DirectionalDirectSearchState.increaseFactor
  }
  def decreaseStepSize = {
    alpha *= DirectionalDirectSearchState.decreaseFactor
  }
}

object DirectionalDirectSearchState {
  var increaseFactor = 2.0
  var decreaseFactor = 0.5
  
  def apply(initPoint: MOOPoint, alpha: Double) = new DirectionalDirectSearchState(initPoint, alpha)
}
