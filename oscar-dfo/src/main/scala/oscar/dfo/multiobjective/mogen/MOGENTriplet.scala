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
package oscar.dfo.multiobjective.mogen

import oscar.dfo.utils._
import oscar.dfo.multiobjective.mogen.algos.ComparativeAlgorithm
import oscar.dfo.multiobjective.mogen.algos.ComparativeAlgorithm
import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.algo.paretofront.ParetoElement

class MOGENTriplet(point: MOOPoint, algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState) extends ParetoElement[Double] {
  def coordinates: Array[Double] = point.coordinates
  
  def objectives: Array[Double] = point.evaluations
  
  /** Returns the MOOPoint contained in the triplet */
  def getMOOPoint: MOOPoint = point
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithm: ComparativeAlgorithm = algorithm
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithmState: ComparativeAlgorithmState = algorithmState
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): Double = point.getEvaluation(functionIndex)
}

object MOGENTriplet {
  
  def apply(point: MOOPoint, algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState) = new MOGENTriplet(point, algorithm, algorithmState)
}
