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

class MOGENTriplet[E <% Ordered[E]](point: MOOPoint[E], algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState[E]) extends ArchiveElement[E] {
  /** Returns the MOOPoint contained in the triplet */
  def getMOOPoint: MOOPoint[E] = point
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithm: ComparativeAlgorithm = algorithm
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithmState: ComparativeAlgorithmState[E] = algorithmState
  
  /** The number of evaluations contained in the MOOPoint of the triplet */
  def nbEvaluations: Int = point.nbEvaluations
  
  /** The number of coordinates contained in the MOOPoint of the triplet */
  def nbCoordinates: Int = point.nbCoordinates
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): E = point.getEvaluation(functionIndex)
}

object MOGENTriplet {
  
  def apply[E <% Ordered[E]](point: MOOPoint[E], algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState[E]) = new MOGENTriplet(point, algorithm, algorithmState)
}
