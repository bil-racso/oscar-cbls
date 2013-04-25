/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.util.mo

import com.sun.org.apache.xpath.internal.operations.Plus
import scala.reflect.ClassTag

/** Point with its coordinates and its associated evaluations.
  *
  * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
  * 
  * @constructor Creates a new point with the coordinates and associated evaluations
  *              passed as argument.
  * @param coordinates An array of size n representing a point in the input space
  * @param evaluations An array of size m representing the evaluations in the output space
  *                    of the coordinates passed as argument
  * @param maximization A Boolean which is true when the point is part of a maximization 
  *                     problem, false otherwise. Default value: true */
class MOOPoint[E <% Ordered[E]](val coordinates: Array[Double], val evaluations: Array[E]) extends ArchiveElement[E] {
  
  def this(coordinates: Array[Double], evaluator: (Array[Double] => Array[E])) = {
    this(coordinates, evaluator(coordinates))
  }
  
  /** Returns the MOOPoint contained in the archive element */
  def getMOOPoint: MOOPoint[E] = this
  
  /** The number of evaluations */
  def nbEvaluations: Int = evaluations.length
  
  /** The number of coordinates */
  def nbCoordinates: Int = coordinates.length
  
  def getEvaluation(functionIndex: Int): E = {
    evaluations(functionIndex)
  }
  
  var iter = 0
  
  def equals(other: MOOPoint[E]): Boolean = {
    for(evalIndex <- 0 until evaluations.length) {
      if(evaluations(evalIndex) != other.evaluations(evalIndex)) return false
    }
    true
  }
  
  override def toString: String = {
    "[" + iter + "] Coordinates: (" + coordinates.mkString(", ") + ")   ->   Evaluations: (" + evaluations.mkString(", ") + ")"
  }
}

object MOOPoint {
  def apply[E <% Ordered[E]](coordinates: Array[Double], evaluations: Array[E]): MOOPoint[E] = {
    new MOOPoint(coordinates, evaluations)
  }
  
  def apply[E <% Ordered[E]](coordinates: Array[Double], evaluator: (Array[Double] => Array[E])): MOOPoint[E] = {
    new MOOPoint(coordinates, evaluator)
  }
}