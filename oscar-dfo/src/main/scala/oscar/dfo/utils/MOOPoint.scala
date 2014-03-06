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
package oscar.dfo.utils

import oscar.algo.paretofront.ParetoElement

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
class MOOPoint(val coords: Array[Double], val evaluations: Array[Double]) extends ParetoElement[Double] {
  
  def coordinates = coords
  
  def objectives = evaluations
  
  override def clone() = MOOPoint(coordinates.clone(), evaluations.clone())
  
  def this(coordinates: Array[Double], evaluator: (Array[Double] => Array[Double])) = {
    this(coordinates, evaluator(coordinates))
  }
  
  /** Returns the MOOPoint contained in the archive element */
  def getMOOPoint: MOOPoint = this
  
  def getEvaluation(functionIndex: Int): Double = {
    evaluations(functionIndex)
  }
  
  def equals(other: MOOPoint): Boolean = {
    for(evalIndex <- 0 until evaluations.length) {
      if(evaluations(evalIndex) != other.evaluations(evalIndex)) return false
    }
    true
  }
  
  override def toString: String = {
    "Coordinates: (" + coordinates.mkString(", ") + ")   ->   Evaluations: (" + evaluations.mkString(", ") + ")"
  }
}

object MOOPoint {
  def apply(coordinates: Array[Double], evaluations: Array[Double]): MOOPoint = {
    new MOOPoint(coordinates, evaluations)
  }
  
  def apply(coordinates: Array[Double], evaluator: (Array[Double] => Array[Double])): MOOPoint = {
    new MOOPoint(coordinates, evaluator)
  }
}
