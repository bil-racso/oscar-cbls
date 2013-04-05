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
class MOOPoint[T, E <% Ordered[E]](coordinates: T, evaluations: Array[E], maximization: Boolean = true) {
  
  var label = ""
  
  def this(coordinates: T, evaluator: (T => Array[E]), maximization: Boolean) = {
    this(coordinates, evaluator(coordinates), maximization)
  }
  
  /** The number of evaluations */
  def nbEvaluations: Int = evaluations.length
  
  def getEvaluation(functionIndex: Int): E = {
    evaluations(functionIndex)
  }
  
  
  
  def evaluationsToString: String = {
    var repr = "("
    for (i <- 0 until (nbEvaluations - 1)) {
      if (evaluations(i).toString.length() > 5) {
        repr += evaluations(i).toString.substring(0, 5) + ", "
      }
      else {
        repr += evaluations(i).toString + ", "
      }
    }
    if (evaluations(nbEvaluations - 1).toString.length() > 5) {
      repr += evaluations(nbEvaluations - 1).toString.substring(0, 5) + ")"
    }
    else {
      repr += evaluations(nbEvaluations - 1).toString + ")"
    }
    repr
  }
  
  override def toString: String = {
    //var repr = "Coordinates: " + coordinates.toString + "  ->  " + evaluationsToString
    var repr = "[" + label + "] " + evaluationsToString
    repr
  }
}

object MOOPoint {
  def apply[T, E <% Ordered[E]](coordinates: T, evaluations: Array[E], maximization: Boolean): MOOPoint[T, E] = {
    new MOOPoint(coordinates, evaluations, maximization)
  }
  
  def apply[T, E <% Ordered[E]](coordinates: T, evaluations: Array[E]): MOOPoint[T, E] = {
    new MOOPoint(coordinates, evaluations, true)
  }
  
  def apply[T, E <% Ordered[E]](coordinates: T, evaluator: (T => Array[E]), maximization: Boolean): MOOPoint[T, E] = {
    new MOOPoint(coordinates, evaluator, maximization)
  }
  
  def apply[T, E <% Ordered[E]](coordinates: T, evaluator: (T => Array[E])): MOOPoint[T, E] = {
    new MOOPoint(coordinates, evaluator, true)
  }
}