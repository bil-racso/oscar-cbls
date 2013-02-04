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

package oscar.dfo.utils

/** Point with its coordinates and its associated evaluations.
  *
  * @constructor Creates a new point with the coordinates and associated evaluations
  *              passed as argument.
  * @param coordinates An array of double of size n representing a point in R^n
  * @param evaluations An array of double of size m representing the evaluations in R^m
  *                    of the coordinates passed as argument */
class DFOPoint(coordinates: Array[Double], evaluations: Array[Double]) {
  
  /** Point with its coordinates and its evaluations determined by the evaluator
    * passed as argument.
    * 
    * @constructor Creates a new point with the coordinates specified as argument and
    *              the evaluations determined by applying the evaluator passed as
    *              argument on the coordinates.
    * @param coordinates An array of double of size n representing a point in R^n 
    * @param evaluator A function taking as argument an array which represents coordinates
    *                  and returns an array representing the evaluations of the coordinates.
    *                  The evaluations of the point are determined by this function passing
    *                  coordinates as argument */
  def this(coordinates: Array[Double], evaluator: Array[Double] => Array[Double]) = {
    this(coordinates, evaluator(coordinates))
  }
  
  /** Returns the evaluation of the function which index is passed as argument.
    *
    * @param functionIndex The index of the function which evaluation is queried
    * @return A double which is the evaluation of the point for the function which
    *         index was passed as argument */
  def getEval(functionIndex: Int): Double = {
    require(functionIndex >= 0, println("The index passed as argument must be positive"))
    require(functionIndex < evaluations.length, println("The index passed as argument" +
        "must not exceed the number of evaluation functions"))
    evaluations(functionIndex)
  }
  
  /** Returns the dimension of the input space of the point.
    * 
    * @return An Int which is the dimension of the input space of the point*/
  def getDimension: Int = {
    coordinates.length
  }
  
  /** Returns the dimension of the output space of the point (i.e. nb of evaluations).
    * 
    * @return An Int which is the number of function of evaluation on which the
    *         point has been evaluated */
  def getNbEvaluations: Int = {
    coordinates.length
  }
}

/** Factory for the QuasiRandomSequence instances. */
object DFOPoint {
  
  /** Creates a new point with the coordinates and associated evaluations
    *              passed as argument.
    * @param coordinates An array of double of size n representing a point in R^n
    * @param evaluations An array of double of size m representing the evaluations in R^m
    *                    of the coordinates passed as argument */
  def apply(coordinates: Array[Double], evaluations: Array[Double]) = new DFOPoint(coordinates, evaluations)
  
  /** Creates a new point with the coordinates specified as argument and
    *              the evaluations determined by applying the evaluator passed as
    *              argument on the coordinates.
    * @param coordinates An array of double of size n representing a point in R^n 
    * @param evaluator A function taking as argument an array which represents coordinates
    *                  and returns an array representing the evaluations of the coordinates.
    *                  The evaluations of the point are determined by this function passing
    *                  coordinates as argument */
  def apply(coordinates: Array[Double], evaluator: Array[Double] => Array[Double]) = new DFOPoint(coordinates, evaluator)
}