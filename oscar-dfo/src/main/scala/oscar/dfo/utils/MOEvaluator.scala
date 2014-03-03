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

import scala.collection.mutable.HashMap

class MOEvaluator(var evalFunctions: Array[Double] => Array[Double], val unfeasibleValue: Array[Double]) {
  var nbCallToEvalFunction = 0
  
  def eval(coordinates: Array[Double], feasibleReg: FeasibleRegion): MOOPoint = {
    val newEvals = if (!feasibleReg.isFeasible(coordinates)) {
      unfeasibleValue
    }
    else {
      evalFunctions(coordinates)
    }
    val newPoint = MOOPoint(coordinates, newEvals)
    nbCallToEvalFunction += 1
    newPoint
  }
}

object MOEvaluator {
  def apply(evalFunctions: Array[Double] => Array[Double], unfeasibleValue: Array[Double]) = new MOEvaluator(evalFunctions, unfeasibleValue)
}
