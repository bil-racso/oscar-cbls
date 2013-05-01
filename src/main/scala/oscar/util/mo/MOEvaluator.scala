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
package oscar.util.mo

import scala.collection.mutable.HashMap

class MOEvaluator[E <% Ordered[E]](var evalFunctions: Array[Double] => Array[E], val unfeasibleValue: Array[E]) {
  val evaluations = HashMap[Array[Double], MOOPoint[E]]()
  var nbCallToEvalFunction = 0
  
  def eval(coordinates: Array[Double], feasibleReg: FeasibleRegion): MOOPoint[E] = {
    evaluations.get(coordinates) match {
      case Some(point) => point
      case _ => {
        val newEvals = if (!feasibleReg.isFeasible(coordinates)) {
          unfeasibleValue
        }
        else {
          evalFunctions(coordinates)
        }
        val newPoint = MOOPoint(coordinates, newEvals)
        evaluations += coordinates -> newPoint
        nbCallToEvalFunction += 1
        newPoint
      }
    }
  }
}

object MOEvaluator {
  def apply[E <% Ordered[E]](evalFunctions: Array[Double] => Array[E], unfeasibleValue: Array[E]) = new MOEvaluator(evalFunctions, unfeasibleValue)
}
