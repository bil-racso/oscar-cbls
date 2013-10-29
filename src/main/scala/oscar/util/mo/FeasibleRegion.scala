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

class FeasibleRegion(var feasibilityFunctions: List[Array[Double] => Boolean]) {
  def addFunction(newFunction: Array[Double] => Boolean) = feasibilityFunctions ::= newFunction
  
  def isFeasible(coordinates: Array[Double]): Boolean = {
    for (feasFctIndex <- 0 until feasibilityFunctions.length) {
      if(!feasibilityFunctions(feasFctIndex)(coordinates)) {
        feasibilityFunctions = feasibilityFunctions(feasFctIndex) :: feasibilityFunctions.take(feasFctIndex) ::: feasibilityFunctions.drop(feasFctIndex + 1)
        return false
      }
    }
    true
  }
}

object FeasibleRegion {
  def apply(feasibilityFunctions: List[Array[Double] => Boolean] = List((it: Array[Double]) => true)) = new FeasibleRegion(feasibilityFunctions)
}
