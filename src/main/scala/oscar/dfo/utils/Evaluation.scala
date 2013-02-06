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

trait Evaluation {
  
  //Returns the evaluations of the function f at the point p.
  def eval(f: Array[Double] => Array[Double], point: Array[Double]): (Array[Double], Int) = {
    (f(point), 1)
  }
  
}


class DeterministicEvaluation extends Evaluation

// to implement ....
class StochasticEvaluation extends Evaluation
