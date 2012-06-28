/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
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
