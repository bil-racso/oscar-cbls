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
package oscar.cp.constraints

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._
import oscar.cp.modeling._
import oscar.algo.reversible.ReversibleSparseSet



/**
 * Circuit constraint
 * Ensures that succ represents a valid circuit. <br>
 * succ[i] represents the city visited after city i. Each city is visited once and
 * there is only one tour.<br>
 * Available propagation strengths are Weak (default) and Strong.
 * @param succ
 * @see CPPropagStrength
 * @author Pierre Schaus pschaus@gmail.com
 */
class Circuit(val succ: Array[CPIntVar], addPredModel: Boolean = true) extends Constraint(succ(0).store, "Circuit") {
  val n = succ.length
  val dest = Array.tabulate(n)(i => new ReversibleInt(s,i))
  val orig = Array.tabulate(n)(i => new ReversibleInt(s,i))
  val lengthToDest = Array.fill(n)(new ReversibleInt(s,0))
  

  override def setup(l: CPPropagStrength): CPOutcome = {

    if (s.post(new AllDifferent(succ:_*), l) == CPOutcome.Failure) {
      return CPOutcome.Failure
    }
    if (n > 0) {
      for (i <- 0 until succ.length) {
        if (succ(i).removeValue(i) == CPOutcome.Failure) {
          return CPOutcome.Failure;
        }
      }
    }
    for (i <- 0 until n) {
      if (succ(i).isBound) {
        if (valBindIdx(succ(i), i) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      } else {
        succ(i).callValBindIdxWhenBind(this, i)
      }
    }
    if (addPredModel) {
      val pred = Array.fill(n)(CPIntVar(0 until n)(s))
      if (s.post(new Inverse(pred, succ), l) == Failure) return Failure
      if (s.post(new Circuit(pred, false),l) == Failure) return Failure
    }    
    return CPOutcome.Suspend
  }
  
  
  override def valBindIdx(x: CPIntervalVar, i: Int): CPOutcome = {
		val j = x.value
		// We have a new assigned path because of new edge i->j:
		// o *-> i -> j *-> d
		val d = dest(j).value
		val o = orig(i).value
		// maintain the property
		dest(o) := d
		orig(d) := o
		val lengthOrigDest = lengthToDest(o).value + lengthToDest(j).value + 1
		lengthToDest(o) := lengthOrigDest
		if (lengthOrigDest < n-1) {
			// otherwise we would have a closed loop with less than n-1 edges
			return succ(d).removeValue(o);
		} else {
			return CPOutcome.Suspend;
		}	
	}  

}