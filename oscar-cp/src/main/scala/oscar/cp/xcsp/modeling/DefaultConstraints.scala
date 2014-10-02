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
 * *****************************************************************************/

package oscar.cp.xcsp.modeling

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.modeling._


trait DefaultConstraints extends ConstraintDefiner {
	def allDifferent(vars: Iterable[CPIntVar]) : Constraint = oscar.cp.modeling.allDifferent(vars)
	def weightedSum(w: Array[Int], x: Array[CPIntVar], y: Int) : Constraint = oscar.cp.modeling.weightedSum(w,x,y)
	def table(x: Array[CPIntVar], tuples: Array[Array[Int]]) : Constraint = oscar.cp.modeling.table(x,tuples)
	def among(n: CPIntVar, x: IndexedSeq[CPIntVar], s: Set[Int]) : Constraint = oscar.cp.modeling.among(n,x,s)
	def atLeast(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint = oscar.cp.modeling.atLeast(n,x,v) 
	def atMost(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint = oscar.cp.modeling.atMost(n,x,v) 
	def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar) : Constraint = oscar.cp.modeling.maxCumulativeResource(starts,durations,ends,demands,capacity) 
	def disjunctive(starts: Array[CPIntVar], durations: Array[CPIntVar]) : Constraint = {
	  val ends = (0 until starts.length).map(a => starts(a)+durations(a)).toArray
	  oscar.cp.modeling.unaryResource(starts, durations, ends)
	}
	def element(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: CPIntVar) : Constraint = oscar.cp.modeling.elementVar(tab,x,z)
	def globalCardinality(x: Array[CPIntVar], valueOccurrence: Array[(Int,CPIntVar)]) : Constraint = oscar.cp.modeling.gcc(x, valueOccurrence)
	def minimumWeightAllDifferent(x: Array[CPIntVar], weights: Array[Array[Int]], cost: CPIntVar) : Constraint = oscar.cp.modeling.minAssignment(x, weights, cost)
}