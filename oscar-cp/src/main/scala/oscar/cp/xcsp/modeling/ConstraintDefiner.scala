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

package oscar.cp.xcsp.modeling

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint

trait ConstraintDefiner {
  def allDifferent(vars: Iterable[CPIntVar]) : Constraint 
  def weightedSum(w: Array[Int], x: Array[CPIntVar], y: Int) : Constraint
  def table(x: Array[CPIntVar], tuples: Array[Array[Int]]) : Constraint
  def among(n: CPIntVar, x: IndexedSeq[CPIntVar], s: Set[Int]) : Constraint
  def atLeast(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint
  def atMost(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint
  def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar) : Constraint
  def disjunctive(starts: Array[CPIntVar], durations: Array[CPIntVar]) : Constraint
  def element(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: CPIntVar) : Constraint
  def globalCardinality(x: Array[CPIntVar], valueOccurrence: Array[(Int,CPIntVar)]) : Constraint
  def minimumWeightAllDifferent(x: Array[CPIntVar], weights: Array[Array[Int]], cost: CPIntVar) : Constraint
  
  
}