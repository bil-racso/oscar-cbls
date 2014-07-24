package oscar.cp.xcsp.modeling

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint

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