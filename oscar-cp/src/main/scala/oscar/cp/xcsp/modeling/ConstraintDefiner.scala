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