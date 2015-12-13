package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt
import oscar.sat.core.CDCLStore

abstract class Clause extends Constraint {

  def activity: Double
  
  def activity_=(d: Double): Unit

  def remove(): Unit

  def simplify(): Boolean
  
  def explain(outReason: ArrayStackInt): Unit
  
  def explainAll(outReason: ArrayStackInt): Unit

  def propagate(literal: Int): Boolean
}

object Clause {
  
  def apply(solver: CDCLStore, literals: Array[Int], learnt: Boolean): Clause = {
    if (literals.length == 2) binary(solver, literals(0), literals(1), learnt)
    else watchLiterals(solver, literals, learnt)
  }
  
  def watchLiterals(solver: CDCLStore, literals: Array[Int], learnt: Boolean): Clause = {
    new WLClause(solver, literals, learnt)
  }
  
  def binary(solver: CDCLStore, lit1: Int, lit2: Int, learnt: Boolean): Clause = {
    new BinaryClause(solver, lit1, lit2, learnt)
  }
}