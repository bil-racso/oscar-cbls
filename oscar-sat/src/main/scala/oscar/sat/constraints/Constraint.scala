package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt

abstract class Constraint {

  def simplify(): Boolean
  
  def explain(outReason: ArrayStackInt): Unit
  
  def explainAll(outReason: ArrayStackInt): Unit

  def propagate(literal: Int): Boolean
  
  def isDeleted = false
}