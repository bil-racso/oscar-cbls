package oscar.sat.constraints

import oscar.sat.core.CDCLStore
import scala.collection.mutable.ArrayBuffer
import oscar.sat.core.Literal
import oscar.sat.core.True
import oscar.sat.core.False

class Clause(solver: CDCLStore, literals: Array[Literal], learnt: Boolean) {

  var activity: Double = 0
  
  def locked: Boolean = solver.assignReason(literals(0).varId) == this

  def remove(): Unit = Unit

  def simplify(): Boolean = true
  
  def explain(outReason: ArrayBuffer[Literal]): Unit = {
    var i = 1
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
    if (learnt) solver.claBumpActivity(this)
  }
  
  def explainAll(outReason: ArrayBuffer[Literal]): Unit = {
        var i = 0
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
    if (learnt) solver.claBumpActivity(this)
  }

  def propagate(literal: Literal): Boolean = {
    // Make sure the false literal is literals(1)
    if (literals(0) == literal.opposite) {
      literals(0) = literals(1)
      literals(1) = literal.opposite
    }

    // If 0th watch is true, then clause is already satisfied
    if (solver.value(literals(0)) == True) {
      solver.watch(this, literal)
      return true
    }

    // Look for a new literal to watch
    var i = 2
    while (i < literals.length) {
      if (solver.value(literals(i)) != False) {
        literals(1) = literals(i)
        literals(i) = literal.opposite
        solver.watch(this, literals(1).opposite)
        return true
      }
      i += 1
    }
    
    // Clause is unit under assignment
    solver.watch(this, literal)
    solver.enqueue(literals(0), this)
  }
  
  final override def toString: String = literals.mkString(" ")

}