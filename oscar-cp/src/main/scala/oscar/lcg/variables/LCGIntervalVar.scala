package oscar.lcg.variables

import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.lcg.core.CDCLStore
import oscar.lcg.core.Literal
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.core.LCGSolver
import scala.util.Random

abstract class LCGIntervalVar extends LCGVar {

  /** Return the CP Store. */
  def cpStore: CPStore
  
  /** Return the LCG store. */
  def cdclStore: CDCLStore
  
  /** Return the minimum value in the domain. */
  def min: Int
  
  /** Return the maximum value in the domain. */
  def max: Int
  
  /** Return the size of the domain. */
  def size: Int
  
  /** Return true if the variable is assigned. */
  def isAssigned: Boolean
  
  /** Return true if the variable is assigned to value. */
  def isAssignedTo(value: Int): Boolean
  
  /** Return true if the domain contains value.*/
  def contains(value: Int): Boolean
    
  /** Return the literal `value <= this`. */
  def greaterEqual(value: Int): Literal

  /** Return the literal `this <= value`. */
  def lowerEqual(value: Int): Literal
  
  /** Register the constraint on bounds changes. */
  def callWhenBoundsChange(constraint: LCGConstraint): Unit
  
  /** Register the constraint on assignments. */
  def callWhenAssigned(constraint: LCGConstraint): Unit
  
  /** Return the name of the variable. */
  def name: String
}

object LCGIntervalVar {
  
  private[this] var id = 0
  @inline private def nextId(): Int = {
    val i = id; id += 1; i
  }
  
  def apply(initMin: Int, initMax: Int, name: String = "")(implicit cdclStore: CDCLStore, store: CPStore): LCGIntervalVar = {
    new LCGIntervalVarImpl(cdclStore, store, nextId(), initMin, initMax, name)
  }
}