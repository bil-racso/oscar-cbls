package oscar.cp.lcg.variables

import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.core.Literal
import scala.util.Random
import oscar.cp.lcg.constraints.LCGConstraint
import oscar.cp.lcg.core.LCGSolver

abstract class LCGIntervalVar {

  /** Return the CP Store. */
  def store: CPStore
  
  /** Return the LCG store. */
  def lcgStore: LCGStore
  
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
    
  /** Return the literal `value <= this.min`. */
  def minGeq(value: Int): Literal

  /** Return the literal `this.max <= value`. */
  def maxLeq(value: Int): Literal
  
  /** Update the domain and notify the constraints. */
  def updateAndNotify(): Unit
  
  /** Register the constraint on bounds changes. */
  def callWhenBoundsChange(constraint: LCGConstraint): Unit
  
  /** Return the name of the variable. */
  def name: String
}

object LCGIntervalVar {
  
  private[this] var id = 0
  @inline private def nextId(): Int = {
    val i = id; id += 1; i
  }
  
  def apply(initMin: Int, initMax: Int, name: String = "")(implicit lcgStore: LCGSolver, store: CPStore): LCGIntervalVar = {
    new LCGIntervalVarImpl(lcgStore.lcgStore, store, nextId(), initMin, initMax, name)
  }
}