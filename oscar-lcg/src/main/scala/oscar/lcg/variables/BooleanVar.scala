package oscar.lcg.variables

import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint

abstract class BooleanVar {
      
  def store: LCGStore
  
  def name: String
  
  def isAssigned: Boolean
  
  def isTrue: Boolean
  
  def isFalse: Boolean
  
  def assignTrue(explanation: Array[Literal]): Boolean
  
  def assignFalse(explanation: Array[Literal]): Boolean
  
  def awakeOnAssign(constraint: Constraint): Unit
  
  def eqLit: Literal
  
  def diffLit: Literal
  
  final def assign(value : Boolean, explanation: Array[Literal]): Boolean = {
    if (value) assignTrue(explanation)
    else assignFalse(explanation)
  }
  
  override def toString: String = {
    if (isAssigned) s"$name: {$isTrue}"
    else s"$name: {true, false}"
  }
}

object BooleanVar {
  def apply(name: String)(implicit lcgStore: LCGStore): BooleanVar = {
    new BooleanVarImpl(lcgStore, name)
  }
}

