package oscar.lcg.variables

import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint

abstract class IntVar {

  def min: Int
  
  def max: Int
  
  def size: Int
  
  def contains(value: Int): Boolean
  
  def isAssigned: Boolean
  
  def isAssignedTo(value: Int): Boolean
  
  def updateMin(value: Int, explanation: Array[Literal]): Boolean
  
  def updateMax(value: Int, explanation: Array[Literal]): Boolean
  
  def awakeOnChanges(constraint: Constraint): Unit
  
  def leqLit(value: Int): Literal
  
  def geqLit(value: Int): Literal
  
  def update(): Unit
}