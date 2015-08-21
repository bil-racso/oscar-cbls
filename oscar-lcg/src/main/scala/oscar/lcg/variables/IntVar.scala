package oscar.lcg.variables

import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint
import oscar.lcg.core.LCGStore
import oscar.lcg.support.Builder

abstract class IntVar {

  def min: Int
  
  def max: Int
  
  def size: Int
  
  def contains(value: Int): Boolean
  
  def isAssigned: Boolean
  
  def getValue: Int
  
  def isAssignedTo(value: Int): Boolean
  
  def updateMin(value: Int, explanation: Array[Literal], explanationSize: Int): Boolean 
  
  def updateMax(value: Int, explanation: Array[Literal], explanationSize: Int): Boolean 
  
  def awakeOnChanges(constraint: Constraint): Unit
  
  def leqLit(value: Int): Literal
  
  def geqLit(value: Int): Literal
  
  def store: LCGStore
  
  def name: String
  
  
  final def updateMin(value: Int, explanation: Array[Literal]): Boolean = {
    updateMin(value, explanation, explanation.length)
  }
  
  final def updateMax(value: Int, explanation: Array[Literal]): Boolean = {
    updateMax(value, explanation, explanation.length)
  }
  
  override def toString: String = {
    if (isAssigned) s"$name: [$min]"
    else s"$name: [$min, $max]"
  }
  //Added for API from the generated classes.
  final def updateub(i: Int, b: Builder): Boolean = {
      updateMax(i,b.array(),b.size());
  }
  final def updatelb(i: Int, b: Builder): Boolean = {
      updateMin(i,b.array(),b.size());
  }
  // This method removes a value only if it is one of the end points. 
  //It needs then to add that information in the explanation.
  final def remove(i: Int, b: Builder): Boolean = {
    if(i==min){
      b.add(geqLit(i))
      updateMin(i+1,b.array(),b.size());
    }else if(i==max){
      b.add(leqLit(i))
      updateMax(i-1,b.array(),b.size());
    }else true
  }
  final def assign(i: Int, b: Builder): Boolean = {
    updateMin(i,b.array(),b.size());
    updateMax(i,b.array(),b.size());
  }
}

object IntVar {
  def apply(min: Int, max: Int, name: String)(implicit lcgStore: LCGStore): IntVar = {
    new IntVarImpl(lcgStore, min, max, name)
  }
}