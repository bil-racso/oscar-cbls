package oscar.lcg.variables

import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint
import oscar.lcg.support.Builder
import oscar.lcg.literals.LitTrue

abstract class BooleanVar {
      
  def store: LCGStore
  
  def name: String
  
  def isAssigned: Boolean
  
  def isTrue: Boolean
  
  def isFalse: Boolean
  
  def assignTrue(explanation: Array[Literal], explanationSize: Int): Boolean
  
  def assignFalse(explanation: Array[Literal], explanationSize: Int): Boolean
  
  def assignTrue(): Boolean
  
  def assignFalse(): Boolean
  
  def awakeOnAssign(constraint: Constraint): Unit
  
  def eqLit: Literal
  
  def diffLit: Literal
  
  final def assign(value : Boolean, explanation: Array[Literal]): Boolean = {
    if (value) assignTrue(explanation)
    else assignFalse(explanation)
  }
  
  final def assignTrue(explanation: Array[Literal]): Boolean = {
    assignTrue(explanation, explanation.length)
  }
    
  final def assignFalse(explanation: Array[Literal]): Boolean = {
    assignFalse(explanation, explanation.length)
  }
  
  //Basically the following methods make a Boolean act as an integer. 
  final def awakeOnChanges(constraint: Constraint): Unit = awakeOnAssign(constraint)
  
  final def min(): Int = if(isAssigned && isTrue) 1 else 0
  final def max(): Int = if(isAssigned && isFalse) 0 else 1
  
  final def updateub(i: Int, b: Builder): Boolean = {
    if(i==0){
      assignFalse(b.array(),b.size());
    }else true
  }
  final def updatelb(i: Int, b: Builder): Boolean = {
    if(i==1){
      assignTrue(b.array(),b.size());
    }else true
  }
  final def assign(value : Int, b: Builder): Boolean = {
    assert(value==0 || value ==1)
    if (value == 1) assignTrue(b.array(),b.size())
    else assignFalse(b.array(),b.size())
  }
  final def remove(value : Int, b: Builder): Boolean = {
    assert(value==0 || value ==1)
    if (value == 1) assignFalse(b.array(),b.size())
    else assignTrue(b.array(),b.size())
  }
  final def geqLit(i:Int): Literal = {
    if(i==1){
      eqLit
    }else LitTrue
  }
  final def leqLit(i:Int): Literal = {
    if(i==0){
      diffLit
    }else LitTrue
  }
  final def getValue: Int = {
    assert(isAssigned)
    if(isTrue) 1 else 0
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

