package oscar.lcg.literals

import oscar.algo.array.ArrayQueue
import oscar.lcg.core.Literal
import oscar.lcg.variables.IntVar
import oscar.lcg.core.Clause
import oscar.algo.array.ArrayStack
import oscar.lcg.core.ConflictAnalyzer

class LitLeq(intVar: IntVar, value: Int) extends Literal {

  private[this] val _clauses = new ArrayQueue[Clause](32)
  private[this] val _explanation = new ArrayStack[Literal](16)

  @inline override def isAssigned: Boolean = intVar.max <= value || intVar.min > value

  @inline override def isTrue: Boolean = intVar.max <= value

  @inline override def isFalse: Boolean = intVar.min > value

  @inline override val opposite = new LitGr(this, intVar, value)

  @inline override def watch(clause: Clause): Unit = _clauses.addLast(clause)

  @inline override def clauses: ArrayQueue[Clause] = _clauses

  @inline override def explanation: ArrayStack[Literal] = _explanation

  override def toString: String = s"[${intVar.name} <= $value]"

  override def assign(): Boolean = {
    if (intVar.max <= value) true // The literal is already assigned
    else if (intVar.min > value) {
      // Need to tell the conflict analyzer
      false
    } else {
      // Need to store the explanation
      // Need to register the level of explanation
      // Need to notify the clauses
      // Need to notify the variable 

      // Notify the variable
      intVar.updateMax(value, Array.empty)
      true
    }
  }
   
  override def notifyAssign(): Unit = Unit
  
  override def explain(): Unit = {
    
  }

  override def explain(explanation: Literal): Unit = {
    ???
  }

  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = {
    ???
  }
}

class LitGr(@inline override val opposite: LitLeq, intVar: IntVar, value: Int) extends Literal {

  private[this] val _clauses = new ArrayQueue[Clause](32)
  private[this] val _explanation = new ArrayStack[Literal](16)

  @inline override def isAssigned: Boolean = intVar.min > value || intVar.max <= value

  @inline override def isTrue: Boolean = intVar.min > value

  @inline override def isFalse: Boolean = intVar.max <= value

  @inline override def watch(clause: Clause): Unit = _clauses.addLast(clause)

  @inline override def clauses: ArrayQueue[Clause] = _clauses
  
  @inline override def explanation: ArrayStack[Literal] = _explanation

  override def toString: String = s"[${intVar.name} > $value]"

  override def assign(): Boolean = {
    if (intVar.max <= value) true // The literal is already assigned
    else if (intVar.min > value) {
      // Need to tell the conflict analyzer
      false
    } else {
      // Need to store the explanation
      // Need to register the level of explanation
      // Need to notify the clauses
      // Need to notify the variable 

      // Notify the variable
      intVar.updateMin(value + 1, Array.empty)
      true
    }
  }
  
  override def notifyAssign(): Unit = Unit
    
  override def explain(): Unit = {
    
  }

  override def explain(explanation: Literal): Unit = {
    ???
  }

  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = {
    ???
  }
}