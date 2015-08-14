package oscar.lcg.literals

import oscar.algo.array.ArrayQueue
import oscar.lcg.core.Literal
import oscar.lcg.variables.IntVar
import oscar.lcg.core.Clause
import oscar.algo.array.ArrayStack


final class LitLeq(@inline override val varId: Int, intVar: IntVar, value: Int) extends Literal {
  
  private[this] val _clauses = new ArrayQueue[Clause](32)
  private[this] val _explanation = new ArrayStack[Literal](16)
  
  @inline override val litId: Int = varId * 2
  
  @inline override def isAssigned: Boolean = intVar.max <= value || intVar.min > value
  
  @inline override def isTrue: Boolean = intVar.max <= value
  
  @inline override def isFalse: Boolean = intVar.min > value
  
  @inline override val opposite = new LitGr(this, intVar, value)
  
  @inline override def watch(clause: Clause): Unit = _clauses.addLast(clause)
  
  @inline override def clauses: ArrayQueue[Clause] = _clauses
  
  override def assign(explanation: Array[Literal], explanationSize: Int): Boolean = {
    if (intVar.max <= value) true // The literal is already assigned
    else if (intVar.min > value) {
      // Need to tell the conflict analyzer
      false
    } else {
      // Need to store the explanation
      // Need to register the level of explanation
      // Need to notify the clauses
      // Need to notify the variable 
      true
    }
  }
  
  override def assign(explanation: Literal): Boolean = {
    ???
  }
  
  /** */
  override def explain(explanation: Literal): Boolean = {
    assert(isTrue)
    assert(_explanation.isEmpty)
    _explanation.push(explanation)
    true
  }
}

final class LitGr(@inline override val opposite: LitLeq, intVar: IntVar, value: Int) extends Literal {
  
  private[this] val watchers = new ArrayQueue[Clause](32)
  
  @inline override val varId: Int = opposite.varId
  
  @inline override val litId: Int = opposite.litId + 1
  
  @inline override def isAssigned: Boolean = intVar.min > value || intVar.max <= value
  
  @inline override def isTrue: Boolean = intVar.min > value
  
  @inline override def isFalse: Boolean = intVar.max <= value
  
  @inline override def watch(clause: Clause): Unit = clauses.addLast(clause)
  
  @inline override def clauses: ArrayQueue[Clause] = watchers
  
  override def assign(explanation: Array[Literal], explanationSize: Int): Boolean = {
    if (intVar.min > value) true // The literal is already assigned
    else if (intVar.max <= value) {
      // Need to tell the conflict analyzer
      false
    } else {
      // Need to store the explanation
      // Need to register the level of explanation
      // Need to notify the clauses
      // Need to notify the variable 
      true
    }
  }
  
  override def assign(explanation: Literal): Boolean = {
    ???
  }
}