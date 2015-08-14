package oscar.lcg.literals

import oscar.lcg.core.Literal
import oscar.lcg.core.Clause
import oscar.algo.array.ArrayQueue

final object LitTrue extends Literal {

  override val varId: Int = -1
  
  override val litId: Int = -1
  
  override val isAssigned: Boolean = true
  
  override val isTrue: Boolean = true
  
  override val isFalse: Boolean = false
  
  override val opposite: Literal = LitFalse
    
  override def watch(clause: Clause): Unit = Unit
  
  override def clauses: ArrayQueue[Clause] = sys.error("True literal.")
  
  override def assign(explanation: Array[Literal], explanationSize: Int): Boolean = true
  
  override def assign(explanation: Literal): Boolean = true
  
  override def explain(explanation: Literal): Unit = Unit
}

final object LitFalse extends Literal {
  
  override val varId: Int = -1
  
  override val litId: Int = -1
  
  override val isAssigned: Boolean = true
  
  override val isTrue: Boolean = false
  
  override val isFalse: Boolean = true
  
  override val opposite: Literal = LitTrue
    
  override def watch(clause: Clause): Unit = Unit
  
  override def clauses: ArrayQueue[Clause] = sys.error("False literal.")
  
  override def assign(explanation: Array[Literal], explanationSize: Int): Boolean = false
  
  override def assign(explanation: Literal): Boolean = false
  
  override def explain(explanation: Literal): Unit = Unit
}