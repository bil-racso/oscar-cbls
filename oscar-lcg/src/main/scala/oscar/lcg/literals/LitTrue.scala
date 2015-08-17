package oscar.lcg.literals

import oscar.lcg.core.Literal
import oscar.lcg.core.Clause
import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack

final object LitTrue extends Literal {
  
  override val isAssigned: Boolean = true
  
  override val isTrue: Boolean = true
  
  override val isFalse: Boolean = false
  
  override val opposite: Literal = LitFalse
    
  override def watch(clause: Clause): Unit = Unit
  
  override def clauses: ArrayQueue[Clause] = sys.error("True literal.")
  
  override def explanation: ArrayStack[Literal] = ???
  
  override def assign(): Boolean = true
  
  override def explain(): Unit = Unit
  
  override def explain(explanation: Literal): Unit = Unit
  
  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = Unit
}

final object LitFalse extends Literal {
  
  private[this] val _explanation = new ArrayStack[Literal](16)
  
  override val isAssigned: Boolean = true
  
  override val isTrue: Boolean = false
  
  override val isFalse: Boolean = true
  
  override val opposite: Literal = LitTrue
    
  override def watch(clause: Clause): Unit = Unit
  
  override def clauses: ArrayQueue[Clause] = sys.error("False literal.")
  
  override def explanation: ArrayStack[Literal] = _explanation
  
  override def assign(): Boolean = false
  
  override def explain(): Unit = {
    _explanation.clear()
  }

  override def explain(explanation: Literal): Unit = {
    _explanation.clear()
    _explanation.push(explanation)
  }

  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = {
    _explanation.clear()
    var i = explanationSize
    while (i > 0) { i -= 1; _explanation.push(explanation(i)) }
  }
}