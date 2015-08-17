package oscar.lcg.literals

import oscar.algo.array.ArrayQueue
import oscar.lcg.core.Literal
import oscar.lcg.core.Clause
import oscar.algo.array.ArrayStack
import oscar.lcg.core.ConflictAnalyzer
import oscar.lcg.variables.BooleanVar

class LitBooleanTrue(booleanVar: BooleanVar) extends Literal {

  private[this] val store = booleanVar.store

  private[this] val _clauses = new ArrayQueue[Clause](32)
  private[this] val _explanation = new ArrayStack[Literal](16)

  @inline override def isAssigned: Boolean = booleanVar.isAssigned

  @inline override def isTrue: Boolean = booleanVar.isTrue

  @inline override def isFalse: Boolean = booleanVar.isFalse

  @inline override val opposite = new LitBooleanFalse(this, booleanVar)

  @inline override def watch(clause: Clause): Unit = _clauses.addLast(clause)

  @inline override def clauses: ArrayQueue[Clause] = _clauses

  @inline override def explanation: ArrayStack[Literal] = _explanation

  override def toString: String = {
    if (booleanVar.isAssigned) s"[${booleanVar.name}]: ${isTrue}"
    else s"[${booleanVar.name}]: _"
  }

  override def assign(): Boolean = {
    explain()
    if (!booleanVar.isAssigned) {
      booleanVar.assignTrue(Array.empty)
      store.enqueue(this)
      true
    } else if (booleanVar.isTrue) true
    else {
      store.failedLiteral = this
      false
    }
  }
  
  override def explain(): Unit = {
    _explanation.clear()
    store.explained(this)
  }

  override def explain(explanation: Literal): Unit = {
    _explanation.clear()
    _explanation.push(explanation)
    store.explained(this)
  }

  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = {
    _explanation.clear()
    var i = explanationSize
    while (i > 0) { i -= 1; _explanation.push(explanation(i)) }
    store.explained(this)
  }
}

class LitBooleanFalse(@inline override val opposite: LitBooleanTrue, booleanVar: BooleanVar) extends Literal {

  private[this] val store = booleanVar.store
  
  private[this] val _clauses = new ArrayQueue[Clause](32)
  private[this] val _explanation = new ArrayStack[Literal](16)

  @inline override def isAssigned: Boolean = booleanVar.isAssigned

  @inline override def isTrue: Boolean = booleanVar.isFalse

  @inline override def isFalse: Boolean = booleanVar.isTrue

  @inline override def watch(clause: Clause): Unit = _clauses.addLast(clause)

  @inline override def clauses: ArrayQueue[Clause] = _clauses

  @inline override def explanation: ArrayStack[Literal] = _explanation

  override def toString: String = {
    if (booleanVar.isAssigned) s"[!${booleanVar.name}]: ${isTrue}"
    else s"[!${booleanVar.name}]: _"
  }

  override def assign(): Boolean = {
    explain()
    if (!booleanVar.isAssigned) {
      booleanVar.assignFalse(Array.empty)
      store.enqueue(this)
      true
    } else if (booleanVar.isFalse) true
    else {
      store.failedLiteral = this
      false
    }
  }
  
  override def explain(): Unit = {
    _explanation.clear()
    store.explained(this)
  }

  override def explain(explanation: Literal): Unit = {
    _explanation.clear()
    _explanation.push(explanation)
    store.explained(this)
  }

  override def explain(explanation: Array[Literal], explanationSize: Int): Unit = {
    _explanation.clear()
    var i = explanationSize
    while (i > 0) { i -= 1; _explanation.push(explanation(i)) }
    store.explained(this)
  }
}