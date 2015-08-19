package oscar.lcg.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack

abstract class Literal {

  // Used by conflict analyzer
  var seen: Boolean = false
  var level: Int = -1

  def isAssigned: Boolean

  def isTrue: Boolean

  def isFalse: Boolean

  def opposite: Literal

  def watch(clause: Clause): Unit

  def clauses: ArrayQueue[Clause]

  def explanation: ArrayStack[Literal]

  /** 
   *  Assign the literal to true. 
   *  @return true if the operation succeeded, false otherwise.
   */
  def assign(): Boolean
  
  /** 
   *  Notify the domain that its value might have changed. 
   */
  def notifyAssign(): Unit

  def explain(): Unit 
  
  def explain(explanation: Literal): Unit

  def explain(explanation: Array[Literal], explanationSize: Int): Unit
  
  final def explain(explanation: Array[Literal]): Unit = {
    explain(explanation, explanation.length)
  }
}