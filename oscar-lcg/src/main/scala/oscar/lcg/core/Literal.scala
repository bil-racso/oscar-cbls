package oscar.lcg.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack

abstract class Literal {
  
  // Used by conflict analyzer
  var seen: Boolean = false
  var level: Int = -1

  def varId: Int
  
  def litId: Int
  
  def isAssigned: Boolean
  
  def isTrue: Boolean 
  
  def isFalse: Boolean
  
  def opposite: Literal
    
  def watch(clause: Clause): Unit
  
  def clauses: ArrayQueue[Clause]
  
  def explanation: ArrayStack[Literal]
  
  def assign(explanation: Array[Literal], explanationSize: Int): Boolean 
  
  def assign(explanation: Literal): Boolean
  
  def explain(explanation: Literal): Unit
  
  final def assign(explanation: Array[Literal]): Boolean = {
    assign(explanation, explanation.length)
  }
}