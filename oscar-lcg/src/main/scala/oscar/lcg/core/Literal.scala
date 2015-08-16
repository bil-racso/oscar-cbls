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
  
  def assign(): Boolean
  
  def explain(explanation: Literal): Unit
  
  def explain(explanation: Array[Literal], explanationSize: Int): Unit
  
  final def assign(explanation: Array[Literal], explanationSize: Int): Boolean = {
    explain(explanation, explanationSize)
    assign()
  }
  
  final def assign(explanation: Array[Literal]): Boolean = {
    explain(explanation, explanation.length)
    assign()
  }
    
  final def assign(explanation: Literal): Boolean = {
    explain(explanation)
    assign()
  }
}