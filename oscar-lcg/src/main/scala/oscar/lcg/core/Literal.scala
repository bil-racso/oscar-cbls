package oscar.lcg.core

import oscar.algo.array.ArrayQueue

abstract class Literal {

  def varId: Int
  
  def litId: Int
  
  def isAssigned: Boolean
  
  def isTrue: Boolean 
  
  def isFalse: Boolean
  
  def opposite: Literal
    
  def watch(clause: Clause): Unit
  
  def clauses: ArrayQueue[Clause]
  
  def assign(explanation: Array[Literal], explanationSize: Int): Boolean 
  
  def assign(explanation: Literal): Boolean
  
  def explain(explanation: Literal): Boolean
  
  final def assign(explanation: Array[Literal]): Boolean = {
    assign(explanation, explanation.length)
  }
}