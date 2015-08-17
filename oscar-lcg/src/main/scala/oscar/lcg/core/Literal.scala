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

  def explain(): Unit 
  
  def explain(explanation: Literal): Unit

  def explain(explanation: Array[Literal], explanationSize: Int): Unit

  final def assign(explanation: Array[Literal], explanationSize: Int): Boolean = {
    val out = assign()
    explain(explanation, explanationSize)
    out
  }

  final def assign(explanation: Array[Literal]): Boolean = {
    val out = assign()
    explain(explanation, explanation.length)
    out
  }

  final def assign(explanation: Literal): Boolean = {
    val out = assign()
    explain(explanation)
    out
  }
}