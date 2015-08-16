package oscar.lcg.core

import oscar.algo.reversible.ReversibleBoolean

abstract class Constraint {
  
  private[this] var _idempotent = false
  private[this] var _enqueued = false
  private[this] var _inPropagate = false

  def store: LCGStore  
  
  def name: String
  
  def setup(): Boolean
  
  def explain(): Unit
  
  protected def filter(): Boolean
  
  final def idempotent_=(b: Boolean): Unit = _idempotent = b
  
  final def idempotent: Boolean = _idempotent
  
  final def enqueued_=(b: Boolean): Unit = _enqueued = b
  
  final def enqueued: Boolean = _enqueued
  
  final def isEnqueuable: Boolean = !_enqueued && (!_inPropagate || !_idempotent)
    
  final def propagate(): Boolean = {
    _inPropagate = true
    val out = filter()
    _inPropagate = false
    out
  }
}