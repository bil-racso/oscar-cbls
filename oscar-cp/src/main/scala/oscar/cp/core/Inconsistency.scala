package oscar.cp.core

/** @author Renaud Hartert ren.hartert@gmail.com */

abstract class Inconsistency extends Exception {
  
  def feedback: Any
  
  // Do not fill the trace
  final override val fillInStackTrace: Throwable = this 
  
  final override val toString: String = "Inconsistency"
}

/** A singleton `Inconsitency` with no feedback. */
object Inconsistency extends Inconsistency {
  
  override final val feedback = None

  /** Returns a new instance of `Inconsistency` with message as feedback. */
  final def apply(message: Any): Inconsistency = new Inconsistency { 
    override final val feedback = message 
  }
}