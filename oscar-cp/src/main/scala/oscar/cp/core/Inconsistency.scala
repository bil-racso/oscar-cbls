package oscar.cp.core

/** @author Renaud Hartert ren.hartert@gmail.com */

abstract class Inconsistency extends Exception {
  
  def feedback: AnyRef
  
  // Do not fill the trace
  final override val fillInStackTrace: Throwable = this 
  
  final override def toString: String = {
    if (feedback == null) "Inconsistency"
    else s"Inconsistency: $feedback"
  }
}

object Inconsistency extends Inconsistency {  
  
  // Feedback message (useful for debugging)
  final private var _feedback: AnyRef = null 
  
  final override def feedback: AnyRef = _feedback 
  
  final def apply(): Inconsistency = apply(null)  
  
  final def apply(feedback: AnyRef): Inconsistency = {
    _feedback = feedback
    this
  }
}