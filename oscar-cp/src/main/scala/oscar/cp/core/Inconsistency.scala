package oscar.cp.core

/** @author Renaud Hartert ren.hartert@gmail.com */

abstract class Inconsistency extends Exception {
  
  // Do not fill the trace
  final override val fillInStackTrace: Throwable = this 
  
  final override val toString: String = "Inconsistency"
}

object Inconsistency extends Inconsistency