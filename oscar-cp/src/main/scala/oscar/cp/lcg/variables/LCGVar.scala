package oscar.cp.lcg.variables

abstract class LCGVar {
  
  /** Update the domain and notify the constraints. */
  def updateAndNotify(): Unit
}