package oscar.lcg.variables

import oscar.lcg.core.CDCLStore

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class LCGVar {
  
  /** Update the domain and notify the constraints. */
  def updateAndNotify(): Unit
  
  /** CDCL Store */
  def cdclStore: CDCLStore
}