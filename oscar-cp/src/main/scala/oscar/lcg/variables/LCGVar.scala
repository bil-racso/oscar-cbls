package oscar.lcg.variables

import oscar.lcg.core.CDCLStore

abstract class LCGVar {
  
  /** Update the domain and notify the constraints. */
  def updateAndNotify(): Unit
  
  /** CDCL Store */
  def cdclStore: CDCLStore
}