package oscar.cp.core.domains

import oscar.cp.core.CPOutcome

abstract class IntDomain extends IntervalDomain {
  def removeValue(value: Int): CPOutcome 
}