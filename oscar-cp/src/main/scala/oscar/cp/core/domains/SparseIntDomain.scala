package oscar.cp.core.domains

import oscar.cp.core.CPOutcome

abstract class SparseIntDomain extends IntervalIntDomain {
  def removeValue(value: Int): CPOutcome 
}