package oscar.cp.core.domains

import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversiblePointer

/** @author Renaud Hartert */

object AdaptableIntDomain {
  def apply(store: CPStore, minVal: Int, maxVal: Int): ReversiblePointer[IntDomain] = {
    val domain = new ReversiblePointer[IntDomain](store, null)
    val intervalDom = new IntervalDomain(domain, minVal, maxVal)
    domain.value = intervalDom
    domain
  }
}