package oscar.cp.searches

import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleContext
import oscar.algo.search.Branching
import oscar.algo.search.Alternative
import oscar.algo.search.noAlternative

class DiscrepancyBranching(context: ReversibleContext, branching: Branching, maxDiscrepancy: Int) extends Branching {

  private[this] var discrepancy = 0

  final override def alternatives: Seq[Alternative] = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) noAlternative
    else {
      val k = math.min(maxDiscrepancy - discrepancy + 1, alternatives.length)
      // Mapped alternative
      val mappedAlternatives = new Array[Alternative](k)
      var i = 0
      while (i < k) {
        val alternative = alternatives(i)
        val newDiscrepancy = discrepancy + i
        mappedAlternatives(i) = () => {
          discrepancy = newDiscrepancy
          alternative.apply()
        }
        i += 1
      }
      mappedAlternatives
    }
  }
}