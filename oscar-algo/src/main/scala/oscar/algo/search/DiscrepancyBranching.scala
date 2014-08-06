package oscar.algo.search

import oscar.algo.reversible.ReversibleInt

class DiscrepancyBranching(val branching: Branching, val maxDiscrepancy: Int, node: SearchNode) extends Branching {
  
  private val discrepancy = new ReversibleInt(node, 0)
  
  override def alternatives: Seq[Alternative] = {
    val alts = branching.alternatives
    if (alts.isEmpty) alts
    else {
      val d = discrepancy.value
      val k = maxDiscrepancy - d
      var i = 0
      alts.take(k).map(alternative => () => {
        discrepancy += i
        i += 1
        alternative()
      })
    }
  }
}