package oscar.cp.searches

import oscar.cp.core.variables.CPSetVar
import oscar.algo.search._

class BinarySetBranching(x: CPSetVar) extends Branching {
  private[this] val store = x.store
  def alternatives(): Seq[Alternative] = {
    if (x.isBound) noAlternative
    else {
      val v = x.arbitraryPossibleNotRequired
      branch(store.post(x ++ v))(store.post(x -- v))
    }
  }
}