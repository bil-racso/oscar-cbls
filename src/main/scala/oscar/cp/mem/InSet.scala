package oscar.cp.mem

import oscar.cp.core._
import oscar.cp.core.CPOutcome._

/** InSet
 *
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class InSet(cp: Store, x: CPVarInt, set: Set[Int]) extends Constraint(cp, "InSet") {

  override def setup(l: CPPropagStrength): CPOutcome = propagate()
  
  override def propagate(): CPOutcome = {
    for (v <- x.min to x.max; if x hasValue v) {
      if (!set.contains(v))
          if (x.removeValue(v) == Failure) return Failure
    }   
    Success
  }
}

