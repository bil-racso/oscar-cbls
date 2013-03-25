package oscar.cp.mem.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.reversible.ReversibleInt

/** NoCycle Constraint
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class NoCycle(private val succ: Array[CPVarInt], private val Succ: Set[Int]) extends Constraint(succ.head.store, "NoCycle") {

  private val cp = succ.head.store 
  private val nSucc = Succ.size

  private val pathDest = Array.tabulate(succ.size)(i => new ReversibleInt(cp, i))
  private val pathOrig = Array.tabulate(succ.size)(i => new ReversibleInt(cp, i))

  override def setup(l: CPPropagStrength): CPOutcome = {   
    for (i <- Succ) {
      // Removes i from its possible successors
      if (succ(i).removeValue(i) == Failure) return Failure  
      // Updates data structures if i has a fixed successor
      if (!succ(i).isBound) succ(i).callValBindIdxWhenBind(this, i)
      else if (valBindIdx(succ(i), i) == Failure) return Failure
    }
    Suspend
  }
  
  override def valBindIdx(cpvar: CPVarInt, i: Int): CPOutcome = {   
    // New arc i -> j   
    val j = cpvar.value     
    // o ->* i -> j ->* d
    val d = pathDest(j).value
    val o = pathOrig(i).value   
    // Merge both paths
    pathDest(o).setValue(d)
    pathOrig(d).setValue(o)       
    // avoid cycle 
    if (Succ contains d) succ(d).removeValue(o)
    else Suspend
  }
}

object NoCycle {
  def apply(succ: Array[CPVarInt], Succ: Set[Int]) = new NoCycle(succ, Succ)
}

