package oscar.cp.mem

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.reversible.ReversibleInt
import oscar.cp.constraints
import oscar.cp.constraints.AllDifferent

/**
 *  Circuit Constraint
 *
 *  @author Pierre Schaus  - pschaus@gmail.com
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class MyCircuit(cp: Store, private val succ: Array[CPVarInt]) extends Constraint(cp, "MyCircuit") {

  private val nSucc = succ.size
  private val Succ = 0 until nSucc

  private val pathDest = Array.tabulate(nSucc)(i => new ReversibleInt(cp, i))
  private val pathOrig = Array.tabulate(nSucc)(i => new ReversibleInt(cp, i))
  private val lengthToDest = Array.fill(nSucc)(new ReversibleInt(cp, 0))

  override def setup(l: CPPropagStrength): CPOutcome = {
    
    // For all i, there is only one j such that succ(j) == i
    if (cp.post(new AllDifferent(succ: _*), l) == Failure) return Failure
    
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
    // We have a new assigned path because of new edge i->j:   
    val j = cpvar.value  
    
    // o ->* i -> j ->* d
    val d = pathDest(j).value
    val o = pathOrig(i).value
    val pathLength = lengthToDest(o).value + lengthToDest(j).value + 1
    
    // Updates data structures
    pathDest(o).setValue(d)
    pathOrig(d).setValue(o)    
    lengthToDest(o).setValue(pathLength)
    
    // otherwise we would have a closed loop with less than n-1 edges
    if (pathLength < nSucc-1) succ(d).removeValue(o)
    else Suspend
  }
}

