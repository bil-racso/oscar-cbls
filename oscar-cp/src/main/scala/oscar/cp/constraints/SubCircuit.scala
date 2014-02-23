package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/**
 * SubCircuit
 *
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class SubCircuit(val successors: Array[CPIntVar]) extends Constraint(successors.head.store, "SubCircuit") {

  private val Succs = 0 until successors.size
  private val dest: Array[ReversibleInt] = Array.tabulate(successors.size)(new ReversibleInt(s, _))
  private val src: Array[ReversibleInt] = Array.tabulate(successors.size)(new ReversibleInt(s, _))
  private val nSubCircuits: ReversibleInt = new ReversibleInt(s, 0)

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (s.post(new AllDifferent(successors: _*), l) == Failure) Failure
    else if (init() == Failure) Failure
    else Suspend
  }

  private def init(): CPOutcome = {
    Succs.foreach(s => {
      if (!successors(s).isBound) successors(s).callValBindIdxWhenBind(this, s)
      else {
        if (valBindIdx(successors(s), s) == Failure) return Failure
      }
    })
    Suspend
  }

  @inline
  private def close(): CPOutcome = {
    Succs.foreach(s => {
      if (!successors(s).isBound) {
        if (successors(s).assign(s) == Failure) return Failure
      }
    })
    Success // All the variables are bound
  }

  override def valBindIdx(cpvar: CPIntVar, u: Int): CPOutcome = {
    // s ->* u -> v ->* d
    val v = cpvar.value
    if (u == v) Suspend
    else {    
      val s = src(u).value
      val d = dest(v).value
      src(d).value = s
      dest(s).value = d     
      // Updates the number of sub-circuits
      if (d != v) nSubCircuits.decr()
      else if (s == u) nSubCircuits.incr()      
      // Only one sub-circuit
      if (nSubCircuits.value > 1) {
        if (successors(d).removeValue(s) == Failure) {
          return Failure
        }
      }     
      // Closes the circuit
      if (src(u).value == v) close()
      else Suspend
    }
  }
}

object SubCircuit {
  def apply(successors: Array[CPIntVar], offset: Int = 0): SubCircuit = {
    val succs = if (offset == 0) successors else successors.map(_-offset)
    new SubCircuit(succs)
  }
}