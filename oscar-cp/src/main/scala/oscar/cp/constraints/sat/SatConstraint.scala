package oscar.cp.constraints.sat

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPIntVar
import scala.collection.mutable.Queue

class SatConstraint(val literals: Array[Literal]) extends Constraint(literals.head.store, "SatConstraint") {
  
  private val nLiterals = literals.size
  private val Literals = 0 until nLiterals
  
  // Graph
  private val litToSCC: Array[Int] = ???
  private val SCCToLit: Array[Array[Int]] = ???
  private val SCCToSCC: Array[Array[Int]] = ???
  
  // Used for internal propagation
  private val queue: Queue[Int] = Queue()
  private val visited: Array[Boolean] = Array.fill(nLiterals)(false)
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure 
    else {
      Literals.foreach(l => literals(l).callValBindWhenAssigned(this, l))
      Suspend
    }
  }
  
  @inline
  private def propagation(lit: Int): CPOutcome = {
    
    // Initializes the structure
    queue.clear()
    Literals.foreach(visited(_) = false)
    
    val init = litToSCC(lit)
    queue.enqueue(init)
    visited(init) = true
    
    while (!queue.isEmpty) {
      val scc = queue.dequeue()
      if (setSCC(scc, true) == Failure) return Failure
      else {
        val sccs = SCCToSCC(scc)
        for (s <- SCCToSCC(scc)) {
          if (!visited(s)) {
            queue.enqueue(s)
            visited(s) = true
          }
        }
      }
    }

    Suspend
  }
  
  @inline
  private def setSCC(scc: Int, value: Boolean): CPOutcome = {
    if (SCCToLit(scc).exists(literals(_).set(value) == Failure)) Failure
    else Suspend
  }
  
  override def valBindIdx(boolvar: CPIntVar, id: Int): CPOutcome = {
    deactivate() // Do not register this constraint in L1
    val outcome = propagation(id)
    activate() // The constraint can be registered in L1
    outcome
  }
}
