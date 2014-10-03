package oscar.cp.constraints.sat

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPIntervalVar
import scala.collection.mutable.ArrayStack
import scala.collection.immutable.Map
import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPStore

class SatConstraint(lit: Array[Literal], val clauses: Array[(Literal, Literal)]) extends Constraint(lit.head.store, "SatConstraint") {

  private val literals = lit.sortBy(_.id)
  
  private val nLiterals = literals.size
  private val Literals = 0 until nLiterals
  
  private val offset = literals.head.id 
  @inline
  private def litId(id: Int): Int = id - offset

  // Graph
  private val nClauses = clauses.size
  private val implicationArcs = Array.tabulate(2 * nClauses) { i =>
    if (i < nClauses) (clauses(i)._1.negation, clauses(i)._2)
    else (clauses(i - nClauses)._2.negation, clauses(i - nClauses)._1)
  }   
    .map { case (l1, l2) => (litId(l1.id), litId(l2.id)) }

  private val implicationAdjacency = Array.tabulate(nLiterals) { i => Array.empty[Int] }
  implicationArcs.groupBy(_._1).foreach { case (i, ijPairs) => implicationAdjacency(i) = ijPairs.map(_._2) }

  private val implicationSccs = new Tarjan(Literals.toArray, implicationAdjacency)

  private val litToSCC: Array[Int] = implicationSccs.sccOf
  private val SCCToLit: Array[Array[Int]] = implicationSccs.nodesOf
  private val SCCToSCC: Array[Array[Int]] = implicationSccs.successorsOf

  // Used for internal propagation
  private val toVisit: ArrayStack[Int] = ArrayStack()

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (isInfeasible() || propagate() == Failure) Failure
    else {
      Literals.foreach(l => literals(l).callValBindWhenAssigned(this, l))
      Suspend
    }
  }

  private def propagation(lit: Int): CPOutcome = {

    if (literals(lit).isFalse) Suspend
    else {
      // Initializes the structure
      toVisit.clear()

      // The propagator can only set a literal to true (notice this might still set the underlying boolean to false).  
      // Mark visited scc by setting their first literal to true,
      // only add successor sccs if their first element is not bound to true.

      val init = litToSCC(lit)
      toVisit.push(init) // always visit the triggered literal's scc 
      if (SCCToLit(init)(0) != lit && setFirstToTrue(init) == Failure) return Failure // lit may not be the first literal of its scc

      while (!toVisit.isEmpty) {
        val scc = toVisit.pop()
        if (setSCC(scc, true) == Failure) return Failure // visit

        for (s <- SCCToSCC(scc)) {
          val firstLit = literals(SCCToLit(s)(0))
          if (!firstLit.isTrue) {
            toVisit.push(s)
            if (firstLit.setTrue == Failure) return Failure // mark
          }
        }
      }

      Suspend
    }
  }

  @inline
  private def setFirstToTrue(scc: Int): CPOutcome = literals(SCCToLit(scc)(0)).setTrue

  @inline
  private def isInfeasible(): Boolean = {
    literals.exists { l => litToSCC(litId(l.id)) == litToSCC(litId(l.negation.id)) }
  }

  @inline
  private def setSCC(scc: Int, value: Boolean): CPOutcome = {
    val max = SCCToLit(scc).size
    for (i <- 1 until max) { // The first literal of an scc we visit is always set to true, never need to visit it
      if (literals(SCCToLit(scc)(i)).setTrue == Failure) return Failure
    }
    Suspend
  }

  override def valBindIdx(boolvar: CPIntervalVar, index: Int): CPOutcome = {
    deactivate() // Do not register this constraint in L1
    val outcome = propagation(index)
    activate() // The constraint can be registered in L1
    outcome
  }
}

object SatConstraint {
  def apply(literals: Array[Literal], clauses: Array[(Literal, Literal)]): SatConstraint = new SatConstraint(literals, clauses)
}
