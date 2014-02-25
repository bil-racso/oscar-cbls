package oscar.cp.constraints.sat

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPIntVar
import scala.collection.mutable.Queue
import scala.collection.immutable.Map

class SatConstraint(val literals: Array[Literal], val clauses: Array[(Literal, Literal)]) extends Constraint(literals.head.store, "SatConstraint") {
  
  private val nLiterals = literals.size
  private val Literals = 0 until nLiterals
  
  def IDToIndex = literals.map(_.id).zipWithIndex.toMap
//  println("IDToIndex = " + IDToIndex)
  
  // Graph
  private val nClauses = clauses.size
  private val implicationArcs = Array.tabulate(2 * nClauses) { i =>
    if (i < nClauses) (clauses(i           )._1.negation, clauses(i           )._2)
    else              (clauses(i - nClauses)._2.negation, clauses(i - nClauses)._1)
  }
    .map { case (l1, l2) => (IDToIndex(l1.id), IDToIndex(l2.id)) }
    
  private val implicationAdjacency = Array.tabulate(nLiterals) { i => Array.empty[Int] }
  implicationArcs.groupBy(_._1).foreach { case (i, ijPairs) => implicationAdjacency(i) = ijPairs.map(_._2) }
//  println("Implication Arcs = " + implicationArcs.mkString("(", ", ", ")"))
    
  private val implicationSccs = new Tarjan(Literals.toArray, implicationAdjacency)
    
  private val litToSCC = implicationSccs.sccOf
  private val SCCToLit = implicationSccs.nodesOf
  private val SCCToSCC = implicationSccs.successorsOf
  
//  println("litToSCC: " + litToSCC.mkString(", "))
//  println("SCCToLit: " + SCCToLit.map(_.mkString(", ")).mkString("; "))
//  println("SCCToSCC: " + SCCToSCC.map(_.mkString(", ")).mkString("; "))
  
  // Used for internal propagation
  private val queue: Queue[Int] = Queue()
  private val visited: Array[Boolean] = Array.fill(nLiterals)(false)
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (isInfeasible() || propagate() == Failure) Failure 
    else {
      Literals.foreach(l => literals(l).callValBindWhenAssigned(this, l))
      Suspend
    }
  }
  
  @inline
  private def propagation(lit: Int): CPOutcome = {
 //   println("2SAT woken on literal of id " + literals(lit).id + ", which is " + literals(lit).boolean)
    if (literals(lit).isFalse) {
 //     println("Nothing to do")
      return Suspend
    }
    
    // Initializes the structure
    queue.clear()
    Literals.foreach(visited(_) = false)
    
    val init = litToSCC(lit)
    queue.enqueue(init)
    visited(init) = true
    
    
    while (!queue.isEmpty) {
      val scc = queue.dequeue()
 //     println("Visiting SCC " + scc)
      if (setSCC(scc, true) == Failure) return Failure
      else {
        val sccs = SCCToSCC(scc)
        for (s <- SCCToSCC(scc)) {
          if (!visited(s)) {
 //           println("Adding SCC " + s + " to queue.")
            queue.enqueue(s)
            visited(s) = true
          }
        }
      }
    }
 //   println("Suspending")

    Suspend
  }
  
  private def isInfeasible(): Boolean = {
    literals.exists { l => litToSCC(IDToIndex(l.id)) == litToSCC(IDToIndex(l.negation.id)) }
  }
  
  @inline
  private def setSCC(scc: Int, value: Boolean): CPOutcome = {
    if (SCCToLit(scc).exists { i => 
  //    println("Setting literals(" + i + ") to " + value) ; 
      literals(i).set(value) == Failure 
      }) {
//      println("scc propagation failed")
      return Failure
    }
      
    else Suspend
  }
  
  override def valBindIdx(boolvar: CPIntVar, index: Int): CPOutcome = {
    deactivate() // Do not register this constraint in L1
    val outcome = propagation(index)
    activate() // The constraint can be registered in L1
    outcome
  }
}

import oscar.cp.modeling._

// Maybe we could harvest inequalities the same way, this could detect infeasibilities a < b < c < a
// 

object ImSlow extends CPModel with App {  // 2 minutes!
  val tStart = System.currentTimeMillis()
  val a = CPIntVar(0 until 2000000000)
  val b = CPIntVar(0 until 2000000000 + 1)
  val c = CPIntVar(0 until 2000000000 + 2)
  
  add(a < b)
  add(b < c)
  try {
    add(c < a)
  } catch { 
    case e:Exception =>
    val tEnd = System.currentTimeMillis()
    println("Je prends " + (tEnd - tStart))
  }
  
}
