package oscar.cp.constraints.sat

import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPStore
import oscar.cp.modeling.CPSolver
import oscar.cp.core.Constraint

import oscar.cp.modeling.CPModel
import oscar.cp.search.BinaryStaticOrderBranching

abstract class Literal(val boolean: CPBoolVar) {
  val id: Long
  def isTrue: Boolean // = boolean.isTrue
  def isFalse: Boolean // = boolean.isFalse
  def isBound: Boolean = boolean.isBound
  def set(b: Boolean): CPOutcome = if (b) setTrue else setFalse
  def setTrue: CPOutcome
  def setFalse: CPOutcome
  val negation: Literal
  val store: CPStore
  def callValBindWhenAssigned(c: Constraint, id: Int): Unit = {
    boolean.callValBindIdxWhenBind(c, id)
  }
  override def toString = boolean.toString
}


object BoolLiteral {
  private var literalCounter: Long = 0
  
  private def nextId(): Long = {
    val id = literalCounter
    literalCounter += 1
    id
  }
  
  def apply()(implicit s: CPStore): Literal = {
    val b = CPBoolVar()
    
    new Literal(b) {
      override val id = nextId()
      override def setTrue: CPOutcome = b.assign(1)
      override def setFalse: CPOutcome = b.assign(0)
      override def isTrue: Boolean = b.isTrue
      override def isFalse: Boolean = b.isFalse
      override val negation: Literal = {
        val lit = this
        new Literal(b) {
          override val id = nextId()
          override def setTrue: CPOutcome = lit.setFalse
          override def setFalse: CPOutcome = lit.setTrue
          override def isTrue: Boolean = lit.isFalse
          override def isFalse: Boolean = lit.isTrue
          override val negation: Literal = lit
          override val store = s
        }
      }
      override val store = s
    }
  }
}


import oscar.cp.modeling._
object TestLiteral extends CPModel with App {

  // val cp = CPSolver()
  // val b = CPBoolVar() //(cp)
  // val lit = BoolLiteral() //(cp)
  
  val lits = Array.tabulate(4)(i => BoolLiteral())
//  println("lits was defined, is " + lits.mkString("[", "; ", "]"))
  
  search {
    new BinaryStaticOrderBranching(lits.map(_.boolean))
  }
  
  // val clauses = Array( (lits(0).negation, lits(0).negation), (lits(0), lits(0)) )  // a \/ a, -a \/ -a, this means a and -a simultaneously, infeasible
  // val clauses = Array( (lits(0).negation, lits(1)), (lits(1).negation, lits(2)), (lits(2).negation, lits(0)) )  // a => b, b => c, c => a
  // val clauses = Array(  (lits(0).negation, lits(1)), (lits(1).negation, lits(2)), (lits(2).negation, lits(0).negation) ) // a => b, b => c, c => -a
  val clauses = Array(  (lits(0).negation, lits(1)), (lits(1).negation, lits(2)), (lits(2).negation, lits(3)), (lits(3).negation, lits(0).negation) ) // a => b, b => c, c => d, d => -a  
  
  add(new SatConstraint(lits ++ lits.map(_.negation), clauses))
  
  onSolution {
    println("solution, lits = " + lits.mkString("[", "; ", "]"))
  }
  
  val stats = start() 
    
  // println("finished, literal = " + lits.mkString("[", "; ", "]"))
}
