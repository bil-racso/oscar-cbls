package oscar.cp.constraints.sat

import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPStore
import oscar.cp.core.CPSolver
import oscar.cp.core.Constraint

import oscar.cp.CPModel
import oscar.cp.searches.BinaryStaticOrderBranching

abstract class Literal(val boolean: CPBoolVar) {
  val id: Int
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
  
  private var nextId: Int = 0
  
  @inline
  def getId(): Int = {
    val id = nextId
    nextId += 1
    id
  }
  
  def apply()(implicit s: CPStore): Literal = {
    val b = CPBoolVar() 
    new Literal(b) {
      override val id = getId()
      override def setTrue: CPOutcome = b.assign(1)
      override def setFalse: CPOutcome = b.assign(0)
      override def isTrue: Boolean = b.isTrue
      override def isFalse: Boolean = b.isFalse
      override val negation: Literal = {
        val lit = this
        new Literal(b) {
          override val id = getId()
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