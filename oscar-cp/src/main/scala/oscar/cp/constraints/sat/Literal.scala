package oscar.cp.constraints.sat

import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPStore
import oscar.cp.modeling.CPSolver
import oscar.cp.core.Constraint

abstract class Literal {
  protected val boolean: CPBoolVar
  def isTrue: Boolean = boolean.isTrue
  def isBound: Boolean = boolean.isBound
  def set(b: Boolean): CPOutcome = if (b) setTrue else setFalse
  def setTrue: CPOutcome
  def setFalse: CPOutcome
  val negation: Literal
  val store: CPStore
  def callValBindWhenAssigned(c: Constraint, id: Int): Unit = {
    boolean.callValBindIdxWhenBind(c, id)
  }
}

object BoolLiteral {

  def apply(s: CPStore): Literal = new Literal {
    override protected val boolean: CPBoolVar = CPBoolVar()(store)
    override def setTrue: CPOutcome = boolean.assign(1)
    override def setFalse: CPOutcome = boolean.assign(0)
    override val negation: Literal = {
      val lit = this
      new Literal {
        override protected val boolean: CPBoolVar = lit.boolean
        override def setTrue: CPOutcome = lit.setFalse
        override def setFalse: CPOutcome = lit.setTrue
        override def isTrue: Boolean = !lit.isTrue
        override val negation: Literal = lit
        override val store = s
      }
    }
    override val store = s
  }
}

object TestFoireux extends App {

  val cp = CPSolver()
  val b = CPBoolVar()(cp)
  val lit = BoolLiteral(cp)
  println("finish")

}