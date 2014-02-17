package oscar.cp.core

import oscar.cp.constraints.Not

/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPBoolVar(st: CPStore, name: String) extends CPIntVarImpl(st, 0, 1, name) {

  def this(s: CPStore) = this(s, "")

  def this(s: CPStore, b: Boolean, name: String = "") = {
    this(s, name)
    if (b) assign(1)
    else assign(0)
  }

  override def toString() = {
    if (!isBound) "false, true"
    else if (value == 0) "false"
    else "true"
  }

  /** @return a constraint setting the boolean variable to true (1) */
  def constraintTrue(): Constraint = new oscar.cp.constraints.Eq(this, 1)

  /** @return a constraint setting the boolean variable to false (0) */
  def constraintFalse(): Constraint = new oscar.cp.constraints.Eq(this, 0)

  /** @return true if the variable is bound and bound to value 1 */
  def isTrue: Boolean = isBound && value == 1

  /** @return true if the variable is bound and bound to value 0 */
  def isFalse: Boolean = isBound && value == 0

  /** Logical or */
  def or(y: CPBoolVar): CPBoolVar = {
    val b = new CPBoolVar(store)
    store.post(new oscar.cp.constraints.OrReif2(Array(this, y), b))
    b
  }

  /** Logical and */
  def and(y: CPBoolVar): CPBoolVar = {
    val res = this.plus(y)
    res.isEq(2)
  }

  def not(): CPBoolVar = {
    val not = new CPBoolVar(store)
    store.post(new oscar.cp.constraints.Not(this, not))
    not
  }

  def implies(y: CPBoolVar) = {
    val V = new CPBoolVar(store)
    store.post(new oscar.cp.constraints.Implication(this, y, V))
    V
  }

  /**
   * -b
   */
  def unary_!(): CPBoolVar = this.not()
  /**
   * x | y
   */
  def |(y: CPBoolVar) = this.or(y)
  /**
   * x || y
   */
  def ||(y: CPBoolVar) = this.or(y)
  /**
   * x & y
   */
  def &(y: CPBoolVar) = this.and(y)
  /**
   * x && y
   */
  def &&(y: CPBoolVar) = this.and(y)
  /**
   * x ==> y
   */
  def ==>(y: CPBoolVar) = this.implies(y)
  /**
   * x != y
   */
  def !=(y: CPBoolVar) = new Not(this, y)
}

object CPBoolVar {

  /** Creates a new CP Boolean Variable */
  def apply(name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVar(store, name)

  /** Creates a new CP Boolean Variable */
  def apply()(implicit store: CPStore): CPBoolVar = apply("")(store)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean, name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVar(store, b, name)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean)(implicit store: CPStore): CPBoolVar = apply(b, "")(store)

  @deprecated("use apply(b: Boolean)(implicit store: CPStore) instead", "1.0")
  def apply(cp: CPStore, b: Boolean): CPBoolVar = new CPBoolVar(cp, b)
}  
  
