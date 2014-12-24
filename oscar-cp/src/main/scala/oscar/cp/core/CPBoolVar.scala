package oscar.cp.core

import scala.util.Random
import oscar.cp.modeling._

/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPBoolVar(val x: CPIntVar, name: String = "") extends CPIntVar(x.store, name) {
  
  def this(store: CPStore) = this(CPIntVar(0 to 1)(store))

  if (x.max > 1 || x.min < 0) throw new RuntimeException("cannot create a CPBoolVar from a non 0/1 CPIntVar")

  // -------------------------------------------------------------

  def transform(v: Int) = x.transform(v)

  def isBound = x.isBound

  override def size = x.size

  override def isEmpty = x.isEmpty

  def constraintDegree = x.constraintDegree

  def isBoundTo(value: Int): Boolean = x.isBoundTo(value)

  def hasValue(value: Int): Boolean = x.hasValue(value)

  def valueAfter(value: Int): Int = x.valueAfter(value)

  def valueBefore(value: Int): Int = x.valueBefore(value)
  
  def randomValue(rand: Random): Int = x.randomValue(rand)

  def updateMin(value: Int) = x.updateMin(value)

  def assign(value: Int) = x.assign(value)

  def updateMax(value: Int) = x.updateMax(value)

  def removeValue(value: Int) = x.removeValue(value)

  def min = x.min

  def max = x.max

  def iterator = {
    x.iterator
  }

  def callPropagateWhenBind(c: Constraint) = x.callPropagateWhenBind(c)

  def callPropagateWhenBoundsChange(c: Constraint) = x.callPropagateWhenBoundsChange(c)

  def callPropagateWhenDomainChanges(c: Constraint,trackDelta: Boolean = false) = x.callPropagateWhenDomainChanges(c,trackDelta)

  // this method is useful when you have a view defined on a view
  def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) = x.callValBindWhenBind(c, variable)

  def callValBindWhenBind(c: Constraint) = x.callValBindWhenBind(c, this)

  // this method is useful when you have a view defined on a view
  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) = x.callUpdateBoundsWhenBoundsChange(c, variable)

  def callUpdateBoundsWhenBoundsChange(c: Constraint) = x.callUpdateBoundsWhenBoundsChange(c, this)

  // this method is useful when you have a view defined on a view
  def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = x.callValRemoveWhenValueIsRemoved(c, variable)

  def callValRemoveWhenValueIsRemoved(c: Constraint) = x.callValRemoveWhenValueIsRemoved(c, this)

  // this method is useful when you have a view defined on a view
  def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) = x.callValBindIdxWhenBind(c, variable, idx)

  def callValBindIdxWhenBind(c: Constraint, idx: Int) = x.callValBindIdxWhenBind(c, this, idx)

  // this method is useful when you have a view defined on a view
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) = x.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = x.callUpdateBoundsIdxWhenBoundsChange(c, this, idx)

  // this method is useful when you have a view defined on a view
  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = x.callValRemoveIdxWhenValueIsRemoved(c, variable, idx)

  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = x.callValRemoveIdxWhenValueIsRemoved(c, this, idx)

  // ----------------------------------

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = x.delta(oldMin, oldMax, oldSize)

  def changed(c: Constraint): Boolean = x.changed(c)

  def minChanged(c: Constraint): Boolean = x.minChanged(c)

  def maxChanged(c: Constraint): Boolean = x.maxChanged(c)

  def boundsChanged(c: Constraint): Boolean = x.boundsChanged(c)

  def oldMin(c: Constraint): Int = x.oldMin(c)

  def oldMax(c: Constraint): Int = x.oldMax(c)

  def oldSize(c: Constraint): Int = x.oldSize(c)

  def deltaSize(c: Constraint): Int = x.deltaSize(c)

  def delta(c: Constraint): Iterator[Int] = {
    x.delta(c)
  }

  // -----------------------------------------------------------------------------
/*
  override def toString() = {
    if (!x.isBound) "false, true"
    else if (x.value == 0) "false"
    else "true"
  }
*/
  // -----------------------------------------------------------

  /** @return a constraint setting the boolean variable to true (1) */
  def constraintTrue(): Constraint = new oscar.cp.constraints.EqCons(x, 1)

  /** @return a constraint setting the boolean variable to false (0) */
  def constraintFalse(): Constraint = new oscar.cp.constraints.EqCons(x, 0)

  /** @return true if the variable is bound and bound to value 1 */
  def isTrue: Boolean = isBound && min == 1

  /** @return true if the variable is bound and bound to value 0 */
  def isFalse: Boolean = isBound && min == 0

  /** Logical or */
  def or(y: CPBoolVar): CPBoolVar = {
    val b = new CPBoolVar(CPIntVar(x.store, 0 to 1))
    store.post(new oscar.cp.constraints.OrReif2(Array(this, y), b))
    b
  }

  /** Logical and */
  def and(y: CPBoolVar): CPBoolVar = {
    val res = plus(this,y)
    res.isEq(2)
  }

  def not(): CPBoolVar = {
    new CPBoolVar((new CPIntVarViewMinus(this))+1)
    /*
    val not = new CPBoolVar(CPIntVar(x.store, 0 to 1))
    store.post(new oscar.cp.constraints.Not(this, not))
    not
    */
  }
  
  override def toString: String = {
    if (isTrue) "1"
    else if (isFalse) "0"
    else "{0,1}"
  }

  def implies(y: CPBoolVar) = {
    val V = new CPBoolVar(CPIntVar(x.store, 0 to 1))
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

}

object CPBoolVar {

  /** Creates a new CP Boolean Variable */
  def apply(name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVar(CPIntVar(0 to 1)(store), name)

  /** Creates a new CP Boolean Variable */
  def apply()(implicit store: CPStore): CPBoolVar = apply("")(store)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean, name: String)(implicit store: CPStore): CPBoolVar = new CPBoolVar(CPIntVar(if (b) 1 else 0)(store), name)

  /** Creates a new CP Boolean Variable assigned to b */
  def apply(b: Boolean)(implicit store: CPStore): CPBoolVar = apply(b, "")(store)

  @deprecated("use apply(b: Boolean)(implicit store: CPStore) instead", "1.0")
  def apply(cp: CPStore, b: Boolean): CPBoolVar = new CPBoolVar(CPIntVar(if (b) 1 else 0)(cp))
}  
  
