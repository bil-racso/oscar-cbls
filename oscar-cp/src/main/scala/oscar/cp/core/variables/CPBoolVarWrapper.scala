package oscar.cp.core.variables

import scala.util.Random
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore

/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPBoolVarWrapper(x: CPIntVar, final override val name: String = "") extends CPBoolVar {
  
  require(x.max <= 1 && x.min >= 0, "cannot create a CPBoolVar from a non 0/1 CPIntVar")
  
  // Alternative constructor
  def this(store: CPStore) = this(CPIntVar(0 to 1)(store))

  final override val store: CPStore = x.store

  final override def transform(v: Int) = x.transform(v)

  final override def isBound = x.isBound

  final override def size = x.size

  final override def isEmpty = x.isEmpty
  
  final override def min = x.min

  final override def max = x.max
  
  final override def isTrue: Boolean = x.isBoundTo(1)

  final override def isFalse: Boolean = x.isBoundTo(0)

  final override def isBoundTo(value: Int): Boolean = x.isBoundTo(value)

  final override def containsTrue: Boolean = x.hasValue(1)
  
  final override def containsFalse: Boolean = x.hasValue(0)
  
  final override def hasValue(value: Int): Boolean = x.hasValue(value)

  final override def valueAfter(value: Int): Int = x.valueAfter(value)

  final override def valueBefore(value: Int): Int = x.valueBefore(value)
  
  final override def randomValue(rand: Random): Int = x.randomValue(rand)

  final override def updateMin(value: Int) = x.updateMin(value)
  
  final override def updateMax(value: Int) = x.updateMax(value)
  
  final override def assignTrue(): CPOutcome = x.assign(1)
  
  final override def assignFalse(): CPOutcome = x.assign(0)

  final override def assign(value: Int) = x.assign(value)

  final override def removeValue(value: Int) = x.removeValue(value)

  final override def iterator = {
    x.iterator
  }
  
  final override def constraintTrue: Constraint = new oscar.cp.constraints.EqCons(x, 1)

  final override def constraintFalse: Constraint = new oscar.cp.constraints.EqCons(x, 0)
  
  final override lazy val not: CPBoolVar = new CPBoolVarNot(this)//CPBoolVarWrapper((new CPIntVarViewMinus(this)) + 1)
  
  final override def toString: String = {
    if (isTrue) "1"
    else if (isFalse) "0"
    else "{0,1}"
  }
  
  final override def constraintDegree = x.constraintDegree

  final override def callPropagateWhenBind(c: Constraint) = x.callPropagateWhenBind(c)

  final override def callPropagateWhenBoundsChange(c: Constraint) = x.callPropagateWhenBoundsChange(c)

  final override def callPropagateWhenDomainChanges(c: Constraint,trackDelta: Boolean = false) = x.callPropagateWhenDomainChanges(c,trackDelta)

  // this method is useful when you have a view final override defined on a view
  final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) = x.callValBindWhenBind(c, variable)

  final override def callValBindWhenBind(c: Constraint) = x.callValBindWhenBind(c, this)

  // this method is useful when you have a view final override defined on a view
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) = x.callUpdateBoundsWhenBoundsChange(c, variable)

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint) = x.callUpdateBoundsWhenBoundsChange(c, this)

  // this method is useful when you have a view final override defined on a view
  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) = x.callValRemoveWhenValueIsRemoved(c, variable)

  final override def callValRemoveWhenValueIsRemoved(c: Constraint) = x.callValRemoveWhenValueIsRemoved(c, this)

  // this method is useful when you have a view final override defined on a view
  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) = x.callValBindIdxWhenBind(c, variable, idx)

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int) = x.callValBindIdxWhenBind(c, this, idx)

  // this method is useful when you have a view final override defined on a view
  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) = x.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = x.callUpdateBoundsIdxWhenBoundsChange(c, this, idx)

  // this method is useful when you have a view final override defined on a view
  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) = x.callValRemoveIdxWhenValueIsRemoved(c, variable, idx)

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) = x.callValRemoveIdxWhenValueIsRemoved(c, this, idx)

  // ----------------------------------

  final override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = x.delta(oldMin, oldMax, oldSize)

  final override def changed(c: Constraint): Boolean = x.changed(c)

  final override def minChanged(c: Constraint): Boolean = x.minChanged(c)

  final override def maxChanged(c: Constraint): Boolean = x.maxChanged(c)

  final override def boundsChanged(c: Constraint): Boolean = x.boundsChanged(c)

  final override def oldMin(c: Constraint): Int = x.oldMin(c)

  final override def oldMax(c: Constraint): Int = x.oldMax(c)

  final override def oldSize(c: Constraint): Int = x.oldSize(c)

  final override def deltaSize(c: Constraint): Int = x.deltaSize(c)

  final override def delta(c: Constraint): Iterator[Int] = x.delta(c)
}