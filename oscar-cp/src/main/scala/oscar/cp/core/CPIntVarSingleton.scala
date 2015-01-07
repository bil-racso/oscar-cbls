package oscar.cp.core

import scala.util.Random
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPOutcome._

final class CPIntVarSingleton(final override val store: CPStore, initValue: Int, final override val name: String = "") extends CPIntVar {
  
  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often
  
  // Static trail entry
  private[this] val trailEntry = new TrailEntry {
    @inline final override def restore(): Unit = _size = 1
  }

  // Domain representation
  private[this] var _size = 1
  
  final override val min: Int = initValue
  
  final override val max: Int = initValue
  
  final override def transform(v: Int) = v

  final override def iterator: Iterator[Int] = Iterator(initValue)
  
  /** @return The number of propagators registered on this variables. */
  final def constraintDegree: Int = degree.value

  /** @return true if the domain of the variable has exactly one value, false if the domain has more than one value */
  @inline final def isBound: Boolean = true

  /**
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  @inline final def isBoundTo(value: Int): Boolean = value == initValue

  /**
   * Test if a value is in the domain
   * @param val
   * @return  true if the domain contains the value val, false otherwise
   */
  @inline final def hasValue(value: Int): Boolean = value == initValue

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  final def valueAfter(value: Int): Int = initValue

  /**
   * @param val
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  final def valueBefore(value: Int): Int = initValue

  /** @return A random value in the domain of the variable (uniform distribution) */
  final override def randomValue(rand: Random): Int = initValue

  /**
   * @return  the size of the domain
   */
  @inline final override def size: Int = _size

  /**
   * @return true if the domain is empty, false otherwise
   */
  @inline final override def isEmpty: Boolean = _size == 0

  final override def toString: String = {
    if (_size == 0) "phi"
    else if (name.isEmpty) initValue.toString
    else name + " " + initValue
  }

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  @inline final override def assign(value: Int): CPOutcome = {
    if (_size == 0) Failure
    else if (value == initValue) Suspend
    else emptyDomain()
  }
  
  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
   */
  @inline final override def removeValue(value: Int): CPOutcome = {
    if (value == initValue) emptyDomain()
    else if (_size == 0) emptyDomain()
    else Suspend
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  @inline final override def updateMin(value: Int): CPOutcome = {
    if (_size == 0) Failure
    else if (value <= initValue) Suspend
    else emptyDomain()
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  @inline final override def updateMax(value: Int): CPOutcome = {
    if (_size == 0) Failure
    else if (value >= initValue) Suspend
    else emptyDomain()
  }
  
  // Trail and empty the domain
  @inline private def emptyDomain(): CPOutcome = {
    store.trail(trailEntry)
    _size = 0
    Failure
  }
  
  final override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = ???
  final override def changed(c: Constraint): Boolean = ???
  final override def minChanged(c: Constraint): Boolean = ???
  final override def maxChanged(c: Constraint): Boolean = ???
  final override def boundsChanged(c: Constraint): Boolean = ???
  final override def oldMin(c: Constraint): Int = ???
  final override def oldMax(c: Constraint): Int = ???
  final override def oldSize(c: Constraint): Int = ???
  final override def deltaSize(c: Constraint): Int = ???
  final override def delta(c: Constraint): Iterator[Int] = ??? 

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBind(c: Constraint): Unit = degree.incr()

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBoundsChange(c: Constraint): Unit = degree.incr()

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  final override def callValBindWhenBind(c: Constraint): Unit = degree.incr()
  final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = degree.incr()
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  final override def callValRemoveWhenValueIsRemoved(c: Constraint): Unit = degree.incr()
  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit = degree.incr()
  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = degree.incr()
  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = degree.incr()

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = degree.incr()
  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = degree.incr()
}
  