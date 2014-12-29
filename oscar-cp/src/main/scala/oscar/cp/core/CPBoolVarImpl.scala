package oscar.cp.core

import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.TrailEntry
import scala.util.Random
import oscar.cp.core.CPOutcome._

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class CPBoolVarImpl( final override val store: CPStore, final override val name: String = "") extends CPBoolVar {

  // Registered constraints
  private[this] val onBoundsL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private[this] val onBindL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private[this] val onDomainL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private[this] val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onDomainL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntVar]](store, null)
  private[this] val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onDomainIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntVar]](store, null)
  
  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  // The first bit corresponds to the min value.
  // The second bit corresponds to the max value. 
  // Empty is represented by 1
  //
  // 00 : False
  // 11 : True
  // 10 : Unassigned
  // 01 : Empty
  
  private[this] var domain: Int = 2 // unassigned

  // Boolean variables only need one pre-instantiated trail entry
  private[this] val trailEntry = new TrailEntry { final override def restore(): Unit = domain = 2 }

  // Used to trail changes
  @inline final def trail(): Unit = store.trail(trailEntry)

  final override def transform(v: Int) = v

  final override def isBound = domain != 2

  final override def size = {
    if (domain == 2) 2
    else if (domain == 1) 0
    else 1
  }

  final override def isEmpty = domain == 1
  
  final override def min: Int = domain & 1 // min is faster than max

  final override def max: Int = (domain & 2) >> 1
  
  final override def isTrue: Boolean = domain == 3

  final override def isFalse: Boolean = domain == 0

  final override def isBoundTo(value: Int): Boolean = {
    if (value == 0) domain == 0
    else if (value == 1) domain == 3
    else false
  }
  
  final override def containsTrue: Boolean = {
    if (domain == 1) false
    else domain >= 2
  }
  
  final override def containsFalse: Boolean = {
    if (domain == 1) false
    else domain <= 2
  }

  final override def hasValue(value: Int): Boolean = {
    if (domain == 1) false
    else if (value == 0) domain <= 2
    else if (value == 1) domain >= 2
    else false
  }

  final override def valueAfter(value: Int): Int = {
    if (value <= 0) if (domain <= 2) 0 else 1
    else value
  }

  final override def valueBefore(value: Int): Int = {
    if (value >= 1) if (domain >= 2) 1 else 0
    else value
  }

  final override def randomValue(rand: Random): Int = {
    if (domain == 2) rand.nextInt(2)
    else domain & 1 // min value
  }

  final override def updateMin(value: Int): CPOutcome = {
    if (value == 1) {
      if (domain == 2) setDomainTrue()
      else Suspend
    } else if (value <= 0) Suspend
    else setDomainEmpty
  }

  final override def updateMax(value: Int): CPOutcome = {
    if (value == 0) {
      if (domain == 2) setDomainFalse()
      else Suspend
    } else if (value >= 1) Suspend
    else setDomainEmpty
  }
  
  final override def assignTrue(): CPOutcome = {
    if (domain == 2) setDomainTrue()
    else if (domain == 3) Suspend
    else setDomainEmpty()
  }

  final override def assignFalse(): CPOutcome = {
    if (domain == 2) setDomainFalse()
    else if (domain == 0) Suspend
    else setDomainEmpty()
  }
    
  final override def assign(value: Int): CPOutcome = {
    if (value == 0) assignFalse()
    else if (value == 1) assignTrue()
    else Failure
  }

  final override def removeValue(value: Int) = {
    if (value == 0) assignTrue() 
    else if (value == 1) assignFalse()
    else Suspend
  }

  @inline private def setDomainTrue(): CPOutcome = {
    trail()
    domain = 3 // assign to true
    // Notify constraints
    store.notifRemoveL1(onDomainL1.value, this, 0)
    store.notifyRemoveIdxL1(onDomainIdxL1.value, this, 0)
    store.notifyBindL1(onBindL1.value, this)
    store.notifyBindIdxL1(onBindIdxL1.value, this)
    store.notifyL2(onBindL2.value)
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    store.notifyL2(onDomainL2.value)
    Suspend
  }

  @inline private def setDomainFalse(): CPOutcome = {
    trail()
    domain = 0 // assign to false
    // Notify constraints
    store.notifRemoveL1(onDomainL1.value, this, 1)
    store.notifyRemoveIdxL1(onDomainIdxL1.value, this, 1)
    store.notifyBindL1(onBindL1.value, this)
    store.notifyBindIdxL1(onBindIdxL1.value, this)
    store.notifyL2(onBindL2.value)
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    store.notifyL2(onDomainL2.value)
    Suspend
  }

  @inline private def setDomainEmpty(): CPOutcome = {
    trail()
    domain = 1 // empty
    Failure
  }

  final override def iterator = {
    if (domain == 2) Iterator(0, 1)
    else if (domain == 0) Iterator(0)
    else if (domain == 3) Iterator(1)
    else Iterator.empty
  }
    
  final override def constraintTrue(): Constraint = new oscar.cp.constraints.EqCons(this, 1)

  final override def constraintFalse(): Constraint = new oscar.cp.constraints.EqCons(this, 0)

  final override lazy val not: CPBoolVar = new CPBoolVarNot(this)

  final override def toString: String = {
    if (isTrue) "1"
    else if (isFalse) "0"
    else "{0,1}"
  }
  
  final override def constraintDegree: Int = degree.value

  final override def callPropagateWhenBind(c: Constraint) {
    degree.incr()
    onBindL2.setValue(new ConstraintQueue(onBindL2.value, c))
  }

  final override def callPropagateWhenBoundsChange(c: Constraint) {
    degree.incr()
    onBoundsL2.setValue(new ConstraintQueue(onBoundsL2.value, c))
  }

  final override def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    degree.incr()
    onDomainL2.setValue(new ConstraintQueue(onDomainL2.value, c))
    if (trackDelta) c.addSnapshot(this)
  }

  final override def callValBindWhenBind(c: Constraint) {
    callValBindWhenBind(c, this)
  }

  final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) {
    degree.incr()
    onBindL1.setValue(new PropagEventQueueVarInt(onBindL1.value, c, variable))
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint) {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) {
    degree.incr()
    onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value, c, variable))
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint) {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) {
    degree.incr()
    onDomainL1.setValue(new PropagEventQueueVarInt(onDomainL1.value, c, variable))
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onDomainIdxL1.setValue(new PropagEventQueueVarInt(onDomainIdxL1.value, c, variable, idx))
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) {
    degree.incr()
    onBoundsIdxL1.setValue(new PropagEventQueueVarInt(onBoundsIdxL1.value, c, variable, idx))
  }

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int) {
    callValBindIdxWhenBind(c, this, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) {
    degree.incr()
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }

  // ----------------------------------

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
}