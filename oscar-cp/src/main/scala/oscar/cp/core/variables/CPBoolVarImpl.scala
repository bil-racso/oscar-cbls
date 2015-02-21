package oscar.cp.core.variables

import scala.Iterator
import scala.util.Random
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome.Failure
import oscar.cp.core.CPOutcome.Suspend
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.ConstraintQueue
import oscar.cp.core.watcher.WatcherListL2
import oscar.cp.core.watcher.WatcherListL1

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class CPBoolVarImpl private(final override val store: CPStore, initDomain: Int, final override val name: String = "") extends CPBoolVar {
  
  import CPBoolVarImpl._
  
  // Registered constraints
  private[this] val onBoundsL2 = new WatcherListL2(store)
  private[this] val onBindL2 = new WatcherListL2(store)
  private[this] val onDomainL2 = new WatcherListL2(store)
  private[this] val onBoundsL1 = new WatcherListL1(store)
  private[this] val onBindL1 = new WatcherListL1(store)
  private[this] val onDomainL1 = new WatcherListL1(store)
  
  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often
  
  // 00 : False
  // 11 : True
  // 10 : Unassigned
  // 01 : Empty
  private[this] var domain: Int = initDomain

  // A Boolean variable only needs one pre-instantiated trail entry
  private[this] val trailEntry = new TrailEntry { 
    final override def restore(): Unit = domain = UNASSIGNED 
  }

  final override def transform(v: Int) = v

  final override def isBound = domain != UNASSIGNED

  final override def size = {
    if (domain == UNASSIGNED) 2
    else if (domain == EMPTY) 0
    else 1
  }

  final override def isEmpty = domain == EMPTY
  
  final override def min: Int = domain & 1 // min is faster than max

  final override def max: Int = (domain & 2) >> 1
  
  final override def isTrue: Boolean = domain == TRUE

  final override def isFalse: Boolean = domain == FALSE

  final override def isBoundTo(value: Int): Boolean = {
    if (value == 0) domain == FALSE
    else if (value == 1) domain == TRUE
    else false
  }
  
  final override def containsTrue: Boolean = {
    if (domain == EMPTY) false
    else domain >= UNASSIGNED 
  }
  
  final override def containsFalse: Boolean = {
    if (domain == EMPTY) false
    else domain <= 2
  }

  final override def hasValue(value: Int): Boolean = {
    if (domain == EMPTY) false
    else if (value == 0) domain <= UNASSIGNED
    else if (value == 1) domain >= UNASSIGNED
    else false
  }

  final override def valueAfter(value: Int): Int = {
    if (value <= 0) if (domain <= UNASSIGNED) 0 else 1
    else value
  }

  final override def valueBefore(value: Int): Int = {
    if (value >= 1) if (domain >= UNASSIGNED) 1 else 0
    else value
  }

  final override def randomValue(rand: Random): Int = {
    if (domain == UNASSIGNED) rand.nextInt(2)
    else domain & 1 // min value
  }

  final override def updateMin(value: Int): CPOutcome = {
    if (value == 1) {
      if (domain == UNASSIGNED) setDomainTrue()
      else Suspend
    } else if (value <= 0) Suspend
    else setDomainEmpty
  }

  final override def updateMax(value: Int): CPOutcome = {
    if (value == 0) {
      if (domain == UNASSIGNED) setDomainFalse()
      else Suspend
    } else if (value >= 1) Suspend
    else setDomainEmpty
  }
  
  final override def assignTrue(): CPOutcome = {
    if (domain == UNASSIGNED) setDomainTrue()
    else if (domain == TRUE) Suspend
    else setDomainEmpty()
  }

  final override def assignFalse(): CPOutcome = {
    if (domain == UNASSIGNED) setDomainFalse()
    else if (domain == FALSE) Suspend
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
    store.trail(trailEntry)
    domain = TRUE
    // Notify constraints
    onDomainL1.enqueueRemove(0)
    onBoundsL1.enqueueBounds()
    onBindL1.enqueueBind()
    onDomainL2.enqueue()
    onBoundsL2.enqueue()
    onBindL2.enqueue()
    Suspend
  }

  @inline private def setDomainFalse(): CPOutcome = {
    store.trail(trailEntry)
    domain = FALSE
    // Notify constraints
    onDomainL1.enqueueRemove(1)
    onBoundsL1.enqueueBounds()
    onBindL1.enqueueBind()
    onDomainL2.enqueue()
    onBoundsL2.enqueue()
    onBindL2.enqueue()
    Suspend
  }

  @inline private def setDomainEmpty(): CPOutcome = {
    // FIXME
    val value = domain
    store.trail(new TrailEntry { final override def restore(): Unit = domain = value })
    domain = EMPTY
    Failure
  }

  final override def iterator = {
    if (domain == UNASSIGNED) Iterator(0, 1)
    else if (domain == FALSE) Iterator(0)
    else if (domain == TRUE) Iterator(1)
    else Iterator.empty
  }
    
  final override def constraintTrue(): Constraint = new oscar.cp.constraints.EqCons(this, 1)

  final override def constraintFalse(): Constraint = new oscar.cp.constraints.EqCons(this, 0)

  final override lazy val not: CPBoolVar = new CPBoolVarNot(this)

  final override def toString: String = {
    domain match { // tableswitch
      case FALSE => "0"
      case EMPTY => "empty"
      case UNASSIGNED => "{0, 1}"
      case TRUE => "1"
      case _ => sys.error("unknown domain")
    }
  }
  
  final override def constraintDegree: Int = degree.value

  final override def callPropagateWhenBind(c: Constraint) {
    degree.incr()
    onBindL2.register(c)
  }

  final override def callPropagateWhenBoundsChange(c: Constraint) {
    degree.incr()
    onBoundsL2.register(c)
  }

  final override def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    degree.incr()
    onDomainL2.register(c)
    if (trackDelta) c.addSnapshot(this)
  }
  
  def callPropagateWhenDomainChanges(c: Constraint, watcher: oscar.cp.core.Watcher): Unit = ???


  final override def callValBindWhenBind(c: Constraint) {
    callValBindWhenBind(c, this)
  }

  final override def callValBindWhenBind(c: Constraint, variable: CPIntVar) {
    degree.incr()
    onBindL1.register(c, variable)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint) {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) {
    degree.incr()
    onBoundsL1.register(c, variable)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint) {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) {
    degree.incr()
    onDomainL1.register(c, variable)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onDomainL1.register(c, variable, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onBoundsL1.register(c, variable, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int) {
    callValBindIdxWhenBind(c, this, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onBindL1.register(c, variable, idx)
  }

  // ----------------------------------
  
  final override def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = ???

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

object CPBoolVarImpl {
  
  // The first bit corresponds to the min value.
  // The second bit corresponds to the max value. 
  // Empty is represented by 1
  //
  // 00 : False
  // 11 : True
  // 10 : Unassigned
  // 01 : Empty
  private final val FALSE = 0
  private final val TRUE = 3
  private final val UNASSIGNED = 2
  private final val EMPTY = 1
  
  def apply(store: CPStore, assignedValue: Boolean, name: String): CPBoolVar = {
    if (assignedValue) new CPBoolVarImpl(store, TRUE, name)
    else new CPBoolVarImpl(store, FALSE, name)
  }
  
  def apply(store: CPStore, assignedValue: Int, name: String): CPBoolVar = {
    if (assignedValue == 0) new CPBoolVarImpl(store, FALSE, name)
    else if (assignedValue == 1) new CPBoolVarImpl(store, TRUE, name)
    else sys.error("assignedValue needs to be 0 or 1.")
  }
  
  def apply(store: CPStore, name: String): CPBoolVar = new CPBoolVarImpl(store, UNASSIGNED, name)
}