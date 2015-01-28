package oscar.cp.core.variables

import scala.util.Random

import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome.Failure
import oscar.cp.core.CPOutcome.Suspend
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.watcher.WatcherListL2
import oscar.cp.core.watcher.PropagEventQueueVarInt

/**
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class CPIntVarTrailEntry(variable: CPIntVarSparse, min: Int, max: Int, size: Int) extends TrailEntry {
  @inline final override def restore(): Unit = {
    variable.min = min
    variable.max = max
    variable.size = size
  }
}

class CPIntVarSparse( final override val store: CPStore, minValue: Int, maxValue: Int, final override val name: String = "") extends CPIntVar {

  // Registered constraints
  private[this] val onBoundsL2 = new WatcherListL2(store)
  private[this] val onBindL2 = new WatcherListL2(store)
  private[this] val onDomainL2 = new WatcherListL2(store)
  private[this] val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onDomainL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntVar]](store, null)
  private[this] val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onDomainIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntVar]](store, null)

  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  // True if some constraints are registered on the bounds
  private[this] val registeredOnBounds = new ReversibleBoolean(store, false) // should not change often

  // Domain representation
  private[this] val nValues = maxValue - minValue + 1
  private[this] val values = Array.tabulate(nValues)(i => i + minValue)
  private[this] val positions = Array.tabulate(nValues)(i => i)
  private[this] var _min = minValue
  private[this] var _max = maxValue
  private[this] var _size = nValues

  // Used to trail changes in the domain
  private[this] var lastMagic: Long = -1L

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new CPIntVarTrailEntry(this, _min, _max, _size))
    }
  }

  @inline final override def size: Int = _size

  @inline final override def min: Int = _min

  @inline final override def max = _max

  @inline final def min_=(newMin: Int): Unit = _min = newMin

  @inline final def max_=(newMax: Int): Unit = _max = newMax

  @inline final def size_=(newSize: Int): Unit = _size = newSize

  @inline final override def isEmpty: Boolean = _size == 0

  @inline final override def isBound: Boolean = _size == 1

  final override def isBoundTo(value: Int): Boolean = {
    _size == 1 && values(0) == value
  }

  @inline final override def hasValue(value: Int): Boolean = {
    if (value < minValue || value > maxValue) false
    else positions(value - minValue) < _size
  }

  final override def valueAfter(value: Int): Int = ???

  final override def valueBefore(value: Int): Int = ???

  final override def randomValue(rand: Random): Int = {
    if (isEmpty) sys.error("the domain is empty")
    else {
      val pos = rand.nextInt(_size)
      values(pos)
    }
  }

  final override def transform(v: Int) = v

  final override def iterator: Iterator[Int] = new Iterator[Int] {
    private[this] val array = new Array[Int](_size)
    private[this] var i = 0
    System.arraycopy(values, 0, array, 0, _size)
    def next(): Int = { val v = array(i); i += 1; v }
    def hasNext: Boolean = i < _size
  }

  final override def constraintDegree: Int = degree.value

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  final override def assign(value: Int): CPOutcome = {
    val id = value - minValue
    if (id < 0 || value > maxValue) Failure
    else if (positions(id) >= _size) Failure
    else if (_size == 1) Suspend
    else assignValue(value) // assign
  }

  @inline private def assignValue(value: Int): CPOutcome = {
    // Notify AC3
    onBoundsL2.enqueue()
    onDomainL2.enqueue()
    onBindL2.enqueue()
    // Notify AC5
    store.notifyBindL1(onBindL1.value, this)
    store.notifyBindIdxL1(onBindIdxL1.value, this)
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    // Notify removed values if necessary
    val removed = onDomainL1.hasValue
    val removedIdx = onDomainIdxL1.hasValue
    if (removed || removedIdx) {
      var i = _size
      while (i > 0) {
        i -= 1
        val v = values(i)
        if (v != value) {
          if (removed) store.notifRemoveL1(onDomainL1.value, this, v)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, v)
        }
      }
    }
    // Update domain
    val id = value - minValue
    val position = positions(id)
    val v = values(0)
    positions(id) = 0
    values(0) = value
    positions(v - minValue) = position
    values(position) = v
    trail() // trail before changes 
    _min = value
    _max = value
    _size = 1
    Suspend
  }

  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
   */
  final override def removeValue(value: Int): CPOutcome = {
    val id1 = value - minValue
    if (id1 < 0 || value > maxValue) Suspend
    else {
      val pos1 = positions(id1)
      if (pos1 >= _size) Suspend
      else if (_size == 1) Failure
      else {
        trail() // trail before changes 
        // Notify removed watchers
        store.notifRemoveL1(onDomainL1.value, this, value)
        store.notifyRemoveIdxL1(onDomainIdxL1.value, this, value)
        onDomainL2.enqueue()
        // Assigned variable
        if (_size == 2) {
          // Notify bind watchers
          store.notifyBindL1(onBindL1.value, this)
          store.notifyBindIdxL1(onBindIdxL1.value, this)
          onBindL2.enqueue()
          // Notify bound watchers
          if (registeredOnBounds.value) {
            store.notifyUpdateBoundsL1(onBoundsL1.value, this)
            store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
            onBoundsL2.enqueue()
          }
          // Update min or max
          if (value == _min) _min = _max
          else _max = _min
        } // Min changed
        else if (_min == value) {
          // Notify bound watchers
          if (registeredOnBounds.value) {
            store.notifyUpdateBoundsL1(onBoundsL1.value, this)
            store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
            onBoundsL2.enqueue()
          }
          // Update min
          var i = _min - minValue + 1
          while (positions(i) >= _size) i += 1
          _min = i + minValue
        } // Notify max watchers
        else if (_max == value) {
          if (registeredOnBounds.value) {
            store.notifyUpdateBoundsL1(onBoundsL1.value, this)
            store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
            onBoundsL2.enqueue()
          }
          // Update max
          var i = _max - minValue - 1
          while (positions(i) >= _size) i -= 1
          _max = i + minValue
        }

        // Update the domain
        _size -= 1
        val v = values(_size)
        val id2 = v - minValue
        val pos2 = positions(id2)
        values(pos1) = v
        values(pos2) = value
        positions(id1) = pos2
        positions(id2) = pos1
        Suspend
      }
    }
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  final override def updateMin(value: Int): CPOutcome = {
    if (value <= _min) Suspend
    else if (value > _max) Failure
    else if (value == _max) assignValue(value)
    else {
      trail() // trail before changes  
      // Remove values
      val removed = onDomainL1.hasValue
      val removedIdx = onDomainIdxL1.hasValue  
      val valueId = value - minValue
      var i = _min - minValue
      while (i < valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + minValue
          val v2 = values(_size)
          val id2 = v2 - minValue
          val pos2 = positions(id2)
          values(pos1) = v2
          values(pos2) = v1
          positions(i) = pos2
          positions(id2) = pos1
          if (removed) store.notifRemoveL1(onDomainL1.value, this, v1)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, v1)
        }
        i += 1
      }
      // Search new min
      while (positions(i) >= _size) i += 1
      _min = i + minValue
      // Notify bounds events
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      onBoundsL2.enqueue()
      onDomainL2.enqueue()
      Suspend
    }
  }
  
  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  final override def updateMax(value: Int): CPOutcome = {
    if (value >= _max) Suspend
    else if (value < _min) Failure
    else if (value == _min) assignValue(value)
    else {
      trail() // trail before changes  
      // Remove values
      val removed = onDomainL1.hasValue
      val removedIdx = onDomainIdxL1.hasValue  
      val valueId = value - minValue
      var i = _max - minValue
      while (i > valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + minValue
          val v2 = values(_size)
          val id2 = v2 - minValue
          val pos2 = positions(id2)
          values(pos1) = v2
          values(pos2) = v1
          positions(i) = pos2
          positions(id2) = pos1
          if (removed) store.notifRemoveL1(onDomainL1.value, this, v1)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, v1)
        }
        i -= 1
      }
      // Search new min
      while (positions(i) >= _size) i -= 1
      _max = i + minValue
      // Notify bounds events
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      onBoundsL2.enqueue()
      onDomainL2.enqueue()
      Suspend
    }
  }

  final override def toString(): String = {
    val buffer = new StringBuffer
    if (name.length > 0) {
      buffer.append(name)
      buffer.append(" ")
    }
    if (_size == 0) buffer.append("phi")
    else if (_size == 1) buffer.append(values(0))
    else {
      val array = new Array[Int](_size)
      System.arraycopy(values, 0, array, 0, _size)
      buffer.append("{")
      buffer.append(array.sorted)
      buffer.append("}")
    }
    buffer.toString
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBind(c: Constraint) {
    degree.incr()
    onBindL2.register(c)
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenBoundsChange(c: Constraint) {
    degree.incr()
    registeredOnBounds.setTrue()
    onBoundsL2.register(c)
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  final override def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    degree.incr()
    onDomainL2.register(c)
    if (trackDelta) c.addSnapshot(this)
  }

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint) {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) {
    degree.incr()
    registeredOnBounds.setTrue()
    onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  final override def callValRemoveWhenValueIsRemoved(c: Constraint) {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) {
    degree.incr()
    onDomainL1.setValue(new PropagEventQueueVarInt(onDomainL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  final override def callValBindWhenBind(c: Constraint) {
    callValBindWhenBind(c, this)
  }

  final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) {
    degree.incr()
    onBindL1.setValue(new PropagEventQueueVarInt(onBindL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onDomainIdxL1.setValue(new PropagEventQueueVarInt(onDomainIdxL1.value, c, variable, idx))
  }

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) {
    degree.incr()
    registeredOnBounds.setTrue()
    onBoundsIdxL1.setValue(new PropagEventQueueVarInt(onBoundsIdxL1.value, c, variable, idx))
  }

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  final override def callValBindIdxWhenBind(c: Constraint, idx: Int) {
    callValBindIdxWhenBind(c, this, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) {
    degree.incr()
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = ???
  def changed(c: Constraint): Boolean = ???
  def minChanged(c: Constraint): Boolean = ???
  def maxChanged(c: Constraint): Boolean = ???
  def boundsChanged(c: Constraint): Boolean = ???
  def oldMin(c: Constraint): Int = ???
  def oldMax(c: Constraint): Int = ???
  def oldSize(c: Constraint): Int = ???
  def deltaSize(c: Constraint): Int = ???
  def delta(c: Constraint): Iterator[Int] = ???
}