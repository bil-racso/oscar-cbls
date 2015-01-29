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
import oscar.cp.core.watcher.PropagEventQueueVarInt
import oscar.cp.core.watcher.WatcherListL2

/**
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class CPIntVarAdaptableDomainState(variable: CPIntVarAdaptable, min: Int, max: Int, size: Int) extends TrailEntry {
  final override def restore(): Unit = {
    variable.min = min
    variable.max = max
    variable.size = size
  }
}

class CPIntVarAdaptableDomainType(variable: CPIntVarAdaptable) extends TrailEntry {
  final override def restore(): Unit = variable.setContinuous()
}

class CPIntVarAdaptable(final override val store: CPStore, minValue: Int, maxValue: Int, continuous: Boolean, final override val name: String = "") extends CPIntVar {
  
  // Registered constraints
  private[this] val onBoundsL2 = new WatcherListL2(store)
  private[this] val onBindL2 = new WatcherListL2(store)
  private[this] val onDomainL2 = new WatcherListL2(store)
  private[this] val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  private[this] val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  private[this] val onDomainL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  private[this] val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  private[this] val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  private[this] val onDomainIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)

  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  // True if some constraints are registered on the bounds or removes
  private[this] val registeredOnBounds = new ReversibleBoolean(store, false) // should not change often
  private[this] val registeredOnRemoves = new ReversibleBoolean(store, false) // should not change often

  // Domain representation
  private[this] val nValues = maxValue - minValue + 1
  private[this] var values: Array[Int] = null
  private[this] var positions: Array[Int] = null
  private[this] var offset = minValue
  private[this] var _continuous = continuous
  private[this] var _min = minValue
  private[this] var _max = maxValue
  private[this] var _size = nValues
  
  // Switch to a sparse set if necessacry
  if (!continuous) buildSparse()

  // Used to trail changes in the domain
  private[this] var lastMagic: Long = -1L

  // Trail entry for swith of domain representation
  private[this] val domainType = new CPIntVarAdaptableDomainType(this)

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new CPIntVarAdaptableDomainState(this, _min, _max, _size))
    }
  }

  @inline final override def size: Int = _size

  @inline final override def min: Int = _min

  @inline final override def max: Int = _max

  @inline final override def isContinuous: Boolean = _continuous

  @inline final def min_=(newMin: Int): Unit = _min = newMin

  @inline final def max_=(newMax: Int): Unit = _max = newMax

  @inline final def size_=(newSize: Int): Unit = _size = newSize

  @inline final def setContinuous(): Unit = _continuous = true

  @inline final override def isBound: Boolean = _size == 1

  final override def isBoundTo(value: Int): Boolean = _size == 1 && _min == value

  @inline final override def hasValue(value: Int): Boolean = {
    if (value < _min || value > _max) false
    else if (_continuous) true
    else positions(value - offset) < _size
  }

  final override def randomValue(rand: Random): Int = {
    val r = rand.nextInt(_size)
    if (_continuous) _min + r
    else values(r)
  }

  final override def transform(v: Int): Int = v

  final override def iterator: Iterator[Int] = {
    if (_continuous) iteratorContinuous
    else iteratorSparse
  }

  @inline private def iteratorContinuous: Iterator[Int] = new Iterator[Int] {
    private[this] val max = _max
    private[this] var i = _min - 1
    final override def next(): Int = { i += 1; i }
    final override def hasNext: Boolean = i < max
  }

  @inline private def iteratorSparse: Iterator[Int] = new Iterator[Int] {
    private[this] val array = new Array[Int](_size)
    private[this] var i = 0
    System.arraycopy(values, 0, array, 0, _size)
    final override def next(): Int = { val v = array(i); i += 1; v }
    final override def hasNext: Boolean = i < _size
  }

  /** 
   *  Returns an array containing all the values in the domain.
   *  The array is not sorted.
   */
  final def toArray: Array[Int] = {
    val array = new Array[Int](_size)
    copyDomain(array)
    array
  }
  
  /** 
   *  Fills the array with the values contained in the domain.
   *  Returns the number of values.
   *  The array is not sorted.
   */
  final def toArray(array: Array[Int]): Int = copyDomain(array)
  
  // Copy the domain in the array and return the size of the domain
  @inline private def copyDomain(array: Array[Int]): Int = {
    if (_continuous) {
      var i = _size
      while (i > 0) { i -= 1; array(i) = i + minValue }
    } else System.arraycopy(values, 0, array, 0, _size)
    _size
  }
  
  final override def constraintDegree: Int = degree.value

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  final override def assign(value: Int): CPOutcome = {
    if (value < _min || value > _max) Failure
    else if (_size == 1) Suspend
    else if (_continuous) assignContinuous(value) // assign continuous
    else if (positions(value - offset) >= _size) Failure // gard on sparse
    else assignSparse(value) // assign sparse
  }

  @inline private def assignContinuous(value: Int): CPOutcome = {
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
    val onRemoved = registeredOnRemoves.value
    if (onRemoved) {
      val removed = onDomainL1.hasValue
      val removedIdx = onDomainIdxL1.hasValue
      var i = _min
      while (i <= _max) {
        if (i != value) {
          if (removed) store.notifRemoveL1(onDomainL1.value, this, i)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
        }
        i += 1
      }
    }
    // Update the domain
    trail() // trail before changes
    _min = value
    _max = value
    _size = 1
    Suspend
  }

  @inline private def assignSparse(value: Int): CPOutcome = {
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
    val id = value - offset
    val position = positions(id)
    val v = values(0)
    positions(id) = 0
    values(0) = value
    positions(v - offset) = position
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
    if (value < _min || value > _max) Suspend
    else if (_size == 1) Failure
    else if (_continuous) removeContinuous(value)
    else removeSparse(value)
  }

  @inline private def removeContinuous(value: Int): CPOutcome = {
    if (value == _min) updateMinContinuous(value + 1)
    else if (value == _max) updateMaxContinuous(value - 1)
    else { // Switch the domain representation
      buildSparse()
      removeSparse(value)
    }
  }

  @inline private def buildSparse(): Unit = {
    store.trail(domainType)
    val nValues = _max - _min + 1
    offset = _min
    values = Array.tabulate(nValues)(i => i + offset)
    positions = Array.tabulate(nValues)(i => i)
    _continuous = false
  }

  @inline private def removeSparse(value: Int): CPOutcome = {
    val id1 = value - offset
    val pos1 = positions(id1)
    if (pos1 >= _size) Suspend
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
        var i = _min - offset + 1
        while (positions(i) >= _size) i += 1
        _min = i + offset
      } // Max change
      else if (_max == value) {
        // Notify bound watchers
        if (registeredOnBounds.value) {
          store.notifyUpdateBoundsL1(onBoundsL1.value, this)
          store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
          onBoundsL2.enqueue()
        }
        // Update max
        var i = _max - offset - 1
        while (positions(i) >= _size) i -= 1
        _max = i + offset
      }

      // Update the domain
      _size -= 1
      val v = values(_size)
      val id2 = v - offset
      val pos2 = positions(id2)
      values(pos1) = v
      values(pos2) = value
      positions(id1) = pos2
      positions(id2) = pos1
      Suspend
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
    else if (_continuous) updateMinContinuous(value)
    else updateMinSparse(value)
  }

  @inline private def updateMinContinuous(value: Int): CPOutcome = {
    if (value == _max) assignContinuous(value)
    else {
      // Notify bounds watchers
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      onBoundsL2.enqueue()
      onDomainL2.enqueue()
      // Notify remove watchers if necessary
      val onRemove = registeredOnRemoves.value
      if (onRemove) {
        val removed = onDomainL1.hasValue
        val removedIdx = onDomainIdxL1.hasValue
        var i = _min
        while (i < value) {
          if (removed) store.notifRemoveL1(onDomainL1.value, this, i)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
          i += 1
        }
      }
      trail() // trail before changes 
      _size -= (value - _min)
      _min = value
      Suspend
    }
  }

  @inline private def updateMinSparse(value: Int): CPOutcome = {
    if (value == _max) assignSparse(value)
    else {
      trail() // trail before changes  
      // Remove values
      val removed = onDomainL1.hasValue
      val removedIdx = onDomainIdxL1.hasValue
      val valueId = value - offset
      var i = _min - offset
      while (i < valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + offset
          val v2 = values(_size)
          val id2 = v2 - offset
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
      _min = i + offset
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
    else if (_continuous) updateMaxContinuous(value)
    else updateMaxSparse(value)
  }

  @inline private def updateMaxContinuous(value: Int): CPOutcome = {
    if (value == _min) assignContinuous(value)
    else {
      // Notify bounds watchers
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      onBoundsL2.enqueue()
      onDomainL2.enqueue()
      // Notify remove watchers if necessary
      val onRemove = registeredOnRemoves.value
      if (onRemove) {
        val removed = onDomainL1.hasValue
        val removedIdx = onDomainIdxL1.hasValue
        var i = _max
        while (i > value) {
          if (removed) store.notifRemoveL1(onDomainL1.value, this, i)
          if (removedIdx) store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
          i -= 1
        }
      }
      trail() // trail before changes 
      _size -= (_max - value)
      _max = value
      Suspend
    }
  }

  @inline private def updateMaxSparse(value: Int): CPOutcome = {
    if (value == _min) assignSparse(value)
    else {
      trail() // trail before changes  
      // Remove values
      val removed = onDomainL1.hasValue
      val removedIdx = onDomainIdxL1.hasValue
      val valueId = value - offset
      var i = _max - offset
      while (i > valueId) {
        val pos1 = positions(i)
        if (pos1 < _size) {
          // Update the domain
          _size -= 1
          val v1 = i + offset
          val v2 = values(_size)
          val id2 = v2 - offset
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
      _max = i + offset
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
    if (name.length > 0) buffer.append(s"$name ")
    if (_size == 1) buffer.append(_min)
    else if (_continuous) buffer.append(s"[${_min}, ${_max}]")
    else {
      val array = new Array[Int](_size)
      System.arraycopy(values, 0, array, 0, _size)
      buffer.append("{")
      buffer.append(array.sorted.mkString(", "))
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

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) {
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
    registeredOnRemoves.setTrue()
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

  final override def callValBindWhenBind(c: Constraint, variable: CPIntVar) {
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
    registeredOnRemoves.setTrue()
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

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) {
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

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int) {
    degree.incr()
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }

  final override def isEmpty: Boolean = _size == 0

  final override def valueAfter(value: Int): Int = {
    if (value >= _max) value
    else if (value < _min) _min
    else if (_continuous) value + 1
    else {
      var i = value - offset + 1
      while (positions(i) >= _size) i += 1
      offset + i
    }
  }
  
  
  final override def valueBefore(value: Int): Int = {
    if (value <= _min) value
    else if (value > _max) _max
    else if (_continuous) value - 1
    else {
      var i = value - offset - 1
      while (positions(i) >= _size) i -= 1
      offset + i
    }
  }
  
  final def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    if (_continuous) deltaContinuous(oldMin, oldMax, oldSize)
    else deltaSparse(oldMin, oldMax, oldSize)
  }

  final def changed(c: Constraint): Boolean = changed(c.snapshotsVarInt(this))

  final def minChanged(c: Constraint): Boolean = minChanged(c.snapshotsVarInt(this))

  final def maxChanged(c: Constraint): Boolean = maxChanged(c.snapshotsVarInt(this))

  final def boundsChanged(c: Constraint): Boolean = boundsChanged(c.snapshotsVarInt(this))

  final def oldMin(c: Constraint): Int = oldMin(c.snapshotsVarInt(this))

  final def oldMax(c: Constraint): Int = oldMax(c.snapshotsVarInt(this))

  final def oldSize(c: Constraint): Int = oldSize(c.snapshotsVarInt(this))

  final def deltaSize(c: Constraint): Int = deltaSize(c.snapshotsVarInt(this))

  final def delta(c: Constraint): Iterator[Int] = {
    val sn = c.snapshotsVarInt(this)
    delta(sn.oldMin, sn.oldMax, sn.oldSize)
  }  
  
  @inline private def deltaContinuous(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin to _min - 1).iterator ++ (_max + 1 to oldMax).iterator
  }
  
  @inline private def deltaSparse(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    (oldMin until minValue).iterator ++ deltaSparse(oldSize) ++ (maxValue + 1 to oldMax).iterator
  }

  @inline private def deltaSparse(oldSize: Int): Iterator[Int] = {
    var ind = size
    new Iterator[Int] {
      override def next(): Int = {
        val v = values(ind)
        ind += 1
        v
      }
      override def hasNext: Boolean = {
        ind < oldSize && ind < values.size
      }
    }
  }
}