package oscar.lcg.variables

import scala.util.Random
import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.watcher.WatcherListL2
import oscar.algo.reversible.TrailEntry
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.core.CDCLStore
import oscar.lcg.core.Literal

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGIntervalTrailEntry(variable: LCGIntervalVarImpl, min: Int, max: Int) extends TrailEntry {
  @inline final override def restore(): Unit = variable.restore(min, max)
}

class LCGIntervalVarImpl(final override val cdclStore: CDCLStore, final override val cpStore: CPStore, varId: Int, initMin: Int, initMax: Int, final override val name: String) extends LCGIntervalVar {
  
  // Watchers
  private[this] val boundWatchers = new WatcherListL2(cpStore)
  private[this] val assignWatchers = new WatcherListL2(cpStore)
  
  // Domain representation with literals
  private[this] val nLiterals = initMax - initMin
  private[this] val literals: Array[Literal] = generateDomain(nLiterals)
  private[this] var _size: Int = nLiterals + 1
  private[this] var _min: Int = initMin
  private[this] var _max: Int = initMax
  
  // Last trail
  private[this] var lastMagic: Long = -1L
  
  // Trail the state of the domain
  @inline private def trail(): Unit = {
    val contextMagic = cpStore.magic
    if (contextMagic != lastMagic) {
      lastMagic = contextMagic
      cpStore.trail(new LCGIntervalTrailEntry(this, _min, _max))
    }
  }
  
  // Restore a previous state
  @inline final def restore(oldMin: Int, oldMax: Int): Unit = {
    _min = oldMin; _max = oldMax
    _size = oldMax - oldMin + 1
  }
  
  @inline final override def min: Int = _min
  
  @inline final override def max: Int = _max
  
  @inline final override def size: Int = _size
  
  @inline final override def isAssigned: Boolean = _min == _max
  
  @inline final override def isAssignedTo(value: Int): Boolean = {
    _min == value && _max == value
  }
  
  @inline final override def contains(value: Int): Boolean = {
    value >= _min && value <= _max
  }
  
  @inline final override def greaterEqual(value: Int): Literal = {
    val id = value - initMin - 1
    if (id < 0) cdclStore.trueLit
    else if (id >= nLiterals) cdclStore.falseLit
    else literals(id).opposite
  }
  
  @inline final override def lowerEqual(value: Int): Literal = {
    val id = value - initMin
    if (id >= nLiterals) cdclStore.trueLit
    else if (id < 0) cdclStore.falseLit
    else literals(id)
  }
  
  final override def updateAndNotify(): Unit = {
    trail() // trail before changes 
    // Update the domain
    _min = searchMin
    _max = searchMax
    _size = _max - _min + 1
    // Notify
    boundWatchers.enqueue()
    if (_size == 1) assignWatchers.enqueue()
  }
  
  // Find the min value in the domain
  @inline private def searchMin: Int = {
    var i = _min - initMin
    while (i < nLiterals && cdclStore.isFalse(literals(i))) i += 1
    initMin + i
  }
  
  // Find the max value in the domain
  @inline private def searchMax: Int = {
    var i = _max - initMin - 1
    while (i >= 0 && cdclStore.isTrue(literals(i))) i -= 1
    i + initMin + 1
  }
    
  final override def callWhenBoundsChange(constraint: LCGConstraint): Unit = {
    boundWatchers.register(constraint)
  }
  
  final override def callWhenAssigned(constraint: LCGConstraint): Unit = {
    assignWatchers.register(constraint)
  }
  
  // Build the domain with literals
  @inline private def generateDomain(nLiterals: Int): Array[Literal] = {
    // Build the domain
    val literals = new Array[Literal](nLiterals)
    var i = 0
    while (i < nLiterals) {
      literals(i) = cdclStore.newVariable(this, "[" + name + " <= " + (i + initMin) + "]", "[" + (i + initMin + 1) + " <= " + name + "]")
      i += 1
    }
    // Add consistency constraints
    i = nLiterals - 1
    while (i > 0) {
      i -= 1
      val lit1 = literals(i)
      val lit2 = literals(i + 1)
      cdclStore.addProblemClause(Array(lit1.opposite, lit2))
    }
    // Returns the domain
    literals
  } 
  
  final override def toString: String = {
    if (_size == 1) _min.toString
    else s"[${_min}, ${_max}]"
  }
}