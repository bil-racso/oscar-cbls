package oscar.cp.lcg.variables

import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.core.Literal
import scala.util.Random
import oscar.cp.lcg.constraints.LCGConstraint
import oscar.cp.core.watcher.WatcherListL2

class LCGIntervalVarImpl(final override val lcgStore: LCGStore, final override val store: CPStore, varId: Int, initMin: Int, initMax: Int, final override val name: String) extends LCGIntervalVar {
  
  // Watchers
  private[this] val boundWatchers = new WatcherListL2(store)
  private[this] val bindWatchers = new WatcherListL2(store)
  
  // Domain representation with literals
  private[this] val nLiterals = initMax - initMin
  private[this] val literals: Array[Literal] = generateDomain(nLiterals)
  
  @inline final override def min: Int = searchMin
  
  @inline final override def max: Int = searchMax
  
  @inline final override def size: Int = {
    searchMin - searchMax + 1
  }
  
  @inline final override def isAssigned: Boolean = {
    searchMin == searchMax
  }
  
  @inline final override def isAssignedTo(value: Int): Boolean = {
    val min = searchMin
    if (min < searchMax) false
    else min == value
  }
  
  @inline final override def contains(value: Int): Boolean = {
    val min = searchMin
    val max = searchMax
    value >= min && value <= max
  }
  
  @inline final override def minGeq (value: Int): Literal = {
    val id = value - initMin - 1
    if (id < 0) lcgStore.trueLit
    else if (id > nLiterals) lcgStore.falseLit
    else literals(id).opposite
  }
  
  @inline final override def maxLeq(value: Int): Literal = {
    val id = value - initMin
    if (id > nLiterals) lcgStore.trueLit
    else if (id < 0) lcgStore.falseLit
    else literals(id)
  }
  
  final override def updateAndNotify(): Unit = {
    boundWatchers.enqueue()
    // Check if the bounds have changed (need a reversible int)
    // if so, notify the corresponding constraints
  }
  
  final override def callWhenBoundsChange(constraint: LCGConstraint): Unit = {
    boundWatchers.register(constraint)
  }
  
  final override def toString: String = {
    var i = initMin - 1
    literals.map(l => {
      i += 1
      if (lcgStore.isTrue(l)) s"$i:T"
      else if (lcgStore.isFalse(l)) s"$i:F"
      else s"$i:_"
    }).mkString("[", ", ", s", $initMax:T]")   
  }
  
  // Find the min value in the domain
  @inline private def searchMin: Int = {
    var i = 0
    while (lcgStore.isFalse(literals(i)) && i < nLiterals) i += 1
    initMin + i
  }
  
  // Find the max value in the domain
  @inline private def searchMax: Int = {
    var i = 0
    while (i < nLiterals && !lcgStore.isTrue(literals(i))) i += 1
    initMin + i
  }
  
  // Build the domain with literals
  @inline private def generateDomain(nLiterals: Int): Array[Literal] = {
    // Build the domain
    val literals = new Array[Literal](nLiterals)
    var i = 0
    while (i < nLiterals) {
      literals(i) = lcgStore.newVariable(this, "<= " + (i + initMin))
      i += 1
    }
    // Add consistency constraints
    i = nLiterals - 1
    while (i > 0) {
      i -= 1
      val lit1 = literals(i)
      val lit2 = literals(i + 1)
      lcgStore.addProblemClause(Array(lit1.opposite, lit2))
    }
    // Returns the domain
    literals
  }
  
}