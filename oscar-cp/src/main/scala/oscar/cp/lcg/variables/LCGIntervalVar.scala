package oscar.cp.lcg.variables

import oscar.cp.core.CPIntervalVar
import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.core.Literal
import scala.util.Random

class LCGIntervalVar(lcgStore: LCGStore, final override val store: CPStore, varId: Int, initMin: Int, initMax: Int, final override val name: String) extends CPIntervalVar {
  
  // Domain representation with literals
  private[this] val nLiterals = initMax - initMin
  private[this] val literals: Array[Literal] = generateDomain(nLiterals)
  
  @inline final override def min: Int = searchMin
  
  @inline final override def max: Int = searchMax
  
  @inline final override def size: Int = {
    searchMin - searchMax + 1
  }
  
  @inline final override def isEmpty: Boolean = {
    size == 0
  }
  
  @inline final override def isBound: Boolean = {
    searchMin == searchMax
  }
  
  @inline final override def isBoundTo(value: Int): Boolean = {
    val min = searchMin
    if (min < searchMax) false
    else min == value
  }
  
  @inline final override def hasValue(value: Int): Boolean = {
    val min = searchMin
    val max = searchMax
    value >= min && value <= max
  }
  
  @inline final def minGeq (value: Int): Literal = {
    val id = value - initMin - 1
    if (id < 0) lcgStore.trueLit
    else if (id > nLiterals) lcgStore.falseLit
    else literals(id).opposite
  }
  
  @inline final def maxLeq(value: Int): Literal = {
    val id = value - initMin
    if (id > nLiterals) lcgStore.trueLit
    else if (id < 0) lcgStore.falseLit
    else literals(id)
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
  
  final override def constraintDegree: Int = ???
  
  final override def iterator: Iterator[Int] = ???
  
  final override def transform(value: Int): Int = ???
  
  final override def valueAfter(value: Int): Int = ???

  final override def valueBefore(value: Int): Int = ???

  final override def randomValue(rand: Random): Int = ???

  final override def callPropagateWhenBind(c: Constraint): Unit = ???

  final override def callPropagateWhenBoundsChange(c: Constraint): Unit = ???

  final override def callValBindWhenBind(c: Constraint): Unit = ???

  final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar): Unit = ???

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = ???

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar): Unit = ???

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = ???

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = ???

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = ???

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = ???

  final override def assign(value: Int): CPOutcome = ???

  final override def updateMin(value: Int): CPOutcome = ???

  final override def updateMax(value: Int): CPOutcome = ???
  
  // Find the min value in the domain
  @inline private def searchMin: Int = {
    var i = 0
    while (lcgStore.isFalse(literals(i)) && i < nLiterals) i += 1
    initMin + i
  }
  
  // Find the max value in the domain
  @inline private def searchMax: Int = {
    var i = nLiterals - 1
    while (lcgStore.isTrue(literals(i)) && i > 0) i -= 1
    initMin + i + 1
  }
  
  // Build the domain with literals
  @inline private def generateDomain(nLiterals: Int): Array[Literal] = {
    // Build the domain
    val literals = new Array[Literal](nLiterals)
    var i = 0
    while (i < nLiterals) {
      literals(i) = lcgStore.newVariable(varId, "")
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

object LCGIntervalVar {
  
  private[this] var id = 0
  @inline private def nextId(): Int = {
    val i = id; id += 1; i
  }
  
  def apply(initMin: Int, initMax: Int, name: String = "")(implicit lcgStore: LCGStore, store: CPStore): LCGIntervalVar = {
    new LCGIntervalVar(lcgStore, store, nextId(), initMin, initMax, name)
  }
}