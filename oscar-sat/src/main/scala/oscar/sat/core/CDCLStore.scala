package oscar.sat.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayQueueInt
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt
import oscar.sat.constraints.Clause
import oscar.sat.constraints.Constraint

/** @author Renaud Hartert ren.hartert@gmail.com */

class CDCLStore {

  // Level in which the store was still consistent
  private[this] var backtrackLevel: Int = Int.MaxValue

  // Clauses
  private[this] val constraints: ArrayStack[Constraint] = new ArrayStack(512)
  private[this] val learntClauses: ArrayStack[Clause] = new ArrayStack(512)

  // Watchers of each literal
  private[this] val watchers: ArrayStack[ArrayQueue[Constraint]] = new ArrayStack(256)

  // Variables store
  private[this] val values: ArrayStack[LiftedBoolean] = new ArrayStack(128)
  private[this] val reasons: ArrayStack[Constraint] = new ArrayStack(128)
  private[this] val levels: ArrayStackInt = new ArrayStackInt(128)
  private[this] val seens: ArrayStackInt = new ArrayStackInt(128)
  private[this] var varStoreSize: Int = 0 // not used yet

  // Trailing queue  
  private[this] var trailAssigned: Array[Int] = new Array[Int](128)
  private[this] var trailExplanations: Array[Clause] = new Array[Clause](32)
  private[this] var trailLevels: Array[Int] = new Array[Int](16)
  private[this] var nTrailAssigned: Int = 0
  private[this] var nTrailExplanations: Int = 0
  private[this] var nTrailLevels: Int = 0

  // Propagation queue
  private[this] val queue: ArrayQueueInt = new ArrayQueueInt(128)

  // Structure for the conflict analysis procedure
  private[this] val outLearnt = new ArrayStackInt(16)
  private[this] val litReason = new ArrayStackInt(16)
  private[this] var conflictingConstraint: Constraint = null

  /** Return the current decision level. */
  @inline final def decisionLevel: Int = nTrailLevels

  final def newVariable(): Int = {
    val varId = varStoreSize
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
    varStoreSize += 1
    // Watchers for both literals
    watchers.append(new ArrayQueue[Constraint](16))
    watchers.append(new ArrayQueue[Constraint](16))
    varId
  }

  /** Register the constraint as a watcher of `litId`. */
  @inline final def watch(constraint: Constraint, litId: Int): Unit = {
    watchers(litId).addLast(constraint)
  }

  @inline final def litValue(litId: Int): LiftedBoolean = {
    val varId = litId >> 1
    val varValue = values(varId)
    if (varValue == Unassigned) Unassigned
    else if ((litId & 1) == 0) varValue
    else varValue.opposite
  }

  @inline final def varValue(varId: Int): LiftedBoolean = values(varId)

  /** Add a permanent clause to the store. */
  final def addProblemClause(literals: Array[Int]): Boolean = {
    val nLiterals = literals.length
    if (nLiterals == 0) false
    else if (nLiterals == 1) enqueue(literals(0), null)
    else {
      val clause = Clause(literals, this)
      constraints.append(clause)
      watch(clause, literals(0) ^ 1)
      watch(clause, literals(1) ^ 1)
      true
    }
  }

  // Literals(0) is the literal to be asserted
  @inline private def buildConflictClause(): Boolean = {
    val nLiterals = outLearnt.length
    if (nLiterals == 0) false
    else if (nLiterals == 1) enqueue(outLearnt(0), null)
    else {
      val literals = outLearnt.toArray
      val maxLitPos = maxLevelLiteral(literals)
      val maxLit = literals(maxLitPos)
      literals(maxLitPos) = literals(1)
      literals(1) = maxLit
      val clause = Clause.learnt(literals, this)
      learntClauses.append(clause)
      watch(clause, literals(0) ^ 1)
      watch(clause, literals(1) ^ 1)
      true
    }
  }

  @inline private def maxLevelLiteral(literals: Array[Int]): Int = {
    var i = literals.length
    var maxLit = -1
    var maxLevel = -1
    while (i > 1) {
      i -= 1
      val varId = literals(i) >> 1
      val level = levels(varId)
      if (level > maxLevel) {
        maxLit = i
        maxLevel = level
      }
    }
    maxLit
  }

  /**
   *  Propagate and conflict analysis
   */
  final def propagate(): Boolean = {
    if (fixedPoint()) true
    else if (nTrailLevels == 0) false
    else {
      analyze()
      undoLevelsUntil(backtrackLevel)
      buildConflictClause()
      false
    }
  }

  @inline private def analyze(): Unit = {

    val seen: Array[Boolean] = new Array(values.length) // FIXME
    var counter = 0
    var litId: Int = -1
    var conflict: Constraint = conflictingConstraint

    outLearnt.clear()
    outLearnt.append(-1) // leave a room for the asserting literal
    backtrackLevel = 0

    do {

      litReason.clear

      if (litId == -1) conflict.explainFail(litReason)
      else conflict.explainAssign(litReason)

      var i = litReason.length
      while (i > 0) {
        i -= 1
        val litId = litReason(i)
        val varId = litId >> 1
        if (!seen(varId)) {
          seen(varId) = true
          val varLevel = levels(varId)
          if (varLevel == nTrailLevels) counter += 1
          else {
            outLearnt.append(litId ^ 1)
            if (varLevel > backtrackLevel) backtrackLevel = varLevel
          }
        }
      }

      // Select next literal to look at
      do {
        litId = trailAssigned(nTrailAssigned - 1)
        conflict = reasons(litId >> 1)
        undoAssignment()
      } while (!seen(litId >> 1))

      counter -= 1

    } while (counter > 0)

    outLearnt(0) = litId ^ 1
  }

  /**
   *  Empty the propagation queue
   *  Return true if no conflict occurs
   */
  @inline private def fixedPoint(): Boolean = {
    var noConflict = true // false if a conflict occurs
    while (!queue.isEmpty && noConflict) {
      val litId = queue.removeFirst
      val constraints = watchers(litId)
      val nConstraints = constraints.size
      var i = nConstraints
      while (i > 0 && noConflict) {
        i -= 1
        val constraint = constraints.removeFirst()
        if (constraint.deleted) { // remove the constraint if not active
          noConflict = constraint.propagate(litId)
          if (!noConflict) conflictingConstraint = constraint
        }
      }
    }

    queue.clear() // possibly not empty
    noConflict
  }

  final def enqueue(litId: Int, reason: Constraint): Boolean = {
    val varId = litId >> 1
    val lboolean = litValue(litId)
    if (lboolean != Unassigned) lboolean == True
    else {
      // New assignment to store
      if ((litId ^ 1) == 0) values(varId) = True
      else values(varId) = False
      reasons(varId) = reason
      levels(varId) = nTrailLevels
      // Trail the new assignment
      newAssignment(litId)
      // Add the literal to the propagation queue
      queue.addLast(litId)
      true
    }
  }

  final def printTrail(): Unit = println("TRAIL = " + trailAssigned.take(nTrailAssigned).mkString("[", ", ", "]"))

  @inline final def newLevel(): Unit = {
    if (trailLevels.length == nTrailLevels) growTrailLevels()
    trailLevels(nTrailLevels) = nTrailAssigned
    nTrailLevels += 1
  }

  @inline private def newAssignment(litId: Int): Unit = {
    if (trailAssigned.length == nTrailAssigned) growTrailAssigned()
    trailAssigned(nTrailAssigned) = litId
    nTrailAssigned += 1
  }

  // Undo the last assignment.
  @inline private def undoAssignment(): Unit = {
    assert(nTrailAssigned > 0)
    nTrailAssigned -= 1
    val litId = trailAssigned(nTrailAssigned)
    val varId = litId >> 1
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
  }

  // Undo the last level.
  @inline private def undoLevel(): Unit = {
    nTrailLevels -= 1
    var nUndo = nTrailAssigned - trailLevels(nTrailLevels)
    while (nUndo > 0) {
      nUndo -= 1
      undoAssignment()
    }
  }

  // Undo the last levels until level.
  @inline private def undoLevelsUntil(level: Int): Unit = {
    while (nTrailLevels > level) undoLevel()
  }

  final def undoAll(): Unit = undoLevelsUntil(0)

  // Used to adapt the length of inner structures.
  @inline private def growTrailAssigned(): Unit = {
    val newTrail = new Array[Int](nTrailAssigned << 1)
    System.arraycopy(trailAssigned, 0, newTrail, 0, nTrailAssigned)
    trailAssigned = newTrail
  }

  // Used to adapt the length of inner structures.
  @inline private def growTrailLevels(): Unit = {
    val newTrail = new Array[Int](nTrailLevels << 1)
    System.arraycopy(trailLevels, 0, newTrail, 0, nTrailLevels)
    trailLevels = newTrail
  }
}