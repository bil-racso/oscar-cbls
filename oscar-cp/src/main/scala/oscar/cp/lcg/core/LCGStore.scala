package oscar.cp.lcg.core

import oscar.algo.ArrayQueue
import oscar.algo.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.lcg.core.clauses.Clause
import oscar.algo.reversible.ReversibleInt
import oscar.cp.lcg.variables.LCGIntervalVar
import oscar.algo.array.ArrayStackInt
import oscar.cp.lcg.core.clauses.FalsifiedClause
import oscar.cp.lcg.core.clauses.FalsifiedClause

class TrailRemoveExplanation(explanation: Clause) extends TrailEntry {
  final override def restore(): Unit = {
    println("deleted   : " + explanation)
    explanation.deactive()
  }
}

class LCGStore(store: CPStore) {

  // Level in which the store was still consistent
  private[this] var backtrackLevel: Int = Int.MaxValue

  // Clauses
  private[this] val problemClauses: ArrayStack[Clause] = new ArrayStack(512)
  private[this] val learntClauses: ArrayStack[Clause] = new ArrayStack(512)
  private[this] val explanationClauses: ArrayStack[Clause] = new ArrayStack(512)

  // Watchers of each literal
  private[this] val watchers: ArrayStack[ArrayQueue[Clause]] = new ArrayStack(256)

  // Variables store
  private[this] var variables: Array[Literal] = new Array(128)
  private[this] var intervalRef: Array[LCGIntervalVar] = new Array(128)
  private[this] var values: Array[LiftedBoolean] = new Array(128)
  private[this] var reasons: Array[Clause] = new Array(128)
  private[this] var levels: Array[Int] = new Array(128)
  private[this] var varStoreSize: Int = 0 // not used yet

  // Trailing queue  
  private[this] var trailAssigned: Array[Literal] = new Array[Literal](128)
  private[this] var trailLevels: Array[Int] = new Array[Int](16)
  private[this] var nTrailAssigned: Int = 0
  private[this] var nTrailLevels: Int = 0

  // True variable
  private[this] val trueVariable = newVariable(null, "TRUE_LIT", "FALSE_LIT")
  values(trueVariable.varId) = True // must not be trailed

  // Propagation queue
  private[this] val queue: ArrayQueue[Literal] = new ArrayQueue(128)

  // Clause to propagate before fixed-point
  private[this] val toPropagate: ArrayStack[Array[Literal]] = new ArrayStack(16)

  // Structure for the conflict analysis procedure
  private[this] val outLearnt = new ArrayStack[Literal](16)
  private[this] val litReason = new ArrayStack[Literal](16)
  private[this] var conflictingClause: Clause = null

  /** Return a literal that is always true. */
  @inline final val trueLit: Literal = trueVariable

  /** Return a literal that is always false. */
  @inline final val falseLit: Literal = trueVariable.opposite

  /** Return the current decision level. */
  @inline final def decisionLevel: Int = nTrailLevels

  /** Return true if the variable is assigned to true. */
  @inline final def isTrue(literal: Literal): Boolean = values(literal.varId) == True

  /** Return true if the variable is assigned to false. */
  @inline final def isFalse(literal: Literal): Boolean = values(literal.varId) == False

  /** Return true if the variable is unassigned. */
  @inline final def isUnassigned(literal: Literal): Boolean = values(literal.varId) == Unassigned

  /** Return the clause responsible of the assignment. */
  @inline final def assignReason(varId: Int): Clause = reasons(varId)

  @inline final def backtrackLvl: Int = backtrackLevel
  
  @inline final def nLeanrt: Int = learntClauses.length

  /** Create a new variable and return its unsigned literal. */
  final def newVariable(interval: LCGIntervalVar, name: String, nameOpposite: String): Literal = {
    if (varStoreSize == values.length) growVariableStore()
    val varId = varStoreSize
    val literal = new Literal(varId, name, nameOpposite)
    variables(varStoreSize) = literal
    intervalRef(varStoreSize) = interval
    values(varStoreSize) = Unassigned
    reasons(varStoreSize) = null
    levels(varStoreSize) = -1
    varStoreSize += 1
    // Watchers for both literals
    watchers.append(new ArrayQueue[Clause](16))
    watchers.append(new ArrayQueue[Clause](16))
    literal
  }

  /** Register the clause as a watcher of `literal`. */
  @inline final def watch(clause: Clause, literal: Literal): Unit = {
    watchers(literal.id).addLast(clause)
  }

  /** Return the value of the literal. */
  final def value(literal: Literal): LiftedBoolean = {
    val assigned = values(literal.varId)
    if (assigned == Unassigned) Unassigned
    else if (literal.signed) assigned.opposite
    else assigned
  }

  /** Add a permanent clause to the store. */
  final def addProblemClause(literals: Array[Literal]): Boolean = {
    // assert decision level is root
    newClause(literals, false)
  }

  /**
   *  Add a backtrable clause to the store.
   *  Entailed clauses are discarded.
   */
  final def addExplanationClause(literals: Array[Literal]): Unit = {
    toPropagate.append(literals)
  }

  // Build a new clause
  @inline private def newClause(literals: Array[Literal], learnt: Boolean): Boolean = {

    if (!learnt) {
      // TODO: check for initial satisfiability
    }

    if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // FIXME Need a clause object here
    else {
      // Allocate clause
      val clause = Clause(this, literals, learnt)
      if (learnt) {
        // Pick a second literal to watch
        var maxLit = 0
        var max = -1
        var j = 1
        while (j < literals.length) {
          val level = levels(literals(j).varId)
          if (max < level) {
            max = level
            maxLit = j
          }
          j += 1
        }
        val tmp = literals(1)
        literals(1) = literals(maxLit)
        literals(maxLit) = tmp

        watchers(literals(0).opposite.id).addLast(clause)
        watchers(literals(1).opposite.id).addLast(clause)
        learntClauses.append(clause)
        enqueue(literals(0), clause)
      } else {
        problemClauses.append(clause)
        watchers(literals(0).opposite.id).addLast(clause)
        watchers(literals(1).opposite.id).addLast(clause)
        true
      }
    }
  }

  /**
   *  Propagate and conflict analysis
   */
  final def propagate(): Boolean = {
    if (handleExplanation() && fixedPoint()) {
      // TODO: Notify domains
      true
    } else if (nTrailLevels > 0) {
      analyze()
      undoLevelsUntil(backtrackLevel)
      learn(outLearnt.toArray)
      false
    } else false
  }

  @inline private def analyze(): Unit = {

    val seen: Array[Boolean] = new Array(values.length) // FIXME
    var counter = 0
    var literal: Literal = null
    var conflict: Clause = conflictingClause

    outLearnt.clear()
    outLearnt.append(null) // leave a room for the asserting literal
    backtrackLevel = 0

    do {

      litReason.clear

      if (literal == null) conflict.explainFail(litReason)
      else conflict.explainUnit(litReason)

      var i = litReason.length
      while (i > 0) {
        i -= 1
        val lit = litReason(i)
        val varId = lit.varId
        if (!seen(varId)) {
          seen(varId) = true
          val varLevel = levels(varId)
          if (varLevel == nTrailLevels) counter += 1
          else if (varLevel > 0) {
            outLearnt.append(lit.opposite)
            if (varLevel > backtrackLevel) backtrackLevel = varLevel
          }
        }
      }

      // Select next literal to look at
      do {
        literal = trailAssigned(nTrailAssigned - 1)
        conflict = reasons(literal.varId)
        undoAssignment()
      } while (!seen(literal.varId))

      counter -= 1

    } while (counter > 0)

    outLearnt(0) = literal.opposite
  }

  /**
   *  Empty the propagation queue
   *  Return true if no conflict occurs
   */
  @inline private def fixedPoint(): Boolean = {
    // False if a conflict occurs
    var noConflict = true
    // Empty the propagation queue
    while (!queue.isEmpty && noConflict) {
      val literal = queue.removeFirst

      if (intervalRef(literal.varId) != null) intervalRef(literal.varId).updateAndNotify() // FIXME: dirty hack

      val clauses = watchers(literal.id)
      val nClauses = clauses.size
      var i = clauses.size
      while (i > 0 && noConflict) {
        i -= 1
        val clause = clauses.removeFirst()
        if (clause.isActive) { // remove the clause if not active
          noConflict = clause.propagate(literal)
          if (!noConflict) conflictingClause = clause
        }
      }
    }

    queue.clear() // possibly not empty
    noConflict
  }

  @inline private def handleExplanation(): Boolean = {
    // False if a conflict occurs
    var noConflict = true
    while (!toPropagate.isEmpty && noConflict) {
      
      // Explanation to handle
      val literals = toPropagate.pop()
      val sortedLiterals = literals.filter(lit => lit.varId != trueLit.varId).sortBy(lit => levels(lit.varId)) // FIXME: perf
      
      val clause = Clause(this, sortedLiterals, false)
      println("explained : " + clause)
      watchers(sortedLiterals(0).opposite.id).addLast(clause)
      watchers(sortedLiterals(1).opposite.id).addLast(clause)
      explanationClauses.append(clause)
      intervalRef(sortedLiterals(0).varId).updateAndNotify() // FIXME: dirty hack
      store.trail(new TrailRemoveExplanation(clause))
      noConflict = enqueue(sortedLiterals(0), clause)
      if (!noConflict) conflictingClause = clause
    }
    
    toPropagate.clear()
    noConflict
  }

  @inline private def learn(literals: Array[Literal]): Unit = {
    println("learnt    : " + literals.mkString("(", " ", ")"))
    newClause(literals, true)
  }

  final def enqueue(literal: Literal, reason: Clause): Boolean = {
    val varId = literal.varId
    val lboolean = value(literal) // FIXME: performance
    if (lboolean != Unassigned) lboolean != False
    else {
      // New assignment to store
      if (literal.signed) values(varId) = False
      else values(varId) = True
      reasons(varId) = reason
      levels(varId) = nTrailLevels
      // Trail the new assignment
      newAssignment(literal)
      // Add the literal to the propagation queue
      queue.addLast(literal)
      true
    }
  }

  final def printTrail(): Unit = println("TRAIL = " + trailAssigned.take(nTrailAssigned).mkString("[", ", ", "]"))

  @inline final def newLevel(): Unit = {
    if (trailLevels.length == nTrailLevels) growTrailLevels()
    trailLevels(nTrailLevels) = nTrailAssigned
    nTrailLevels += 1
  }

  @inline private def newAssignment(literal: Literal): Unit = {
    if (trailAssigned.length == nTrailAssigned) growTrailAssigned()
    trailAssigned(nTrailAssigned) = literal
    nTrailAssigned += 1
  }

  // Unfo the last assignment.
  @inline private def undoAssignment(): Unit = {
    assert(nTrailAssigned > 0)
    nTrailAssigned -= 1
    val literal = trailAssigned(nTrailAssigned)
    val varId = literal.varId
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

  // Used to adapt the length of inner structures.
  @inline private def growTrailAssigned(): Unit = {
    val newTrail = new Array[Literal](nTrailAssigned << 1)
    System.arraycopy(trailAssigned, 0, newTrail, 0, nTrailAssigned)
    trailAssigned = newTrail
  }

  // Used to adapt the length of inner structures.
  @inline private def growTrailLevels(): Unit = {
    val newTrail = new Array[Int](nTrailLevels << 1)
    System.arraycopy(trailLevels, 0, newTrail, 0, nTrailLevels)
    trailLevels = newTrail
  }

  // Used to adapt the length of inner structures.
  @inline private def growVariableStore(): Unit = {
    val newSize = varStoreSize * 2
    val newVariables = new Array[Literal](newSize)
    val newIntervalRef = new Array[LCGIntervalVar](newSize)
    val newValues = new Array[LiftedBoolean](newSize)
    val newReasons = new Array[Clause](newSize)
    val newLevels = new Array[Int](newSize)
    System.arraycopy(variables, 0, newVariables, 0, varStoreSize)
    System.arraycopy(intervalRef, 0, newIntervalRef, 0, varStoreSize)
    System.arraycopy(values, 0, newValues, 0, varStoreSize)
    System.arraycopy(reasons, 0, newReasons, 0, varStoreSize)
    System.arraycopy(levels, 0, newLevels, 0, varStoreSize)
    variables = newVariables
    intervalRef = newIntervalRef
    values = newValues
    reasons = newReasons
    levels = newLevels
  }
}