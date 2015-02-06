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

class TrailRemoveExplanation(explanation: Clause) extends TrailEntry {
  final override def restore(): Unit = explanation.deactive()
}

class LCGStore(store: CPStore) {

  // Level in which the store was still consistent
  private[this] var backtrackLevel: Int = Int.MaxValue

  // Clauses
  private[this] val problemClauses: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val learntClauses: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val explanationClauses: ArrayStack[Clause] = new ArrayStack(128)

  // Variables store
  private[this] var variables: Array[Literal] = new Array(128)
  private[this] var intervalRef: Array[LCGIntervalVar] = new Array(128)
  private[this] var values: Array[LiftedBoolean] = new Array(128)
  private[this] var reasons: Array[Clause] = new Array(128)
  private[this] var levels: Array[Int] = new Array(128) 
  private[this] var varStoreSize: Int = 0 // not used yet

  // Watchers of each literal
  private[this] val watchers: ArrayStack[ArrayQueue[Clause]] = new ArrayStack(128)

  // Trailing queue  
  private[this] var lastMagic = store.magic
  private[this] var trail: Array[Literal] = new Array[Literal](128)
  private[this] var trailSize: Int = 0
  private[this] var currentLevel: Int = 0
  
  // True variable
  private[this] val trueVariable = newVariable(null, "TRUE_VAR")
  values(trueVariable.varId) = True // must not be trailed

  // Propagation queue
  private[this] val queue: ArrayQueue[Literal] = new ArrayQueue(128)

  // Clause to propagate before fixed-point
  private[this] val toPropagate: ArrayStack[Array[Literal]] = new ArrayStack(16)

  /** Return a literal that is always true. */
  @inline final val trueLit: Literal = trueVariable

  /** Return a literal that is always false. */
  @inline final val falseLit: Literal = trueVariable.opposite

  /** Return true if the store is inconsistent. */
  @inline final def isInconsistent: Boolean = currentLevel > backtrackLevel

  /** Return the current decision level. */
  @inline final def decisionLevel: Int = currentLevel

  /** Return true if the variable is assigned to true. */
  @inline final def isTrue(literal: Literal): Boolean = values(literal.varId) == True

  /** Return true if the variable is assigned to false. */
  @inline final def isFalse(literal: Literal): Boolean = values(literal.varId) == False

  /** Return true if the variable is unassigned. */
  @inline final def isUnassigned(literal: Literal): Boolean = values(literal.varId) == Unassigned

  /** Return the clause responsible of the assignment. */
  @inline final def assignReason(varId: Int): Clause = reasons(varId)

  /** Create a new variable and return its unsigned literal. */
  final def newVariable(interval: LCGIntervalVar, name: String): Literal = {
    if (varStoreSize == values.length) growVariableStore()   
    val varId = varStoreSize
    val literal = new Literal(varId, name)
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

  /** Register the clause as a watcher of literal. */
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

  /** Add a new decision to the store. */
  final def addDecision(literal: Literal): Boolean = {
    val noConflict = enqueue(literal, null)
    if (!noConflict) sys.error("inconsistent decision")
    noConflict
  }

  /**
   *  Add a backtrable clause to the store.
   *  Entailed clauses are discarded.
   */
  final def addExplanationClause(literals: Array[Literal]): Unit = {
    toPropagate.append(literals)
  }

  private def handleExplanation(literals: Array[Literal]): Boolean = {

    var watched1 = -1
    var watched2 = -1

    var i = 0
    while (i < literals.length) {
      val lit = literals(i)
      val v = value(lit)
      if (v == True) sys.error("entained explanation")
      else if (v == Unassigned) {
        if (watched1 == -1) watched1 = i
        else watched2 = i
      }
      i += 1
    }

    if (watched1 == -1) {
      // falsified
      false
    } else if (watched2 == -1) {
      // assertive
      sys.error("should not happen right now")
      true
    } else sys.error("entailed or non-assertive clause.")
  }

  // Build a new clause
  @inline private def newClause(literals: Array[Literal], learnt: Boolean): Boolean = {

    if (!learnt) {
      // check for initial satisfiability
    }

    if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // FIXME Need a clause object here
    else {
      // Allocate clause
      val clause = Clause(this, literals, learnt)
      if (learnt) {
        // Add clause to learnt
        learntClauses.append(clause)
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

      } else {
        problemClauses.append(clause)
      }
      watchers(literals(0).opposite.id).addLast(clause)
      watchers(literals(1).opposite.id).addLast(clause)
      true
    }
  }

  private var conflictingClause: Clause = null

  /**
   *  Propagate and conflict analysis
   */
  final def propagate(): Boolean = {
    if (currentLevel > backtrackLevel) false
    else {
      // Call the fixed-point algorithm
      val noConflict = fixedPoint()
      if (noConflict) true
      else {
        backtrackLevel = currentLevel - 1
        false
        //analyze(conflict)
        //cancelUntil(outBacktsLevel)
        //record(outLearnt.toArray)
        //updateActivities()
        //true
      }
      
      
      // TODO Notify changes in domains
      
    }
  }
  
  @inline private def analyze(): Unit = {
    
    val seen = new Array[Boolean](varStoreSize)
    var counter = 0
    var p: Literal = null
    var conflict = conflictingClause
    
    val pReason = new ArrayStack[Literal]()
    val outReason = new ArrayStack[Literal]()
    backtrackLevel = 0
    var trailLevel = trailSize - 1
    
    do {
      pReason.clear()
      
      if (p == null) conflict.explainFail(pReason)
      else conflict.explainUnit(pReason)
      
      for (literal <- pReason) {
        val varId = literal.varId
        if (!seen(varId)) {
          seen(varId) = true
          val level = levels(varId)
          if (level == decisionLevel) counter += 1
          else {
            outReason.append(literal.opposite)
            if (level > backtrackLevel) backtrackLevel = level
          }
        }
      }
      
      do {
        p = trail(trailLevel)
        trailLevel -= 1
        conflict = reasons(p.varId)       
      } while (!(seen(p.varId)))
        
      counter -= 1
    } while (counter > 0)
      
    // at this point, p is the UIP
      
    backtrackLevel = levels(p.varId) - 1 // is that always true ?
  }
  
  /*
   * private def analyze(initConflict: Clause): Unit = {

    val seen: Array[Boolean] = new Array(values.size) // FIXME
    var counter = 0
    var p: Literal = null
    var conflict: Clause = initConflict

    outLearnt.clear()
    outLearnt.append(null) // leave a room for the asserting literal
    outBacktsLevel = 0

    do {

      pReason.clear
      if (p == null) conflict.explainAll(pReason)
      else conflict.explain(pReason)

      // Trace reason for p
      for (literal <- pReason) { // FIXME 
        val varId = literal.varId
        if (!seen(varId)) {
          seen(varId) = true
          val level = levels(varId)
          if (level == decisionLevel) counter += 1
          else if (level > 0) {
            outLearnt.append(literal.opposite)
            if (level > outBacktsLevel) outBacktsLevel = level
          }
        }
      }

      // Select next literal to look at
      do {
        p = trail.top
        conflict = reasons(p.varId)
        undoOne()
      } while (!seen(p.varId))
        
      counter -= 1
      
    } while (counter > 0)
      
    outLearnt(0) = p.opposite
  }
   */

  /**
   *  Empty the propagation queue
   *  Return true if no conflict occurs
   */
  @inline private def fixedPoint(): Boolean = {

    // False if a conflict occurs
    var noConflict = true

    // New explanation to handle
    while (!toPropagate.isEmpty && noConflict) {
      val literals = toPropagate.pop()
      noConflict = handleExplanation(literals)
    }

    // Empty the propagation queue
    while (!queue.isEmpty && noConflict) {
      val literal = queue.removeFirst
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

  final def enqueue(literal: Literal, from: Clause): Boolean = {
    val varId = literal.varId
    val lboolean = value(literal)
    if (lboolean != Unassigned) lboolean != False
    else {
      // new fact to store
      if (literal.signed) values(varId) = False
      else values(varId) = True
      levels(varId) = currentLevel
      reasons(varId) = from
      trailAssignment(literal) // Increase the trail here
      // Notify corresponding variable
      intervalRef(varId).updateAndNotify()
      queue.addLast(literal)
      true
    }
  }
  
  class TrailBactrackLevel extends TrailEntry {
    @inline final override def restore(): Unit = {
      currentLevel -= 1
      if (currentLevel <= backtrackLevel) {
        backtrackLevel = Int.MaxValue
      }
    }
  }

  class TrailUndoOne extends TrailEntry {
    @inline final override def restore(): Unit = popAssignment()
  }

  final def printTrail(): Unit = println("TRAIL = " + trail.take(trailSize).mkString("[", ", ", "]"))

  @inline private def popAssignment(): Unit = {
    if (trailSize > 0) {
      //printTrail() // TODO : remove
      trailSize -= 1
      val literal = trail(trailSize)
      val varId = literal.varId
      values(varId) = Unassigned
      reasons(varId) = null
      levels(varId) = -1
    }
  }

  @inline final def newDecisionLevel(): Unit = {
    currentLevel += 1
    store.trail(levelTrailEntry)
  }

  // Static trail entries
  private[this] val undoTrailEntry = new TrailUndoOne
  private[this] val levelTrailEntry = new TrailBactrackLevel

  @inline private def trailAssignment(literal: Literal): Unit = {
    if (trail.length == trailSize) growTrail()
    trail(trailSize) = literal
    trailSize += 1
    store.trail(undoTrailEntry)
  }

  @inline private def growTrail(): Unit = {
    val newTrail = new Array[Literal](trail.length * 2)
    System.arraycopy(trail, 0, newTrail, 0, trail.length)
    trail = newTrail
  }
  
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