package oscar.cp.lcg.core

import oscar.algo.ArrayQueue
import oscar.algo.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry

class CDCLSolver {

  // Inconsistency
  private[this] var inconsistent: Boolean = false
  private[this] var inconsistentLevel: Int = -1

  // Activity and Decay
  private final val scaleLimit: Double = 100000000
  private[this] var activityStep: Double = 0.5
  private[this] var activityDecay: Double = 0.5
  private[this] var variableStep: Double = 0.5
  private[this] var variableDecay: Double = 0.5

  // Clauses
  private[this] val problemClauses: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val learntClauses: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val explanationClauses: ArrayStack[Clause] = new ArrayStack(128)

  // Variables
  private[this] val variables: ArrayStack[Literal] = new ArrayStack(128)
  private[this] val intervalRef: ArrayStack[Int] = new ArrayStack(128)
  private[this] val values: ArrayStack[LiftedBoolean] = new ArrayStack(128)
  private[this] val reasons: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val levels: ArrayStack[Int] = new ArrayStack(128) // FIXME boxing
  private[this] val activities: ArrayStack[Double] = new ArrayStack(128)

  // Watchers of each literal
  private[this] val watchers: ArrayStack[ArrayQueue[Clause]] = new ArrayStack(128)

  // Trailing queue  
  private[this] val trail: ArrayStack[Literal] = new ArrayStack(100)
  private[this] val trailLevels: ArrayStack[Int] = new ArrayStack(100) // FIXME boxing

  // Propagation queue
  private[this] val queue: ArrayQueue[Literal] = new ArrayQueue(128)

  /** Return a literal that is always true. */
  @inline def trueLit: Literal = ???

  /** Return a literal that is always false. */
  @inline def falseLit: Literal = ???

  /** Return true if the variable is assigned to true. */
  @inline final def isTrue(literal: Literal): Boolean = values(literal.varId) == True

  /** Return true if the variable is assigned to false. */
  @inline final def isFalse(literal: Literal): Boolean = values(literal.varId) == False

  /** Return true if the variable is unassigned. */
  @inline final def isUnassigned(literal: Literal): Boolean = values(literal.varId) == Unassigned

  /** Return the clause responsible of the assignment. */
  @inline final def assignReason(varId: Int): Clause = reasons(varId)

  /** Create a new variable and return its unsigned literal. */
  final def newVariable(ref: Int, name: String): Literal = {
    val varId = values.size
    val literal = new Literal(varId, name)
    variables.append(literal)
    intervalRef.append(ref)
    values.append(Unassigned)
    reasons.append(null)
    levels.append(-1)
    activities.append(0)
    // Watchers for both literals
    watchers.append(new ArrayQueue[Clause](16))
    watchers.append(new ArrayQueue[Clause](16))
    literal
  }

  /** Register the clause as a watcher of literal. */
  @inline final def watch(clause: Clause, literal: Literal): Unit = {
    watchers(literal.id).addLast(clause)
  }

  /** Add a permanent clause to the store. */
  final def addProblemClause(literals: Array[Literal]): Boolean = newClause(literals, false)

  /** Add a backtrable clause to the store. */
  final def addExplanationClause(literals: Array[Literal]): Boolean = ???

  /** */
  final def newClause(literals: Array[Literal], learnt: Boolean): Boolean = {

    if (!learnt) {
      // check for initial satisfiability
    }

    if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // Unit fact
    else {
      // Allocate clause
      val clause = new Clause(this, literals, learnt)
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

        // Bumping
        claBumpActivity(clause)
        var i = 0
        while (i < literals.length) {
          varBumpActivity(literals(i))
          i += 1
        }
      } else {
        problemClauses.append(clause)
      }
      watchers(literals(0).opposite.id).addLast(clause)
      watchers(literals(1).opposite.id).addLast(clause)
      true
    }
  }

  /**
   *  Propagate and conflict analysis
   */
  final def propagate(): Boolean = {
    if (inconsistent) false
    else {
      val conflict = fixedPoint()
      if (conflict == null) true
      else if (decisionLevel == 0) false
      else {
        analyze(conflict)
        cancelUntil(outBacktsLevel)
        record(outLearnt.toArray)
        updateActivities()
        true
      }
    }
  }

  /**
   *  Empty the propagation queue
   *  Return the first inconsistent clause if any
   */
  @inline private def fixedPoint(): Clause = {
    var failReason: Clause = null
    while (!queue.isEmpty && failReason == null) {
      val literal = queue.removeFirst
      val clauses = watchers(literal.id)
      val nClauses = clauses.size
      var i = clauses.size
      while (i > 0 && failReason == null) {
        i -= 1
        val clause = clauses.removeFirst()
        if (clause.isActive) { // remove the clause if not active
          val consistent = clause.propagate(literal)
          if (!consistent) failReason = clause
        }
      }
    }
    queue.clear()
    failReason
  }

  final def enqueue(literal: Literal, from: Clause): Boolean = {
    val varId = literal.varId
    val lboolean = value(literal)
    if (lboolean != Unassigned) {
      if (lboolean == False) false
      else true
    } else {
      // new fact to store
      if (literal.signed) values(varId) = False
      else values(varId) = True
      levels(varId) = trailLevels.size
      reasons(varId) = from
      trail.push(literal)
      queue.addLast(literal)
      true
    }
  }

  final def value(literal: Literal): LiftedBoolean = {
    val assigned = values(literal.varId)
    if (assigned == Unassigned) Unassigned
    else if (literal.signed) assigned.opposite
    else assigned
  }

  final def claBumpActivity(clause: Clause): Unit = {
    clause.activity += activityStep
    if (clause.activity >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def claRescaleActivity(): Unit = {
    var i = 0
    while (i < learntClauses.length) {
      learntClauses(i).activity /= scaleLimit
      i += 1
    }
  }

  final def claDecayActivity(): Unit = activityStep *= activityDecay

  final def varBumpActivity(literal: Literal): Unit = {
    val varId = literal.varId
    activities(varId) += variableStep
    if (activities(varId) >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def varRescaleActivity(): Unit = {
    var i = 0
    while (i < activities.length) {
      activities(i) /= scaleLimit
      i += 1
    }
  }

  final def nAssigns(): Int = trail.size
  final def nVars(): Int = values.size

  final def varDecayActivity(): Unit = variableStep *= variableDecay

  final def updateActivities(): Unit = {
    varDecayActivity()
    claDecayActivity()
  }

  // Conflict-Analysis
  // -----------------

  // These structures are used to build the nogood returned by a conflict analysis.
  private[this] val outLearnt: ArrayStack[Literal] = new ArrayStack[Literal](16)
  private[this] val pReason: ArrayStack[Literal] = new ArrayStack[Literal](16)

  private[this] final var outBacktsLevel: Int = -1
  
  private def conflitAnalysis(initConflict: Clause): Unit = {
    val seen: Array[Boolean] = new Array(values.size) // FIXME
    var counter = 0
    var p: Literal = null
    var conflict: Clause = initConflict
    
    outLearnt.clear()
    outLearnt.append(null) // leave a romm for the asserted literal
  }

  private def analyze(initConflict: Clause): Unit = {

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

  final def record(literals: Array[Literal]): Unit = {
    newClause(literals, true)
    val clause = learntClauses.last
    enqueue(literals(0), clause)
  }

  final def decisionLevel: Int = trailLevels.size

  // Trailing queue
  // --------------

  @inline final def pushState(externalTrail: ReversibleContext): Unit = {
    // push in my structure
    // push in external structure
    externalTrail.trail(new TrailEntry { final override def restore(): Unit = popState() })
  }

  @inline final def popState(): Unit = ???

  @inline private def assume(literal: Literal): Boolean = {
    trailLevels.push(trail.size)
    enqueue(literal, null)
  }

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal.varId
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
  }

  @inline private def cancel(): Unit = {
    var nLevels = trail.size - trailLevels.pop()
    while (nLevels > 0) {
      nLevels -= 1
      undoOne()
    }
  }

  @inline private def cancelUntil(level: Int): Unit = {
    while (trailLevels.size > level) cancel()
  }
}