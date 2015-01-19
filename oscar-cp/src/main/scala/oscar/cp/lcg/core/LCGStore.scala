package oscar.cp.lcg.core

import oscar.algo.ArrayQueue
import oscar.algo.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.lcg.core.clauses.Clause

class LCGStore(store: CPStore) {

  // Inconsistency
  private[this] var inconsistent: Boolean = false
  private[this] var inconsistentLevel: Int = -1
  
  // Level in which the store was consistent
  private[this] var backtrackLevel: Int = -1

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
  private[this] val trailVar: ArrayStack[Literal] = new ArrayStack(128)
  private[this] val trailVarLevels: ArrayStack[Int] = new ArrayStack(128) // FIXME boxing
  private[this] val trailExplain: ArrayStack[Clause] = new ArrayStack(128)
  private[this] val trailExplainLevels: ArrayStack[Int] = new ArrayStack(128) // FIXME boxing

  // True variable
  private[this] val trueVariable = newVariable(-1, "TRUE_VAR")
  values(trueVariable.varId) = True // should not be trailed

  // Propagation queue
  private[this] val queue: ArrayQueue[Literal] = new ArrayQueue(128)

  // Clause to propagate before fixed-point
  private[this] val toPropagate: ArrayStack[Clause] = new ArrayStack(16)

  /** Return a literal that is always true. */
  @inline final val trueLit: Literal = trueVariable

  /** Return a literal that is always false. */
  @inline final val falseLit: Literal = trueVariable.opposite

  /** Return true if the store is inconsistent. */
  @inline final def isInconsistent: Boolean = inconsistent

  /** Return the current decision level. */
  @inline final def decisionLevel: Int = trailVarLevels.size

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

  /** 
   *  Add a backtrable clause to the store. 
   *  Entailed clauses are discarded.  
   */
  final def addExplanationClause(literals: Array[Literal]): Boolean = ???

  @inline private def newProblemClause(literals: Array[Literal]): Boolean = {

    assert(trailVarLevels.size == 0)

    // TODO Check for initial satisfiability

    val clause = Clause(this, literals, false)
    problemClauses.append(clause)
    watchers(literals(0).opposite.id).addLast(clause)
    watchers(literals(1).opposite.id).addLast(clause)
    true
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

  /**
   *  Propagate and conflict analysis
   */
  final def propagate(): Boolean = {
    if (decisionLevel > inconsistentLevel) false
    else {
      // Trail if necessary
      trail()
      // Call the fixed-point algorithm
      val conflict = fixedPoint()
      if (conflict == null) true
      else {
        // Backtrack
        inconsistentLevel = decisionLevel - 1
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

  /**
   *  Empty the propagation queue
   *  Return the first inconsistent clause if any
   */
  @inline private def fixedPoint(): Clause = {
    // Clause responsible of the conflict
    var failReason: Clause = null
    // New clauses to propagate
    while (!toPropagate.isEmpty && failReason == null) {
      val clause = toPropagate.pop()
      val consistent = clause.setup()
      if (!consistent) failReason = clause
    }
    // Empty the propagation queue
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
      levels(varId) = trailVarLevels.size
      reasons(varId) = from
      trailVar.push(literal)
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

  // Conflict-Analysis
  // -----------------

  // These structures are used to build the nogood returned by a conflict analysis.
  private[this] val outLearnt: ArrayStack[Literal] = new ArrayStack[Literal](16)
  private[this] val pReason: ArrayStack[Literal] = new ArrayStack[Literal](16)

  private[this] final var outBacktsLevel: Int = -1

  /*private def analyze(initConflict: Clause): Unit = {

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
        p = trailVar.top
        conflict = reasons(p.varId)
        undoAssignment()
      } while (!seen(p.varId))

      counter -= 1

    } while (counter > 0)

    outLearnt(0) = p.opposite
  }

  final def record(literals: Array[Literal]): Unit = {
    newClause(literals, true)
    val clause = learntClauses.last
    enqueue(literals(0), clause)
  }*/

  // Trailing queue
  // --------------

  // Used for efficient trailing
  private[this] var lastMagic: Long = -1

  class TrailUnit(level: Int) extends TrailEntry {
    @inline final override def restore(): Unit = cancelUntil(level)
  }

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      // External trail
      lastMagic = contextMagic
      store.trail(new TrailUnit(decisionLevel))
      // Inner trail
      trailVarLevels.push(trailVar.size) // assignments
      trailExplainLevels.push(trailExplain.size) // explanations
    }
  }

  @inline private def undoAssignment(): Unit = {
    assert(trailVar.size > 0)
    val literal = trailVar.pop()
    val varId = literal.varId
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
  }

  @inline private def undoExplanation(): Unit = {
    assert(trailExplain.size > 0)
    val explanation = trailExplain.pop()
    explanation.deactive()
  }

  @inline private def undo(): Unit = {
    var nLevels = trailVar.size - trailVarLevels.pop()
    while (nLevels > 0) {
      nLevels -= 1
      undoAssignment()
      undoExplanation()
    }
  }

  @inline private def cancelUntil(level: Int): Unit = {
    while (trailVarLevels.size > level) undo()
  }
}