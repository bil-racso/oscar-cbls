package oscar.sat.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayStackDouble
import oscar.sat.constraints.Clause
import oscar.sat.heuristics.Heuristic
import oscar.sat.constraints.Constraint

/** @author Renaud Hartert ren.hartert@gmail.com */

class CDCLStore {
  
  private var heuristic: Heuristic = null

  // Clauses and literals
  private[this] val constraints: ArrayStack[Constraint] = new ArrayStack(128)
  private[this] val learntClauses: ArrayStack[Clause] = new ArrayStack(128)

  // Activity and Decay
  private final val scaleLimit: Double = 1000000
  private[this] var activityStep: Double = 0.5
  private[this] var activityDecay: Double = 0.5
  private[this] var variableStep: Double = 0.5
  private[this] var variableDecay: Double = 0.5

  // Watchers of each literal
  private[this] val watchers: ArrayStack[ArrayQueue[Clause]] = new ArrayStack(128)
  
  // Trailing queue  
  private[this] val trail: ArrayStackInt = new ArrayStackInt(100)
  private[this] val trailLevels: ArrayStackInt = new ArrayStackInt(100)

  // Propagation queue
  private[this] val queue: ArrayQueue[Int] = new ArrayQueue(128)

  // Variables structure
  private[this] var values: Array[LiftedBoolean] = new Array(128)
  private[this] var reasons: Array[Constraint] = new Array(128)
  private[this] var levels: Array[Int] = new Array(128)
  private[this] var activities: Array[Double] = new Array(128)
  private[this] var varStoreSize: Int = 0
  
  /** Returns the clause responsible of the assignment */
  @inline final def assignReason(varId: Int): Constraint = reasons(varId)
  
  @inline final def isAssigned(varId: Int): Boolean = values(varId) != Unassigned
  
  @inline final def varActivity(varId: Int): Double = activities(varId)
  
  final def newVar(name: String): Int = {
    if (varStoreSize == values.length) growVariableStore()
    val varId = varStoreSize
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
    activities(varId) = 0.0
    varStoreSize += 1
    watchers.append(new ArrayQueue[Clause](16))
    watchers.append(new ArrayQueue[Clause](16))
    varId
  }

  @inline final def watch(clause: Clause, literal: Int): Unit = {
    watchers(literal).addLast(clause)
  }
  
  private def recordNogood(literals: Array[Int]): Boolean = {
    
    assert(literals != null)
    assert(literals.length > 0)
    
    if (literals.length == 1) enqueue(literals(0), null) // Unit fact
    else {
      // Allocate clause
      val clause = new Clause(this, literals, true)
      
      
      // Pick a second literal to watch
      var maxLit = 0
      var max = -1
      var j = 1
      while (j < literals.length) {
        val level = levels(literals(j) / 2)
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
      
      watchers(literals(0) ^ 1).addLast(clause)
      watchers(literals(1) ^ 1).addLast(clause)
      
      // Add clause to learnt
      learntClauses.append(clause)
      enqueue(literals(0), clause)
    }
  }

  final def newClause(literals: Array[Int]): Boolean = {
  
    // check for initial satisfiability

    if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // Unit fact
    else {
      // Allocate clause
      val clause = new Clause(this, literals, false)
      constraints.append(clause)
      watchers(literals(0) ^ 1).addLast(clause)
      watchers(literals(1) ^ 1).addLast(clause)
      true
    }
  }

  /** 
   *  Empty the propagation queue
   *  Return the inconsistent clause if any
   */
  final def propagate(): Constraint = {
    var failReason: Constraint = null
    while (!queue.isEmpty && failReason == null) {
      val literal = queue.removeFirst()
      val constraints = watchers(literal)
      val nConstrains = constraints.size
      var i = nConstrains
      while (i > 0 && failReason == null) {
        i -= 1
        val constraint = constraints.removeFirst()
        val consistent = constraint.propagate(literal)
        if (!consistent) failReason = constraint
      }
    }
    queue.clear()
    failReason
  }

  final def enqueue(literal: Int, from: Clause): Boolean = {
    val varId = literal / 2
    val lboolean = litValue(literal)
    if (lboolean != Unassigned) {
      if (lboolean == False) false
      else true
    } else {
      // new fact to store
      if ((literal & 1) == 1) values(varId) = False
      else values(varId) = True
      levels(varId) = trailLevels.size
      reasons(varId) = from
      trail.push(literal)
      queue.addLast(literal)
      true
    }
  }

  final def litValue(literal: Int): LiftedBoolean = {
    val assigned = values(literal / 2)
    if (assigned == Unassigned) Unassigned
    else if ((literal & 1) == 1) assigned.opposite
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

  final def varBumpActivity(literal: Int): Unit = {
    val varId = literal / 2
    activities(varId) += variableStep
    heuristic.updateActivity(varId)
    if (activities(varId) >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def varRescaleActivity(): Unit = {
    var i = 0
    while (i < varStoreSize) {
      activities(i) /= scaleLimit    
      heuristic.updateActivity(i)
      i += 1
    }
  }
  
  var solution: Array[Boolean] = null
  var totalConfict = 0
  
  final def search(nofConflict: Int, initVarDecay: Double, initClaDecay: Double): LiftedBoolean = {
    
    var conflictC = 0
    variableDecay = 1 / initVarDecay
    activityDecay = 1 / initClaDecay
    
    heuristic.init()
    
    var complete = false
    
    while (!complete) {
      val conflict = propagate()
      
      // Conflict to learn
      if (conflict != null) {
        conflictC += 1
        totalConfict += 1
        if (decisionLevel == 0) return False
        else {
          analyze(conflict)
          cancelUntil(outBacktsLevel)
          recordNogood(outLearnt.toArray)
          decayActivities()
        }
      }
      // No conflict
      else {
      
        // No root simplification
        // No filtering of learnt clause
        
        if (nAssigns() == nVars) {
          // Model found
          solution = Array.tabulate(varStoreSize)(i => values(i) == True)
          cancelUntil(0)
          return True
        }
        else if (conflictC >= nofConflict) {
          // Reached bound on number of conflicts
          cancelUntil(0)
          return Unassigned
        }
        else {
          // Search heuristic
          val literal = heuristic.nextLiteral()
          assume(literal)
        } 
      }
    }
    
    Unassigned
  }
  
  final def nAssigns(): Int = trail.size
  final def nVars(): Int = varStoreSize

  final def varDecayActivity(): Unit = variableStep *= variableDecay
  
  final def decayActivities(): Unit = {
    varDecayActivity()
    claDecayActivity()
  }

  // ANALYZE

  
  // These structures are used to build the nogood returned by a conflict analysis.
  private[this] val outLearnt: ArrayStackInt = new ArrayStackInt(16)
  private[this] val pReason: ArrayStackInt = new ArrayStackInt(16)
  
  private[this] final var outBacktsLevel: Int = -1
  
  private def analyze(initConflict: Constraint): Unit = {

    val seen: Array[Boolean] = new Array(values.size) // FIXME
    var counter = 0
    var p: Int = -1
    var conflict: Constraint = initConflict

    outLearnt.clear()
    outLearnt.append(-1) // leave a room for the asserting literal
    outBacktsLevel = 0

    do {

      pReason.clear
      if (p == -1) conflict.explainAll(pReason)
      else conflict.explain(pReason)

      // Trace reason for p
      for (literal <- pReason) { // FIXME 
        val varId = literal / 2
        if (!seen(varId)) {
          seen(varId) = true
          val level = levels(varId)
          if (level == decisionLevel) counter += 1
          else if (level > 0) {
            outLearnt.append(literal ^ 1)
            if (level > outBacktsLevel) outBacktsLevel = level
          }
        }
      }

      // Select next literal to look at
      do {
        p = trail.top
        conflict = reasons(p / 2)
        undoOne()
      } while (!seen(p / 2))
        
      counter -= 1
      
    } while (counter > 0)
      
    outLearnt(0) = p ^ 1
  }

  final def decisionLevel: Int = trailLevels.size

  // TRAIL

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal / 2
    values(varId) = Unassigned // unasign
    reasons(varId) = null
    levels(varId) = -1
    heuristic.undo(varId)
  }

  @inline private def assume(literal: Int): Boolean = {
    trailLevels.push(trail.size)
    enqueue(literal, null)
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

  final def print: Unit = {
    var i = 0
    while (i < values.size) {
      println(values(i))
      i += 1
    }
  }
  
  final def solve(h: Heuristic): Boolean = {
    heuristic = h
    var nofConflicts = 100
    var nofLearnts = constraints.size / 3
    var status: LiftedBoolean = Unassigned
    
    while (status == Unassigned) {
      status = search(nofConflicts, 0.98, 0.999)
      nofConflicts += nofConflicts/2
      nofLearnts += nofLearnts/10
    }
    
    cancelUntil(0)
    status == True
  }
  
    // Used to adapt the length of inner structures.
  @inline private def growVariableStore(): Unit = {
    val newSize = varStoreSize * 2
    val newValues = new Array[LiftedBoolean](newSize)
    val newReasons = new Array[Constraint](newSize)
    val newLevels = new Array[Int](newSize)
    val newActivities = new Array[Double](newSize)
    System.arraycopy(values, 0, newValues, 0, varStoreSize)
    System.arraycopy(reasons, 0, newReasons, 0, varStoreSize)
    System.arraycopy(levels, 0, newLevels, 0, varStoreSize)
    System.arraycopy(activities, 0, newActivities, 0, varStoreSize)
    values = newValues
    reasons = newReasons
    levels = newLevels
    activities = newActivities
  }
}