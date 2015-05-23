package oscar.sat.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt
import oscar.sat.constraints.Constraint
import oscar.sat.constraints.Clause

/** @author Renaud Hartert ren.hartert@gmail.com */
class CDCLStore {
  /*
  // Variables
  private[this] val values: Array[LiftedBoolean] = ???
  private[this] var nVariables: Int = ???
  
  // Explanations
  private[this] val reasons: Array[Constraint] = ???
  private[this] val levels: Array[Int] = ???
  
  // Constraints
  private[this] val constraints: Array[Constraint] = ???
  private[this] val learntClauses: ArrayStack[Clause] = ???
  
  // Watchers 
  private[this] val watchers: Array[ArrayQueue[Clause]] = ???
  
  // Trailing queue  
  private[this] val trail: ArrayStackInt = new ArrayStackInt(100)
  private[this] val trailLevels: ArrayStackInt = new ArrayStackInt(100)

  // Propagation queue
  private[this] val queue: ArrayQueue[Literal] = new ArrayQueue(128)
  
  /** Returns the clause responsible of the assignment */
  @inline final def assignReason(varId: Int): Constraint = reasons(varId)
  
  @inline private def opposite(litId: Int): Int = litId ^ 1
  
  @inline private def variable(litId: Int): Int = litId >> 1
  
  final def allocateVariables(nVariables: Int): Unit = {
    
  }
  
  final def newVar(name: String): Literal = {
    val varId = values.size
    val literal = new Literal(varId, name)
    variables.append(literal)
    watchers.append(new ArrayQueue[Clause](16))
    watchers.append(new ArrayQueue[Clause](16))
    reasons.append(null)
    values.append(Unassigned)
    levels.append(-1)
    // order
    literal
  }

  
  @inline final def watch(clause: Clause, literal: Literal): Unit = {
    watchers(literal.id).addLast(clause)
  }
  
  final def newClause(literals: Array[Literal]): Boolean = newClause(literals, false)

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
      }
      else {
        problemClauses.append(clause)
      }
      watchers(literals(0).opposite.id).addLast(clause)
      watchers(literals(1).opposite.id).addLast(clause)
      true
    }
  }

  /** 
   *  Empty the propagation queue
   *  Return the inconsistent clause if any
   */
  final def propagate(): Clause = {
    var failReason: Clause = null
    while (!queue.isEmpty) {
      val literal = queue.removeFirst
      val clauses = watchers(literal.id)
      val nClauses = clauses.size
      var i = clauses.size
      while (i > 0 && failReason == null) {
        i -= 1
        val clause = clauses.removeFirst()
        val consistent = clause.propagate(literal)
        if (!consistent) failReason = clause
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
  
  var solution: Array[Boolean] = null
  
  final def search(nofConflict: Int, initVarDecay: Double, initClaDecay: Double): LiftedBoolean = {
    
    var conflictC = 0
    
    var complete = false
    
    while (!complete) {
      val conflict = propagate()
      
      // Conflict to learn
      if (conflict != null) {
        conflictC += 1
        if (decisionLevel == 0) return False
        else {
          analyze(conflict)
          cancelUntil(outBacktsLevel)
          record(outLearnt.toArray)
          decayActivities()
        }
      }
      // No conflict
      else {
      
        // No root simplification
        // No filtering of learnt clause
        
        if (nAssigns() == nVars) {
          // Model found
          solution = Array.tabulate(values.length)(i => values(i) == True)
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
          val id = (0 until values.size).filter(values(_) == Unassigned).sortBy(activities(_)).head
          val literal = variables(id)
          assume(literal)
        } 
      }
    }
    
    Unassigned
  }
  
  final def nAssigns(): Int = trail.size
  final def nVars(): Int = values.size


  // ANALYZE

  
  // These structures are used to build the nogood returned by a conflict analysis.
  private[this] val outLearnt: ArrayBuffer[Literal] = new ArrayBuffer[Literal](16)
  private[this] val pReason: ArrayBuffer[Literal] = new ArrayBuffer[Literal](16)
  
  private[this] final var outBacktsLevel: Int = -1
  
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

  // TRAIL

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal.varId
    values(varId) = Unassigned // unasign
    reasons(varId) = null
    levels(varId) = -1
    //order.undo
  }

  @inline private def assume(literal: Literal): Boolean = {
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
  
  final def solve: Boolean = {
    
    var nofConflicts = 100
    var nofLearnts = problemClauses.size / 3
    var status: LiftedBoolean = Unassigned
    
    while (status == Unassigned) {
      status = search(nofConflicts, 0.95, 0.999)
      nofConflicts += nofConflicts/2
      nofLearnts += nofLearnts/10
    }
    
    cancelUntil(0)
    status == True
  }*/
}