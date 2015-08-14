package oscar.lcg.core

import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayStack
import oscar.lcg.variables.IntVar

class ConflictAnalyzer {

  // Domains of the boolean variables
  private[this] val domains = new ArrayStack[LiftedBoolean](16)
  private[this] val variables = new ArrayStack[IntVar](16)
  private[this] val levels = new ArrayStackInt(16)

  // Structures used to analyze conflicts
  private[this] val graph = new ArrayStack[ArrayStackInt](16)
  private[this] val nogood = new ArrayStackInt(16)
  private[this] val seen = new ArrayStack[Boolean](16) // FIXME
  private[this] var failedLiteral: Int = -1

  // Trailing queue
  private[this] val trailLiterals = new ArrayStackInt(16)
  private[this] val trailLevels = new ArrayStackInt(16)

  newVariable(True) // initialize true and false literals

  /** Return the id of the false literal. */
  val trueLiteral: Int = 0

  /** Return the id of the true literal. */
  val falseLiteral: Int = 1

  /** Return the id of a new boolean variable. */
  def newVariable(): Int = newVariable(Unassigned)

  // Increase inner structures for a new variable and return its id. 
  @inline private def newVariable(domain: LiftedBoolean): Int = {
    val varId = domains.length
    domains.push(domain)
    seen.push(false)
    levels.push(-1)
    graph.push(new ArrayStackInt(16))
    graph.push(new ArrayStackInt(16))
    varId
  }
  
  def newVariable(intVar: IntVar, value: Int): Literal = {
    val varId = domains.length
    seen.push(false)
  }

  /** Return the domain of the variable */
  def value(varId: Int): LiftedBoolean = domains(varId)

  /** Return true if the variable is assigned, false otherwise. */
  def isAssigned(varId: Int): Boolean = domains(varId) == Unassigned

  /** Return true if the variable is assigned to true, false otherwise. */
  def isTrue(varId: Int): Boolean = domains(varId) == True

  /** Return true if the variable is assigned to false, false otherwise. */
  def isFalse(varId: Int): Boolean = domains(varId) == False

  /** Assume litId to be true. */
  def assume(litId: Int): Boolean = {
    trailLevels.push(trailLiterals.length) // new decision level
    explain(litId, Array.empty, 0) // no explanation FIXME
  }

  /** Assume varId to be assigned to value. */
  def assume(varId: Int, value: LiftedBoolean): Boolean = {
    assert(value != Unassigned)
    if (value == False) assume(varId * 2 + 1)
    else assume(varId * 2)
  }

  /** Explain the assignation of litId. */
  def explain(litId: Int, explanation: Array[Int], explanationSize: Int): Boolean = {    
    record(litId, explanation, explanationSize)
    assign(litId)
  }

  /** Explain the assignation of litId. */  
  def explain(litId: Int, explanation: Int): Boolean = {
    record(litId, explanation)
    assign(litId)
  }
  
  def assign(litId: Int): Boolean = {
    // Check contradiction
    val varId = litId >> 1
    val domain = domains(varId)
    val isSigned = (litId & 1) == 1
    if (domain != Unassigned) {
      if (domain == False && isSigned) true
      else { failedLiteral = litId; false }
    } else {
      // Record domain change
      if (isSigned) domains(varId) = False
      else domains(varId) = True
      levels(varId) = trailLevels.length
      trailLiterals.push(litId)
      // Notify variable
      variables(varId).update()
      // End
      true
    }
  }

  /** Explain the fail. */
  def fail(explanation: Array[Int], explanationSize: Int): Boolean = {
    failedLiteral = falseLiteral
    record(falseLiteral, explanation, explanationSize)
    false
  }

  // Record the explanation for litId
  @inline private def record(litId: Int, explanation: Array[Int], explanationSize: Int): Unit = {
    assert(explanationSize <= explanation.length)
    assert(graph(litId).length == 0)
    var i = explanationSize
    while (i > 0) {
      i -= 1
      graph(litId).push(explanation(i))
    }
  }
  
  @inline private def record(litId: Int, explanation: Int): Unit = {
    assert(graph(litId).length == 0)
    graph(litId).push(explanation)
  }

  /** Builds a no good in case of failure */
  def analyze(): ArrayStackInt = {

    var nPaths = 0
    var conflictingLit = failedLiteral
    val currentLevel = trailLevels.size

    // Reset structures
    var i = seen.length
    while (i > 0) { i -= 1; seen(i) = false }
    nogood.clear()

    // Handle conflicting literal if any  
    if (conflictingLit != falseLiteral) {
      val varId = conflictingLit >> 1
      seen(varId) = true
      nPaths = 1
    }

    do {
      // Handle explanations
      while (!graph(conflictingLit).isEmpty) {
        val litId = graph(conflictingLit).pop()
        val varId = litId >> 1
        if (!seen(varId)) {
          seen(varId) = true
          val level = levels(varId)
          if (level == currentLevel) nPaths += 1
          else if (level > 0) nogood.push(litId ^ 1)
        }
      }

      do {
        graph(conflictingLit).clear()
        conflictingLit = trailLiterals.pop()
        val varId = conflictingLit >> 1
        levels(varId) = -1
        domains(varId) = Unassigned
      } while (!seen(conflictingLit >> 1))

      nPaths -= 1

    } while (nPaths > 0)

    nogood.push(conflictingLit ^ 1)
    nogood
  }

  def untrailLevel(): Unit = {
    assert(!trailLevels.isEmpty)
    var i = trailLiterals.length - trailLevels.pop()
    while (i > 0) {
      i -= 1
      val litId = trailLiterals.pop()
      val varId = litId >> 1
      levels(varId) = -1
      domains(varId) = Unassigned
      graph(litId).clear()
    }
  }

  def untrailAll(): Unit = {
    assert(!trailLevels.isEmpty)
    var i = trailLiterals.length - trailLevels(0)
    trailLevels.clear()
    while (i > 0) {
      i -= 1
      val litId = trailLiterals.pop()
      val varId = litId >> 1
      levels(varId) = -1
      domains(varId) = Unassigned
      graph(litId).clear()
    }
  }
