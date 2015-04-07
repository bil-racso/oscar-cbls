package oscar.nogood.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.nogood.decisions.Decision
import oscar.nogood.decisions.LowerEq
import oscar.nogood.decisions.Greater

class RestartConflictSet(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store
  
  private[this] val lastValues = Array.fill(nVariables)(Int.MinValue)

  private[this] val assigned = Array.tabulate(nVariables)(i => i)
  private[this] val nAssignedRev = new ReversibleInt(store, 0)
  private[this] val nDecisionRev = new ReversibleInt(store, 0)
  private[this] var nAssigned = 0
  private[this] var maxDecision = 0

  private[this] val priorities = new Array[Int](nVariables)

  private[this] var conflictDecision: Int = -1
  private[this] var conflictVar: Int = -1
  
  final override def reset(): Unit = {
    conflictDecision = - 1
    conflictVar = -1
  }

  final override def nextDecision: Decision = {
    updateAssigned()
    if (nAssigned == nVariables) null
    else {   
      
      // Trail the number of assigned variables
      nAssignedRev.value = nAssigned
      
      // New decision level
      val decision = nDecisionRev.incr() - 1
      
      // Handle last conflict if any
      if (conflictDecision >= decision) updatePriority(conflictVar)

      // Select the next variable and value
      val varId = nextVariable(decision)
      val variable = variables(varId)
      val minValue = variable.min
      val maxValue = variable.max
      val lastValue = lastValues(varId)
      val value = /*if (minValue <= lastValue && lastValue <= maxValue) lastValue else*/ valHeuristic(varId)

      conflictDecision = decision
      conflictVar = varId

      // Alternatives
      if (value == variable.max) new Greater(variable, value - 1)
      else new LowerEq(variable, value)
    }
  }

  @inline private def nextVariable(nDecision: Int): Int = {
    if (nDecision < maxDecision) nextPriority
    else {
      maxDecision += 1
      nextHeuristic
    }
  }
  
  @inline private def nextPriority: Int = {
    var minId = -1
    var min = Int.MaxValue
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      val priority = priorities(varId)
      if (priority < min) {
        min = priority
        minId = varId
      }
      i += 1
    }
    minId
  }
  
  @inline private def nextHeuristic: Int = {
    var minId = -1
    var min = Int.MaxValue
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      val heuristic = varHeuristic(varId)
      if (heuristic < min) {
        min = heuristic
        minId = varId
      }
      i += 1
    }
    minId
  }

  @inline private def updatePriority(varId: Int): Unit = {
    var i = nVariables
    while (i > 0) {
      i -= 1
      priorities(i) += 1
    }
    priorities(varId) = 0
  }
  
  @inline private def updateAssigned(): Unit = {
    nAssigned = nAssignedRev.value
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      if (variables(varId).isBound) {
        val tmp = assigned(nAssigned)
        assigned(nAssigned) = varId
        assigned(i) = tmp
        lastValues(varId) = variables(varId).min
        nAssigned += 1
      }
      i += 1
    }
  }
}