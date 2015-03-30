package oscar.nogood.searches

import oscar.cp.core.variables.CPIntVar
import oscar.algo.reversible.ReversibleInt
import oscar.nogood.decisions.Decision
import oscar.nogood.decisions.LowerEq

class SplitConflictSet(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends NogoodBranching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  private[this] val assigned = Array.tabulate(nVariables)(i => i)
  private[this] val nAssignedRev = new ReversibleInt(store, 0)
  private[this] var maxAssigned = 0

  private[this] val priorities = new Array[Int](nVariables)

  private[this] var conflictAssign: Int = -1
  private[this] var conflictVar: Int = -1
  
  final override def reset(): Unit = {
    maxAssigned = 0
    conflictAssign = -1
  }

  final override def nextDecision: Decision = {
    val nAssigned = countAssigned()
    if (nAssigned == nVariables) null
    else {
      
      // Trail the number of assigned variables
      nAssignedRev.value = nAssigned

      // Handle last conflict if any
      if (conflictAssign > nAssigned) updatePriority(conflictVar)

      // Select the next variable and value
      val varId = nextVariable(nAssigned)
      val variable = variables(varId)
      val minValue = variable.min
      val maxValue = variable.max
      val value = variable.min

      conflictAssign = nAssigned
      conflictVar = varId

       // Decision
      new LowerEq(variable, value)
    }
  }

  @inline private def nextVariable(nAssigned: Int): Int = {
    if (nAssigned < maxAssigned) nextPriority(nAssigned)
    else {
      maxAssigned += 1
      nextHeuristic(nAssigned)
    }
  }
  
  @inline private def nextPriority(nAssigned: Int): Int = {
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
  
  @inline private def nextHeuristic(nAssigned: Int): Int = {
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
    var i = varId
    while (i > 0) {
      i -= 1
      priorities(i) += 1
    }
    priorities(varId) = 0
  }
  
  @inline private def countAssigned(): Int = {
    var nAssigned = nAssignedRev.value
    var i = nAssigned
    while (i < nVariables) {
      val varId = assigned(i)
      if (variables(varId).isBound) {
        val tmp = assigned(nAssigned)
        assigned(nAssigned) = varId
        assigned(i) = tmp
        nAssigned += 1
      }
      i += 1
    }
    nAssigned
  }
}