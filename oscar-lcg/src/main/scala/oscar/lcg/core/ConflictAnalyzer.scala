package oscar.lcg.core

import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayStack
import oscar.lcg.variables.IntVar
import oscar.lcg.literals.LitFalse

class ConflictAnalyzer {

  // Structures used to analyze conflicts
  private[this] val nogood = new ArrayStack[Literal](16)
  private[this] val seenStack = new ArrayStack[Literal](16)
  private[this] val trailLiterals = new ArrayStack[Literal](128)
  private[this] val trailLevels = new ArrayStackInt(64)
  
  def isExplained(literal: Literal): Unit = trailLiterals.push(literal)
  
  def fail(literal: Literal): Unit = ???
  
  def fail(explanation: Array[Literal]): Unit = ???

  /** Builds a no good in case of failure */
  def analyze(conflictingLit: Literal): ArrayStack[Literal] = {
    assert(seenStack.isEmpty)
    
    var nPaths = 0
    val currentLevel = trailLevels.size
    var lit = conflictingLit

    // Handle conflicting literal if any  
    if (conflictingLit != LitFalse) {
      conflictingLit.seen = true
      seenStack.append(conflictingLit)
      nPaths = 1
    }

    do {
      // Handle explanations
      val explanation = lit.explanation
      while (!explanation.isEmpty) {
        val exLit = explanation.pop()
        if (!exLit.seen) {
          exLit.seen = true
          seenStack.append(exLit)
          val level = exLit.level
          if (level == currentLevel) nPaths += 1
          else if (level > 0) nogood.push(exLit.opposite)
        }
      }
      
      assert(lit.explanation.isEmpty)

      do {
        lit = trailLiterals.pop()
        lit.level = -1
      } while (!lit.seen)

      nPaths -= 1

    } while (nPaths > 0)

    nogood.push(lit.opposite)
    
    // Clear seen literals
    while (!seenStack.isEmpty) seenStack.pop().seen = false
    
    nogood
  }

  def newLevel(): Unit = {
    trailLevels.push(trailLiterals.length)
  }
  
  def undoLevel(): Unit = {
    assert(!trailLevels.isEmpty)
    var i = trailLiterals.length - trailLevels.pop()
    while (i > 0) {
      i -= 1
      val lit = trailLiterals.pop()
      lit.level = -1
      lit.explanation.clear()
    }
  }
}
