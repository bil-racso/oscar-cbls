package oscar.lcg.search

import oscar.algo.array.ArrayStack
import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal

class DFSearch(store: LCGStore) {

  private[this] val alternativesStack = new ArrayStack[Iterator[Literal]](100)

  // Number of backtracks of the previous search
  private[this] var _nFails: Int = 0

  // Number of solutions of the previous search
  private[this] var _nSols: Int = 0

  // Number of nodes explored in the previous search
  private[this] var _nNodes: Int = 0
  
  // True if the previous search was exhaustive
  private[this] var _completed: Boolean = false

  // Actions to execute in case of solution node
  private[this] val solutionActions = new ArrayStack[() => Unit](8)

  final def nFails: Int = _nFails
  final def nSols: Int = _nSols
  final def nNodes: Int = _nNodes
  final def completed: Boolean = _completed

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions.append(() => action)
  
  @inline private def expand(heuristic: Heuristic): Boolean = {
    val decision = heuristic.decision()
    if (decision == null) false
    else {
      alternativesStack.push(Iterator(decision, decision.opposite))
      true
    }
  }

  final def start(heuristic: Heuristic, stopCondition: DFSearch => Boolean = _ => false): Unit = {

    // Initializes the search
    alternativesStack.clear()
    _nSols = 0
    _nFails = 0
    _nNodes = 0
    _completed = false

    // Initial propagate
    val failed = store.propagate()
    if (failed) {}
    else {
      
      // Take a first decision
      val isExpandable = expand(heuristic)
      if (!isExpandable) { // the root is a solution
        solutionActions.foreach(_())
        _nSols += 1
      }
      
      // Start the search
      while (!alternativesStack.isEmpty && !stopCondition(this)) {
        
        // New node
        _nNodes += 1
        
        val alternatives = alternativesStack.top
        val alternative = alternatives.next()
        
        val isLast = !alternatives.hasNext     
        if (!isLast) store.newLevel()
        else alternativesStack.pop() // No more decision in the sequence
        
        // Apply decision
        alternative.assign(Array.empty[Literal])
        
        val failed = store.propagate()
        if (!failed) {
          val isExpandable = expand(heuristic)
          if (!isExpandable) {
            _nSols += 1
            solutionActions.foreach(_())
            store.undoLevel()
          }
        } else {
          _nFails += 1
          store.undoLevel()
        }
      }
      
      // Pop the remaining nodes 
      var i = alternativesStack.size
      if (i == 0) _completed = true
      else while (i != 0) {
        store.undoLevel()
        i -= 1
      }
      store.undoLevel()
    }
  }
}