package oscar.lcg.searches

import oscar.algo.array.ArrayStack
import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.ConflictAnalyzer
import oscar.lcg.clauses.WLClause

class BackjumpSearch(store: LCGStore) {

  // Number of backtracks of the previous search
  private[this] var _nFails: Int = 0

  // Number of solutions of the previous search
  private[this] var _nSols: Int = 0

  // Number of nodes explored in the previous search
  private[this] var _nNodes: Int = 0

  // True if the previous search was exhaustive
  private[this] var _completed: Boolean = false

  // Solution
  private[this] var _solution: Array[Literal] = null

  // Decision stack
  private[this] val decisionStack = new ArrayStack[Literal](16)

  // Actions to execute in case of solution node
  private[this] val solutionActions = new ArrayStack[() => Unit](2)

  final def nFails: Int = _nFails
  final def nSols: Int = _nSols
  final def nNodes: Int = _nNodes
  final def completed: Boolean = _completed

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions.append(() => action)

  final def start(heuristic: Heuristic, stopCondition: BackjumpSearch => Boolean = _ => false): Unit = {

    // Initializes the search
    _nSols = 0
    _nFails = 0
    _nNodes = 0
    _completed = false

    do {
      // Search for a solution
      val solution = search(heuristic, stopCondition)
      // Forbid the solution if any
      if (_solution != null) {
        if (_solution.length == 0) _completed = true
        else if (_solution.length == 1) _solution(0).assign()
        else store.add(new WLClause(_solution))
      }
      // Repeat until the search is completed
    } while (!_completed && !stopCondition(this))
  }

  @inline private def search(heuristic: Heuristic, stopCondition: BackjumpSearch => Boolean): Unit = {

    var stop = false
    _solution = null

    while (!stop && !stopCondition(this)) {

      // Propagation
      val notFailed = store.propagate()

      if (!notFailed) {
        _nFails += 1
        if (decisionStack.isEmpty) {
          // No solution
          _completed = true
          stop = true
        } else {
          // Analyze
          val literals = store.analyzer.analyze(store.failedLiteral)
          val level = store.analyzer.backjumpLevel
          // Backjump
          while (decisionStack.length > level) {
            store.undoLevel()
            decisionStack.pop()
          }
          // Create and add the clause
          if (literals.length == 1) {
            println(literals(0))
            literals(0).assign()
          } else {
            val litArray = new Array[Literal](literals.length)
            var i = litArray.length
            while (i > 0) { i -= 1; litArray(i) = literals(i) }
            val clause = new WLClause(litArray)
            clause.setup()
          }
        }
      } else {
        // Take a new decision
        val decisionLit = heuristic.decision()
        // New solution
        if (decisionLit == null) {
          _nSols += 1
          solutionActions.foreach(_())
          storeSolution()
          stop = true
        } else {
          // New search level
          _nNodes += 1
          store.newLevel()
          // Apply decision
          decisionLit.assign()
          // Store decision
          decisionStack.append(decisionLit)
          //println("decision " + decisionLit)
        }
      }
    }

    // Pop the remaining nodes
    while (!decisionStack.isEmpty) {
      decisionStack.pop()
      store.undoLevel()
    }
  }

  @inline private def storeSolution(): Unit = {
    _solution = Array.tabulate(decisionStack.length)(i => {
      decisionStack(i).opposite
    })
  }
}