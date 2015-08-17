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

    var depth = 0

    var stop = false
    while (!stop && !stopCondition(this)) {

      // Propagation
      val notFailed = store.propagate()

      if (!notFailed) {
        _nFails += 1
        if (depth == 0) {
          // No solution
          stop = true
        } else {
          // Analyze
          val literals = store.analyzer.analyze(store.failedLiteral)
          val level = store.analyzer.backjumpLevel
          // Backjump
          while (depth > level) {
            depth -= 1
            store.undoLevel()
          }
          // Create and add the clause
          if (literals.length == 1) {
            println(literals(0))
            literals(0).assign()
          } else {
            val litArray = new Array[Literal](literals.length)
            var i = litArray.length
            while (i > 0) { i -= 1; litArray(i) = literals(i) }
            println(litArray.mkString(" "))
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
          stop = true
        } else {
          // New search level
          _nNodes += 1
          depth += 1
          store.newLevel()
          // Apply decision
          decisionLit.assign()
          println("decision " + decisionLit)
        }
      }
    }

    // Pop the remaining nodes
    while (depth > 0) { depth -= 1; store.undoLevel() }
  }
}