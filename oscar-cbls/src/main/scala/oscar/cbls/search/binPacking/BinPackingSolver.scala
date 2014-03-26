package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.objective.Objective
import oscar.cbls.invariants.core.computation.CBLSSetVar


object BinPackingSolver extends SearchEngineTrait{
  def solveBinPacking(p:BinPackingProblem, maxStep:Int) = {
    val swapNB = new SwapItemsNeighborhood(p)
    val moveNB = new ItemMoveNeighborhood(p)

    (moveNB exhaustBack swapNB) doAllImprovingMoves(maxStep)
  }
}
