package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.objective.Objective
import oscar.cbls.invariants.core.computation.CBLSSetVar


object BinPackingSolver extends SearchEngineTrait{
  def solveBinPacking(items:Map[Int,Item], bins: Map[Int,Bin], overallViolation:Objective, mostViolatedBins:CBLSSetVar, maxStep:Int) = {
    val swapNB = new SwapItemsNeighborhood(items, bins, overallViolation, mostViolatedBins)
    val moveNB = new ItemMoveNeighborhood(items, bins, overallViolation, mostViolatedBins)

    (moveNB exhaustBack swapNB) doAllImprovingMoves(maxStep)
  }
}
