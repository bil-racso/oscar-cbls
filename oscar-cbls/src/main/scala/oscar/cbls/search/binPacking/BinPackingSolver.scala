package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait

object BinPackingSolver extends SearchEngineTrait{
  def solveBinPacking(p:BinPackingProblem, maxStep:Int) = {
    (ItemMoveNeighborhood(p) exhaustBack SwapItemsNeighborhood(p)) doAllImprovingMoves(maxStep)
  }
}

