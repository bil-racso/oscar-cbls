package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.scheduling.model.Activity
import oscar.cbls.invariants.lib.logic.TranslatedDenseCluster
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar, Store, CBLSIntConst}
import oscar.cbls.objective.Objective
import oscar.cbls.invariants.lib.minmax.ArgMaxArray

object BinPackingSolver extends SearchEngineTrait{
  def solveBinPacking(p:BinPackingProblem, maxStep:Int) = {

    (ItemMoveNeighborhood(p) exhaustBack ItemsSwapNeighborhood(p)) doAllImprovingMoves(maxStep)

  }


}

