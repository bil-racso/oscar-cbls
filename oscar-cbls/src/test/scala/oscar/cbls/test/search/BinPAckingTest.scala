package oscar.cbls.test.search

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}
import oscar.cbls.objective.Objective
import oscar.cbls.search.binPacking._
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.search.binPacking.ItemMoveNeighborhood
import oscar.cbls.search.moves.Neighborhood

/**
 * Created by rdl on 24/04/2014.
 */
object BinPackingTest extends CBLSModel with App{

  //the index is the first element of the couple
  def indexList(l:List[Int]):List[(Int,Int)] = null

  val itemSizes = List(20, 5, 6, 7, 3, 5, 9, 7, 3, 5)

  val binSizes = List(20, 20, 300)

  val problem = BinPackingProblem(itemSizes,binSizes,s, c, 0)

  s.close()

  val x = ItemMoveNeighborhood(problem) exhaust ItemsSwapNeighborhood(problem)
  x.verbose = true
  x.doAllImprovingMoves(100)

  println(problem)
}
