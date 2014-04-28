package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar, Store, CBLSIntConst}
import oscar.cbls.search.moves._
import oscar.cbls.search.moves.AssingMove

object BinPackingSolver extends SearchEngineTrait {
  def solveBinPacking(p: BinPackingProblem, maxStep: Int) = {

    val x = MoveItem(p) exhaustBack SwapItems(p) orElse JumpSwapItems(p)
    x.doAllImprovingMoves(maxStep)

  }
}

/**moves item away from most violated bin*/
case class MoveItem(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait{

  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove():SearchResult = {

    val oldViolation:Int = p.overallViolation.Objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose) println("ItemMoveNeighborhood: problem is solved")
      return ProblemSolved
    }
    val bin1 = p.bins(selectFirst(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose) println("ItemMoveNeighborhood: problem is solved")
      return ProblemSolved
    }

    for(itemId <- bin1.items.value; item = p.items(itemId)){
      for(bin2 <- p.bins if bin2._2.number != bin1.number && bin2._2.violation.value == 0){
        //if (verbose) println("ItemMoveNeighborhood: trying to move item " + item + " to bin " + bin2._2)
        val objAfter = p.overallViolation.assignVal(item.bin, bin2._1)
        if(objAfter < oldViolation){
         // if (verbose) println("ItemMoveNeighborhood: move found: move " + item + " to bin " + bin2._2 + " objAfter " + objAfter )
          return AssingMove(item.bin,bin2._1,objAfter)
        }
      }
    }
    if (verbose) println("ItemMoveNeighborhood: no improvement found")
    NoMoveFound
  }
}


/**swaps items of different sizes out of most violated bin*/
case class SwapItems(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait{

  val itemList:List[Item] = p.items.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.Objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose) println("ItemsSwapNeighborhood: problem is solved")
      return ProblemSolved
    }

    val bin1 = p.bins(selectFirst(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose) println("ItemsSwapNeighborhood: problem is solved")
      return ProblemSolved
    }

    for(itemId <- bin1.items.value; item1 = p.items(itemId)){
      for(item2 <- itemList if item2.bin.value != bin1.number && item1.size != item2.size){
        val objAfter = p.overallViolation.swapVal(item1.bin, item2.bin)
        if(objAfter < oldViolation){
          //if (verbose) println("ItemsSwapNeighborhood: move found: swapping bins of " + item1 + " and " + item2 + " objAfter:" + objAfter)
          return SwapMove(item1.bin,item2.bin,objAfter)
        }
      }
    }
    if (verbose) println("ItemsSwapNeighborhood: no improvement found")
    NoMoveFound
  }
}

/** this move performs a random move in the search space.
  * it is a swap of two items of different sizes, from different bins
  */
case class JumpSwapItems(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait {

  val itemList: List[Item] = p.items.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    if (p.mostViolatedBins.value.isEmpty) {
      if (verbose) println("JumpNeighborhood: problem is solved")
      return ProblemSolved
    }

    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if (bin1.violation.value == 0) {
      if (verbose) println("JumpNeighborhood: problem is solved")
      return ProblemSolved
    }

    selectFrom2[Item,Item](bin1.items.value.map(p.items(_)),
      itemList,
      (item1,item2) =>  item2.bin.value != item1.bin.value && item1.size != item2.size)
    match {
      case (item1,item2) => {
        if (verbose) println("JumpNeighborhood: swapping bins of " + item1 + " and " + item2)
        return SwapMove(item1.bin, item2.bin, 0)
      }
      case null => {
        if (verbose) println("JumpNeighborhood: no move found")
        NoMoveFound
      }
    }
  }
}

