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
      if (verbose >= 2) println("ItemMoveNeighborhood: problem is solved")
      return ProblemSolved
    }
    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemMoveNeighborhood: problem is solved")
      return ProblemSolved
    }

    for(itemId <- bin1.items.value; item = p.items(itemId)){
      for(bin2 <- p.bins if bin2._2.number != bin1.number && bin2._2.violation.value == 0){
        val objAfter = p.overallViolation.assignVal(item.bin, bin2._1)
        if(objAfter < oldViolation){
           if (verbose >= 2) println("ItemMoveNeighborhood: move found: move " + item + " to bin " + bin2._2 + " objAfter " + objAfter )
          return AssingMove(item.bin,bin2._1,objAfter, "ItemMoveNeighborhood")
        }
      }
    }
    if (verbose >= 2) println("ItemMoveNeighborhood: no improvement found")
    NoMoveFound
  }
}

/**swaps items of different sizes, one of them being in one of the mostViolated bins. */
case class SwapItems(p:BinPackingProblem, best:Boolean = false)
  extends StatelessNeighborhood with SearchEngineTrait{

  val itemList:List[Item] = p.items.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.Objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose >= 2) println("ItemsSwapNeighborhood: problem is solved")
      return ProblemSolved
    }

    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemsSwapNeighborhood: problem is solved")
      return ProblemSolved
    }

    (if (best)
      selectMin2(bin1.items.value.map(p.items(_)), itemList,
        (item1:Item,item2:Item) => p.overallViolation.swapVal(item1.bin, item2.bin),
        (item1:Item,item2:Item) => item2.bin.value != bin1.number && item1.size != item2.size)
    else selectFirst2(bin1.items.value.map(p.items(_)), itemList,
      (item1:Item,item2:Item) => item2.bin.value != bin1.number
        && item1.size != item2.size &&
        p.overallViolation.swapVal(item1.bin, item2.bin) < oldViolation))
    match {
      case (item1, item2) => {
        val newObj = p.overallViolation.swapVal(item1.bin, item2.bin)
        if(newObj < oldViolation) SwapMove(item1.bin, item2.bin, newObj, "ItemsSwapNeighborhood")
        else{
          if (verbose >= 2) println("ItemsSwapNeighborhood: no improvement found")
          NoMoveFound
        }
      }
      case null => {
        if (verbose >= 2) println("ItemsSwapNeighborhood: no improvement found")
        NoMoveFound
      }
    }
  }
}

/** this move performs a random move in the search space.
  * it is a swap of two items of different sizes, from different bins
  */
case class JumpSwapItems(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait {

  val itemList: List[Item] = p.items.toList.map(_._2)
  val binList: List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.Objective.value

    val bin1:Bin = selectFrom(binList, (bin:Bin) => bin.violation.value > 0)

    if (bin1 == null) {
      if (verbose >= 2) println("JumpNeighborhood: problem is solved")
      return ProblemSolved
    }

    selectFrom2[Item,Item](bin1.items.value.map(p.items(_)),
      itemList,
      (item1,item2) =>  item2.bin.value != item1.bin.value && item1.size != item2.size &&  p.overallViolation.swapVal(item1.bin, item2.bin) > oldViolation)
    match {
      case (item1,item2) => {
        if (verbose >= 2) println("JumpNeighborhood: swapping bins of " + item1 + " and " + item2)
        return SwapMove(item1.bin, item2.bin, 0, "JumpNeighborhood")
      }
      case null => {
        if (verbose >= 2) println("JumpNeighborhood: no move found")
        NoMoveFound
      }
    }
  }
}


