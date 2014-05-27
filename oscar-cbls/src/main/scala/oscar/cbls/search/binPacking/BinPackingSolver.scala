package oscar.cbls.search.binPacking

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar, Store, CBLSIntConst}
import oscar.cbls.search.moves._
import oscar.cbls.search.moves.AssingMove

object BinPackingSolver extends SearchEngineTrait {
  def solveBinPacking(p: BinPackingProblem, maxStep: Int) = {

    val x = ((MoveItem(p) exhaustBack SwapItems(p)) orElse JumpSwapItems(p)) maxMoves maxStep/5 exhaustBack (JumpSwapItems(p) maxMoves 10) protectBest(p.overallViolation.Objective)
    x.doAllImprovingMoves(maxStep)
    x.restoreBest()
  }
}

/**moves item away from most violated bin*/
case class MoveItem(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait{

  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove():SearchResult = {

    val oldViolation:Int = p.overallViolation.Objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose >= 2) println("ItemMove: problem is solved")
      return ProblemSolved
    }
    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemMove: problem is solved")
      return ProblemSolved
    }

    for(itemId <- bin1.items.value; item = p.items(itemId)){
      for(bin2 <- p.bins if bin2._2.number != bin1.number && bin2._2.violation.value == 0){
        val objAfter = p.overallViolation.assignVal(item.bin, bin2._1)
        if(objAfter < oldViolation){
           if (verbose >= 2) println("ItemMove: move found: move " + item + " to bin " + bin2._2 + " objAfter " + objAfter )
          return AssingMove(item.bin,bin2._1,objAfter, "ItemMove")
        }
      }
    }
    if (verbose >= 2) println("ItemMove: no improvement found")
    NoMoveFound
  }
}

/**swaps items of different sizes, one of them being in one of the mostViolated bins.
  * the first item is taken from the mode violated bin.
 *
 * @param p
 * @param best true if the best swap is seeked, false then the first improving move is enough
 */
case class SwapItems(p:BinPackingProblem, best:Boolean = false)
  extends StatelessNeighborhood with SearchEngineTrait{

  val itemList:List[Item] = p.items.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.Objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose >= 2) println("ItemsSwap: problem is solved")
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
        if(newObj < oldViolation) SwapMove(item1.bin, item2.bin, newObj, "ItemsSwap")
        else{
          if (verbose >= 2) println("ItemsSwap: no improvement found")
          NoMoveFound
        }
      }
      case null => {
        if (verbose >= 2) println("ItemsSwap: no improvement found")
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

    val bin1:Bin = selectMax(binList, (bin:Bin) => bin.violation.value, (bin:Bin) => bin.violation.value > 0)

    if (bin1 == null) {
      if (verbose >= 2) println("Jump: problem is solved")
      return ProblemSolved
    }

    selectFrom2[Item,Item](bin1.items.value.map(p.items(_)),
      itemList,
      (item1,item2) =>  {
        item2.bin.value != item1.bin.value &&
        p.bins(item1.bin.value).violation != p.bins(item2.bin.value).violation &&
        item1.size != item2.size
      })
    match {
      case (item1,item2) => {
        if (verbose >= 2) println("Jump: swapping bins of " + item1 + " and " + item2)
        return SwapMove(item1.bin, item2.bin, 0, "Jump")
      }
      case null => {
        if (verbose >= 2) println("Jump: no move found")
        NoMoveFound
      }
    }
  }
}

/** this move performs a random move in the search space.
  * it is a swap of two items of different sizes, from different bins
  */
case class EmptyMostViolatedBin(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait {

  val itemList: List[Item] = p.items.toList.map(_._2)
  val binList: List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.Objective.value

    val bin1:Bin = selectMax(binList, (bin:Bin) => bin.violation.value, (bin:Bin) => bin.violation.value > 0)

    if (bin1 == null) {
      if (verbose >= 2) println("EmptyMostViolatedBin: problem is solved")
      return ProblemSolved
    }



    CompositeMove(
      bin1.items.value.toList.map(itemid => {
        val item = p.items(itemid)
        val newBin = selectFrom(binList, (bin:Bin) => bin.number != bin1.number)
        AssingMove(item.bin,newBin.number,0)
      }), 0, "Jump, Emptying bin " + bin1.number
    )

  }
}


