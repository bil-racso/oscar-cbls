package oscar.cbls.binPacking.solver

//TODO: détection de dominance
//TODO: tabu

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.search.moves._
import oscar.cbls.search.moves.AssingMove
import oscar.cbls.binPacking.model.{BinPackingProblem, Bin, Item}

object BinPackingSolver extends SearchEngineTrait {
  def solveBinPacking(p: BinPackingProblem, maxStep: Int){

    val x = ((MoveItem(p) exhaustBack SwapItems(p))
              orElse (JumpSwapItems(p) maxMoves 3)
              orElse EmptyMostViolatedBin(p)) protectBest(p.overallViolation.objective)


    x.doAllImprovingMoves(maxStep)
    x.restoreBest()
  }
}

object identicalMerger{
  def removeIdenticals[T](l:List[T], isIdentical:(T,T) => Boolean):List[T] =
    removeIdenticals[T](l, isIdentical, Nil)

  private def removeIdenticals[T](l:List[T], isIdentical:(T,T) => Boolean, canonicals:List[T]):List[T] = {
    l match{
      case Nil => canonicals
      case h :: t =>
        if(canonicals.exists(c => isIdentical(c,h)))
          removeIdenticals(t, isIdentical, canonicals)
        else removeIdenticals(t, isIdentical, h::canonicals)
    }
  }
}


/** moves one item away from most violated bin
 * @param p the problem
 * @param best true: the best move is returned, false: the first move is returned tie breaks are both random
 * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing. supposed to be an equivalence relation.
 *                          Identical items must be of the same size, but this does not need to be tested, since an internal pre-filter performs this.
  *                          by default, we consider that items of the same size are identical
 * @param areBinsIdentical only one of identical bins will be considered for moves; this speeds up things. Supposed to be an equivalence relation.
  *                         items of different sizes will be considered as different by the algorithms through an additional mechanism, so this does not need to be tested.
  *                         by default, we consider that bins with identical free spaces are identical
 */
case class MoveItem(p:BinPackingProblem,
                    best:Boolean = false,
                    areItemsIdentical: (Item,Item) => Boolean = null,
                    areBinsIdentical: (Bin,Bin) => Boolean = null)
  extends StatelessNeighborhood with SearchEngineTrait{

  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove():SearchResult = {

    val oldViolation:Int = p.overallViolation.objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose >= 2) println("ItemMove: problem is solved")
      return ProblemSolved
    }
    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemMove: problem is solved")
      return ProblemSolved
    }

    val itemOfBin1 = bin1.items.value.toList.map(p.items(_))

    val itemsOfBin1GroupedBySize = itemOfBin1.groupBy(_.size).values
    val itemsOfBin1Canonical:Iterable[Item] = if(areItemsIdentical == null) itemsOfBin1GroupedBySize.map(l => l.head)
    else itemsOfBin1GroupedBySize.map(l => identicalMerger.removeIdenticals(l,areItemsIdentical)).flatten

    val binsNotBin1GroupedBySpareSize = binList
      .filter(bin => bin.number != bin1.number && bin.violation.value == 0)
      .groupBy(bin => bin.size - bin.content.value).values
    val binsNotBin1Canonical:Iterable[Bin] = if(areBinsIdentical == null) binsNotBin1GroupedBySpareSize.map(l => l.head)
    else binsNotBin1GroupedBySpareSize.map(l => identicalMerger.removeIdenticals(l,areBinsIdentical)).flatten

    (if (best)
      selectMin2(itemsOfBin1Canonical,
        binsNotBin1Canonical,
        (item:Item,bin:Bin) => p.overallViolation.assignVal(item.bin, bin.number))
    else
      selectFirst2(itemsOfBin1Canonical.toList.sortBy(item => -(item.size)),
        binsNotBin1Canonical,
        (item:Item,bin:Bin) => p.overallViolation.assignVal(item.bin, bin.number) < oldViolation))
    match{
      case (item, newBin) => {
        val objAfter = p.overallViolation.assignVal(item.bin, newBin.number)
        if(objAfter < oldViolation) AssingMove(item.bin,newBin.number,objAfter, "ItemMove")
        else{
          if (verbose >= 2) println("ItemMove: no improvement found")
          NoMoveFound
        }
      }
      case null => {
        if (verbose >= 2) println("ItemMove: no improvement found")
        NoMoveFound
      }
    }
  }
}

/**swaps items of different sizes, one of them being in one of the mostViolated bins.
  * the first item is taken from the mode violated bin.
  *
  * @param p
  * @param best true if the best swap is seeked, false then the first improving move is enough
  * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing. supposed to be an equivalence relation.
  *                          Identical items must be of the same size, but this does not need to be tested, since an internal pre-filter performs this.
  *                          by default, we consider that items of the same size are identical
 */
case class SwapItems(p:BinPackingProblem,
                     best:Boolean = false,
                     areItemsIdentical: (Item,Item) => Boolean = null)
  extends StatelessNeighborhood with SearchEngineTrait{

  val itemList:List[Item] = p.items.toList.map(_._2)
  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(): SearchResult = {
    val oldViolation:Int = p.overallViolation.objective.value

    if(p.mostViolatedBins.value.isEmpty){
      if (verbose >= 2) println("ItemsSwap: problem is solved")
      return ProblemSolved
    }

    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemsSwapNeighborhood: problem is solved")
      return ProblemSolved
    }

    val itemOfBin1 = bin1.items.value.toList.map(p.items(_))

    val itemsOfBin1GroupedBySize = itemOfBin1.groupBy(_.size).values
    val itemsOfBin1Canonical:Iterable[Item] = if(areItemsIdentical == null){
      itemsOfBin1GroupedBySize.map(l => l.head)
    }else{
      itemsOfBin1GroupedBySize.map(l => identicalMerger.removeIdenticals(l,areItemsIdentical)).flatten
    }

    //TODO: this should be made lazy in case we go for the first improving move
    val itemsGroupedByBins = binList.filter(_.number != bin1.number).map(bin => bin.items.value.toList.map(p.items(_)))
    val itemsGroupedByBinsAndBySize = itemsGroupedByBins.map(_.groupBy(_.size).values)
    val itemsGroupedByBinsAndCanonicals = itemsGroupedByBinsAndBySize.map(itemsOfSameBinGroupedBySize =>
      if(areItemsIdentical == null){
        itemsOfSameBinGroupedBySize.map(l => l.head)
      }else{
        itemsOfSameBinGroupedBySize.map(l => identicalMerger.removeIdenticals(l,areItemsIdentical)).flatten
      })

    val itemsNotOfBin1Canonical:Iterable[Item] = itemsGroupedByBinsAndCanonicals.flatten

    (if (best)
      selectMin2(itemsOfBin1Canonical, itemsNotOfBin1Canonical,
        (item1:Item,item2:Item) => p.overallViolation.swapVal(item1.bin, item2.bin),
        (item1:Item,item2:Item) => item1.size != item2.size )
    else selectFirst2(itemsOfBin1Canonical, itemsNotOfBin1Canonical,
      (item1:Item,item2:Item) => item1.size != item2.size
        && p.overallViolation.swapVal(item1.bin, item2.bin) < oldViolation))
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


