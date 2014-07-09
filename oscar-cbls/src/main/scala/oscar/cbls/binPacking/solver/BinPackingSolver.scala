/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

package oscar.cbls.binPacking.solver

//TODO: tabu

import oscar.cbls.search.core.{StatelessNeighborhood, NoMoveFound, SearchResult}
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.binPacking.model.{BinPackingProblem, Bin, Item}
import oscar.cbls.search.move.{CompositeMove, SwapMove, AssignMove}
import scala.collection.immutable.SortedSet

/**
 * this is a standard solver for a binPacking. 
 * it performs a combination of MoveItem, Swaps, randomSwaps and binEmptying
 * @author renaud.delandtsheer@cetic.be 
 */
object BinPackingSolver extends SearchEngineTrait {
  def solveBinPacking(p: BinPackingProblem, maxStep: Int){

    val x = ((MoveItem(p) exhaustBack SwapItems(p))
              orElse (JumpSwapItems(p) maxMoves 3)
              orElse EmptyMostViolatedBin(p)) protectBest p.overallViolation.objective

    x.doAllImprovingMoves(_ >= maxStep || p.overallViolation.value == 0)
    x.restoreBest()
  }
}

/**
 * a generic algorithm for aggregating identical stuff
 * @author renaud.delandtsheer@cetic.be
 * */
object identicalAggregator{

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

  /**
   * @param l a list of items such that we want to discard items of identical class
   * @param itemClass a function that gives a class for a given item.
   *                  Class Int.MinValue is considered as different from itself
   * @tparam T
   * @return a maximal subset of l such that
   *         all items are of different class according to itemClass (with Int.MinValue exception)
   */
  def removeIdenticalClasses[T](l:List[T], itemClass:T => Int):List[T] = {
    val a: Set[Int] = SortedSet.empty
    removeIdenticalClasses[T](l, itemClass, Nil, a)
  }

  private def removeIdenticalClasses[T](l:List[T],
                                        itemClass:T => Int,
                                        canonicals:List[T],
                                        classes:Set[Int]):List[T] = {
    l match{
      case Nil => canonicals
      case h :: t =>
        val classOfH:Int = itemClass(h)
        if(classOfH != Int.MinValue && classes.contains(classOfH))
          removeIdenticalClasses(t, itemClass, canonicals,classes)
        else removeIdenticalClasses(t, itemClass, h::canonicals, classes+classOfH)
    }
  }
}

/** moves one item away from most violated bin
 * @param p the problem
 * @param best true: the best move is returned, false: the first move is returned tie breaks are both random
 * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing.
  *                          supposed to be an equivalence relation.
 *                          Identical items must be of the same size, but this does not need to be tested,
  *                          since an internal pre-filter performs this.
  *                          by default, we consider that items of the same size are identical
 * @param areBinsIdentical only one of identical bins will be considered for moves; this speeds up things.
  *                         Supposed to be an equivalence relation.
  *                         items of different sizes will be considered as different by the algorithms
  *                         through an additional mechanism, so this does not need to be tested.
  *                         by default, we consider that bins with identical free spaces are identical
  * @author renaud.delandtsheer@cetic.be
  * */
case class MoveItem(p:BinPackingProblem,
                    best:Boolean = false,
                    areItemsIdentical: (Item,Item) => Boolean = null,
                    areBinsIdentical: (Bin,Bin) => Boolean = null)
  extends StatelessNeighborhood with SearchEngineTrait{

  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):SearchResult = {
    require(!p.mostViolatedBins.value.isEmpty)

    val oldViolation:Int = p.overallViolation.objective.value
    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemMove: problem seems to be solved")
      return NoMoveFound
    }

    val itemOfBin1 = bin1.items.value.toList.map(p.items(_))

    val itemsOfBin1GroupedBySize = itemOfBin1.groupBy(_.size).values
    val itemsOfBin1Canonical:Iterable[Item] = if(areItemsIdentical == null) itemsOfBin1GroupedBySize.map(l => l.head)
    else itemsOfBin1GroupedBySize.map(l => identicalAggregator.removeIdenticals(l,areItemsIdentical)).flatten

    val binsNotBin1GroupedBySpareSize = binList
      .filter(bin => bin.number != bin1.number && bin.violation.value == 0)
      .groupBy(bin => bin.size - bin.content.value).values
    val binsNotBin1Canonical:Iterable[Bin] = if(areBinsIdentical == null) binsNotBin1GroupedBySpareSize.map(l => l.head)
    else binsNotBin1GroupedBySpareSize.map(l => identicalAggregator.removeIdenticals(l,areBinsIdentical)).flatten

    (if (best)
      selectMin2(itemsOfBin1Canonical,
        binsNotBin1Canonical,
        (item:Item,bin:Bin) => p.overallViolation.assignVal(item.bin, bin.number))
    else
      selectFirst2(itemsOfBin1Canonical.toList.sortBy(item => -item.size),
        binsNotBin1Canonical,
        (item:Item,bin:Bin) => p.overallViolation.assignVal(item.bin, bin.number) < oldViolation))
    match{
      case (item, newBin) =>
        val objAfter = p.overallViolation.assignVal(item.bin, newBin.number)
        if(acceptanceCriteria(oldViolation,objAfter)) AssignMove(item.bin,newBin.number,objAfter, "ItemMove")
        else{
          if (verbose >= 2) println("ItemMove: no improvement found")
          NoMoveFound
        }
      case null =>
        if (verbose >= 2) println("ItemMove: no improvement found")
        NoMoveFound
    }
  }
}

/**swaps items of different sizes, one of them being in one of the mostViolated bins.
  * the first item is taken from the mode violated bin.
  *
  * @param p the problem
  * @param best true if the best swap is seeked, false then the first improving move is enough
  * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing. supposed to be an equivalence relation.
  *                          Identical items must be of the same size, but this does not need to be tested, since an internal pre-filter performs this.
  *                          by default, we consider that items of the same size are identical
  * @author renaud.delandtsheer@cetic.be
  * */
case class SwapItems(p:BinPackingProblem,
                     best:Boolean = false,
                     areItemsIdentical: (Item,Item) => Boolean = null)
  extends StatelessNeighborhood with SearchEngineTrait{

  val itemList:List[Item] = p.items.toList.map(_._2)
  val binList:List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {
    require(!p.mostViolatedBins.value.isEmpty)

    val oldViolation:Int = p.overallViolation.objective.value
    val bin1 = p.bins(selectFrom(p.mostViolatedBins.value))

    if(bin1.violation.value == 0){
      if (verbose >= 2) println("ItemsSwapNeighborhood: problem seems to be solved")
      return NoMoveFound
    }

    val itemOfBin1 = bin1.items.value.toList.map(p.items(_))

    val itemsOfBin1GroupedBySize = itemOfBin1.groupBy(_.size).values
    val itemsOfBin1Canonical:Iterable[Item] = if(areItemsIdentical == null){
      itemsOfBin1GroupedBySize.map(l => l.head)
    }else{
      itemsOfBin1GroupedBySize.map(l => identicalAggregator.removeIdenticals(l,areItemsIdentical)).flatten
    }

    //TODO: this should be made lazy in case we go for the first improving move
    val itemsGroupedByBins = binList.filter(_.number != bin1.number).map(bin => bin.items.value.toList.map(p.items(_)))
    val itemsGroupedByBinsAndBySize = itemsGroupedByBins.map(_.groupBy(_.size).values)
    val itemsGroupedByBinsAndCanonicals = itemsGroupedByBinsAndBySize.map(itemsOfSameBinGroupedBySize =>
      if(areItemsIdentical == null){
        itemsOfSameBinGroupedBySize.map(l => l.head)
      }else{
        itemsOfSameBinGroupedBySize.map(l => identicalAggregator.removeIdenticals(l,areItemsIdentical)).flatten
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
      case (item1, item2) =>
        val newObj = p.overallViolation.swapVal(item1.bin, item2.bin)
        if(acceptanceCriteria(oldViolation,newObj)) SwapMove(item1.bin, item2.bin, newObj, "ItemsSwap")
        else{
          if (verbose >= 2) println("ItemsSwap: no improvement found")
          NoMoveFound
        }
      case null =>
        if (verbose >= 2) println("ItemsSwap: no improvement found")
        NoMoveFound
    }
  }
}


/** this move performs a random move in the search space.
  * it is a swap of two items of different sizes, from different bins
  * @author renaud.delandtsheer@cetic.be
  * */
case class JumpSwapItems(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait {

  val itemList: List[Item] = p.items.toList.map(_._2)
  val binList: List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = null): SearchResult = {

    val bin1:Bin = selectMax(binList, (bin:Bin) => bin.violation.value, (bin:Bin) => bin.violation.value > 0)

    if (bin1 == null) {
      if (verbose >= 2) println("Jump: problem seems to be solved")
      return NoMoveFound
    }

    selectFrom2[Item,Item](bin1.items.value.map(p.items(_)),
      itemList,
      (item1,item2) =>  {
        item2.bin.value != item1.bin.value &&
        p.bins(item1.bin.value).violation != p.bins(item2.bin.value).violation &&
        item1.size != item2.size
      })
    match {
      case (item1,item2) =>
        if (verbose >= 2) println("Jump: swapping bins of " + item1 + " and " + item2)
        SwapMove(item1.bin, item2.bin, 0, "Jump")
      case null =>
        if (verbose >= 2) println("Jump: no move found")
        NoMoveFound
    }
  }
}

/** this move performs a random move in the search space.
  * it is a swap of two items of different sizes, from different bins
  * @author renaud.delandtsheer@cetic.be
  */
case class EmptyMostViolatedBin(p:BinPackingProblem)
  extends StatelessNeighborhood with SearchEngineTrait {

  val itemList: List[Item] = p.items.toList.map(_._2)
  val binList: List[Bin] = p.bins.toList.map(_._2)

  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = null): SearchResult = {

    val bin1:Bin = selectMax(binList, (bin:Bin) => bin.violation.value, (bin:Bin) => bin.violation.value > 0)

    if (bin1 == null) {
      if (verbose >= 2) println("EmptyMostViolatedBin: problem seems to be solved")
      return NoMoveFound
    }

    CompositeMove(
      bin1.items.value.toList.map(itemid => {
        val item = p.items(itemid)
        val newBin = selectFrom(binList, (bin:Bin) => bin.number != bin1.number)
        AssignMove(item.bin,newBin.number,0)
      }), 0, "Jump, Emptying bin " + bin1.number
    )
  }
}


