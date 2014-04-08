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
package oscar.cbls.search.binPacking

import oscar.cbls.invariants.core.computation.CBLSSetVar
import oscar.cbls.search._
import oscar.cbls.objective.Objective
import scala.Some
import oscar.cbls.search.moves.{SwapMove, Neighborhood, Move}

/**swaps items of different sizes out of most violated bin*/
case class SwapItemsNeighborhood(p:BinPackingProblem)
  extends Neighborhood with SearchEngineTrait{

  val binList:List[Bin] = p.bins.toList.map(_._2)
  val itemList:List[Item] = p.items.toList.map(_._2)

  override def getImprovingMove(): Option[Move] = {
    val oldViolation:Int = p.overallViolation.Objective.value
    if(p.mostViolatedBins.value.isEmpty) return None
    val bin1 = p.bins(selectFirst(p.mostViolatedBins.value))
    for(itemId <- bin1.items.value; item1 = p.items(itemId)){
      for(item2 <- itemList if item2.bin.value != bin1.number && item1.size != item2.size){
        val objAfter = p.overallViolation.swapVal(item1.bin, item2.bin)
        if(objAfter < oldViolation){
          return Some(SwapMove(item1.bin,item2.bin,objAfter))
        }
      }
    }
    None
  }
}

