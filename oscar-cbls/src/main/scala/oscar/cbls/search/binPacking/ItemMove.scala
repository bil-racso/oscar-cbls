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
import oscar.cbls.search.moves.{AssingMove, Neighborhood, Move}

/**moves item away from most violated bin*/
case class ItemMoveNeighborhood(p:BinPackingProblem)
  extends Neighborhood with SearchEngineTrait{

  val binList:List[Bin] =p.bins.toList.map(_._2)

  override def getImprovingMove(): Option[Move] = {
    val oldViolation:Int = p.overallViolation.Objective.value
    if(p.mostViolatedBins.value.isEmpty) return None
    val bin1 = p.bins(selectFirst(p.mostViolatedBins.value))
    for(itemId <- bin1.items.value; item = p.items(itemId)){
      for(bin2 <- p.bins if bin2._2.number != bin1.number && bin2._2.violation.value == 0){
        val objAfter = p.overallViolation.assignVal(item.bin, bin2._1)
        if(objAfter < oldViolation){
          Some(AssingMove(item.bin,bin2._1,objAfter))
        }
      }
    }
    None
  }
}
