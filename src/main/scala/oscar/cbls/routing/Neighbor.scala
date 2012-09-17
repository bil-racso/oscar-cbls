package oscar.cbls.routing

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

abstract class Neighbor{
  def getObjAfter:Int
  def comit
}

trait BasicMoves{
  /**
   * this flips the segment of route from "from" to "to"
   * they are supposed to be related to each other
   */
  def flipSegment(BeforeSegmentStart:Int, SegmentEnd:Int, vrp:VRP){
    var nodestack:List[Int] = List.empty
    //register the list of nodes
    var current:Int = BeforeSegmentStart
    while(current != SegmentEnd) nodestack = current :: nodestack
    while(!nodestack.isEmpty){
      vrp.Next(current) := nodestack.head
      current = nodestack.head
      nodestack = nodestack.tail
    }
  }

  def moveSegment(BeforeSegmentStart:Int, SegmentEnd:Int,  InsertionPoint:Int, vrp:VRP){
    val SegmentStart:Int = vrp.Next(BeforeSegmentStart).value
    val oldNextOfSegmentEnd:Int = vrp.Next(SegmentEnd).value
    val oldNextOfInsertionPoint:Int = vrp.Next(InsertionPoint).value

    vrp.Next(BeforeSegmentStart) := oldNextOfSegmentEnd
    vrp.Next(SegmentEnd) := oldNextOfInsertionPoint
    vrp.Next(InsertionPoint) := SegmentStart
  }
}

