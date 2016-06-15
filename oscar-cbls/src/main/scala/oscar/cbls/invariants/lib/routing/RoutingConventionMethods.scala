package oscar.cbls.invariants.lib.routing

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

import oscar.cbls.invariants.core.algo.rb.RedBlackTreeMap
import oscar.cbls.invariants.core.algo.seq.functional.IntSequence

object RoutingConventionMethods {

  def batchVehicleReachingPosition(seq:IntSequence,v:Int):(Int=>Option[Int]) = {
    val vehiclePositionArray:Array[(Int,Int)] =
      Array.tabulate(v)(vehicle => (seq.positionOfAnyOccurrence(vehicle).head, vehicle))

    val vehiclePositionRB = RedBlackTreeMap.makeFromSorted(vehiclePositionArray)

    def findVehicleReachingPosition(position:Int):Option[Int] = {
      vehiclePositionRB.biggestLowerOrEqual(position) match{
        case None => None
        case Some((vpostion,vehicle)) => Some(vehicle)
      }
    }
    findVehicleReachingPosition
  }

  def searchVehicleReachingPosition(position:Int, seq:IntSequence, v:Int):Int = {
    var upperVehicle = v-1
    var upperVehiclePosition = seq.positionOfAnyOccurrence(upperVehicle).head

    if(position >= upperVehiclePosition) return upperVehicle

    var lowerVehicle = 0
    var lowerVehiclePosition = 0

    assert(seq.positionOfAnyOccurrence(lowerVehicle).head == 0)
    require(lowerVehiclePosition <= upperVehiclePosition)

    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = seq.positionOfAnyOccurrence(midVehicle).head
      if(midVehiclePosition == position){
        return midVehicle
      }
      if(midVehiclePosition <= position){
        lowerVehicle = midVehicle
        lowerVehiclePosition = midVehiclePosition
      }else{
        upperVehicle = midVehicle
        upperVehiclePosition = midVehiclePosition
      }
    }
    lowerVehicle
  }

  def routingSuccVal2Val(value:Int, seq:IntSequence, v:Int):Int =
    routingSuccPos2Val(seq.positionOfAnyOccurrence(value).head, seq, v)

  def routingSuccPos2Val(position:Int, seq:IntSequence, v:Int):Int = {
    seq.valueAtPosition(position + 1) match{
      case None => v-1
      case Some(succToIfNoLoop ) => if (succToIfNoLoop < v) succToIfNoLoop-1 else succToIfNoLoop
    }
  }

  def routingPredVal2Val(value:Int, seq:IntSequence, v:Int):Int = {
    if(value < v) {
      //looking for the end node of vehicle value
        if(value == v-1) {
          //it is the last vehicle
          seq.valueAtPosition(seq.size-1).head
        }else {
          //there is oe vehicle above
          seq.valueAtPosition(seq.positionOfAnyOccurrence(value+1).head-1).head
        }
    }else {
      //simple predecessor
      seq.valueAtPosition(seq.positionOfAnyOccurrence(value).head-1).head
    }
  }


  def routingPredPos2Val(position:Int, seq:IntSequence, v:Int):Int = {
    routingPredVal2Val(seq.valueAtPosition(position).head, seq, v)
  }
}
