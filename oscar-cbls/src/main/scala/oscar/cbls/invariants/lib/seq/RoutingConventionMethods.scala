package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence

/**
 * Created by rdl on 11-05-16.
 */
object RoutingConventionMethods {

  def searchVehicleReachingPosition(position:Int, seq:UniqueIntSequence, v:Int):Int = {
    var upperVehicle = v-1
    var upperVehiclePosition = seq.positionOfValue(upperVehicle).head

    if(position > upperVehiclePosition) return upperVehicle

    var lowerVehicle = 0
    var lowerVehiclePosition = 0

    assert(seq.positionOfValue(lowerVehicle).head == 0)
    require(lowerVehiclePosition <= upperVehiclePosition)

    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = seq.positionOfValue(midVehicle).head
      if(midVehiclePosition == position) return midVehicle
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

  def routingSuccVal2Val(value:Int, seq:UniqueIntSequence, v:Int):Int =
    routingSuccPos2Val(seq.positionOfValue(value).head, seq, v)

  def routingSuccPos2Val(position:Int, seq:UniqueIntSequence, v:Int):Int = {
    seq.valueAtPosition(position + 1) match{
      case None => v-1
      case Some(succToIfNoLoop ) => if (succToIfNoLoop < v) succToIfNoLoop-1 else succToIfNoLoop
    }
  }

  def routingPredVal2Val(value:Int, seq:UniqueIntSequence, v:Int):Int = {
    if(value < v) {
      //looking for the end node of vehicle value
        if(value == v-1) {
          //it is the last vehicle
          seq.valueAtPosition(seq.size-1).head
        }else {
          //there is oe vehicle above
          seq.valueAtPosition(seq.positionOfValue(value+1).head-1).head
        }
    }else {
      //simple predecessor
      seq.valueAtPosition(seq.positionOfValue(value).head-1).head
    }
  }


  def routingPredPos2Val(position:Int, seq:UniqueIntSequence, v:Int):Int = {
    routingPredVal2Val(seq.valueAtPosition(position).head, seq, v)
  }
}
