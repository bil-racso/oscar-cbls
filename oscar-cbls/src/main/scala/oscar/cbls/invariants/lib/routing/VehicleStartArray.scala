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



/**
  * Created by  Jannou Brohée on 8/11/16.
  */





object VehicleStartArray{
  def apply(_numberOfVehicle:Int): VehicleStartArray = new VehicleStartArray(_numberOfVehicle)
}


/**
  * Implementation providing an array stocking the vehicle start position and methods to update the array
  * @param _numberOfVehicle the number of vehicles to consider
  */
class VehicleStartArray(_numberOfVehicle:Int) {

  private val nbrOfVehicle:Int = _numberOfVehicle
  private val startPositionOfVehicle : Array[Int]= Array.tabulate(nbrOfVehicle)(((car:Int)=> Int.MinValue))

  /**
    * Set the start position of the given vehicle
    * @param vehicle the vehicle to consider
    * @param startPosition the new start position
    */
  def update(vehicle:Int, startPosition:Int){
    assert(startPosition>=0 && startPosition <=Int.MaxValue && vehicle < nbrOfVehicle && vehicle>=0)
    startPositionOfVehicle(vehicle)=startPosition
  }

  /**
    *
    * @param vehicle
    * @return
    */
  def apply(vehicle:Int): Int ={
    assert(vehicle < nbrOfVehicle && vehicle>=0)
    startPositionOfVehicle(vehicle)
  }

  /**
    * Updates the array of vehicles start position following a insert
    * @param position the position where the insert occur
    */
  def updateForInsert(position:Int): Unit ={
    val vehicleNextVehicleReachingPosition = vehicleReachingPosition(position-1)+1
    for(pos <- vehicleNextVehicleReachingPosition until nbrOfVehicle) startPositionOfVehicle(pos)+=1
  }

  /**
    * Updates the array of vehicles start position following a delete
    * @param position the position where the delete occur
    */
  def updateForDelete(position:Int): Unit ={
    val vehicleNextVehicleReachingPosition = vehicleReachingPosition(position)+1
    for(pos <- vehicleNextVehicleReachingPosition until nbrOfVehicle) startPositionOfVehicle(pos)-=1
  }

  /**
    * Updates the array of vehicles start position following a move
    * @param fromIncluded the start position of interval to move
    * @param toIncluded the end position of interval to move
    * @param after the position after which the interval is moved
    */
  def updateForMove(fromIncluded:Int, toIncluded:Int, after:Int, delta:Int): Unit ={ // todo use intSeq to dynamically compute the delta ?
    val vehicleReachingFromIncluded = vehicleReachingPosition(fromIncluded)
    val vehicleReachingAfter = vehicleReachingPosition(after)
    val case1:Boolean = after<fromIncluded
    if (vehicleReachingFromIncluded!= vehicleReachingAfter) {
      var pos = if(case1) vehicleReachingAfter+1 else vehicleReachingFromIncluded+1
      while (pos <= (if(case1) vehicleReachingFromIncluded else vehicleReachingAfter) ) {
        startPositionOfVehicle(pos)+=delta
        pos += 1
      }
    }
  }

  /**
    * Returns the vehicle reaching the given position
    * @param position the position to reach
    * @return the vehicle reaching the position
    * @see RoutingConventionMethods.searchVehicleReachingPosition for author of implementation
    */
  def vehicleReachingPosition(position:Int):Int ={
    var upperVehicle = nbrOfVehicle-1
    var upperVehiclePosition = startPositionOfVehicle(upperVehicle)
    if(position>=upperVehiclePosition) return upperVehicle
    var lowerVehicle = 0
    var lowerVehiclePosition = 0
    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = startPositionOfVehicle(midVehicle)
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


}



