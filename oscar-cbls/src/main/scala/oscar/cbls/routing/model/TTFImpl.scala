/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Renaud De Landtsheer
  ******************************************************************************/
package oscar.cbls.routing.model

/** Stores a square matrix of primitive TTF
  *
  * @param nodeCount the number of nodes to consider; continuous ranges starting at zero
  * @param defaultTTF if we do not specify a TTF for a node, this is the value considered
  * @author Renaud De Landtsheer
  */
class TTFMatrix(nodeCount:Int,defaultTTF:PrimitiveTravelTimeFunction) extends TravelTimeFunction{

  private val matrix:Array[Array[PrimitiveTravelTimeFunction]] = Array.fill(nodeCount,nodeCount)(defaultTTF)

  def setTTF(from:Int,to:Int,ttf:PrimitiveTravelTimeFunction){
    matrix(from)(to) = ttf
  }

  def getTTF(from:Int,to:Int,ttf:PrimitiveTravelTimeFunction):PrimitiveTravelTimeFunction =
    matrix(from)(to)

  override def getTravelDuration(from: Int, leaveTime: Int, to: Int): Int =
    matrix(from)(to).getTravelDuration(leaveTime)

  override def getMinTravelDuration(from: Int, to: Int): Int =
    matrix(from)(to).getMinTravelDuration

  override def getMaxTravelDuration(from: Int, to: Int): Int =
    matrix(from)(to).getMaxTravelDuration

  override def getMinMaxTravelDuration(from: Int, to: Int): (Int, Int) =
    matrix(from)(to).getMinMaxTravelDuration
}

/** This stores a single TTF of a travel binding two nodes
  * @author Renaud De Landtsheer
  * */
abstract class PrimitiveTravelTimeFunction {

  /** the duration to perform the travel at leave time "éleaveTime
    *
    * @param leaveTime when the travel begins
    * @return when the travel ends
    */
  def getTravelDuration(leaveTime: Int): Int

  def getMinMaxTravelDuration: (Int, Int) =
    (getMinTravelDuration, getMaxTravelDuration)

  def getMinTravelDuration: Int
  def getMaxTravelDuration: Int

}

/** A TTF that is constant
  * this is similar to using a TTFHistogram with a single slot, but this class is lighter
  * @param travelDuration the duration of the travel
  * @author Renaud De Landtsheer
  **/
class TTFConst(travelDuration:Int) extends PrimitiveTravelTimeFunction {
  override def getTravelDuration(leaveTime: Int): Int = travelDuration

  override def getMinTravelDuration: Int = travelDuration

  override def getMaxTravelDuration: Int = travelDuration
}

/**
 * represents a TTF using histograms
 * Notice that the representation is modulo, if asked for a time after the overallDuration,
 * it is assumed to start again at position zero in time
 *
 * @param NbSlots the number of slots in the histogram
 * @param overallDuration the duration of the whole TTF
 * @author Renaud De Landtsheer
 * */
class TTFHistogram(val NbSlots:Int, val overallDuration:Int) extends PrimitiveTravelTimeFunction {
  private val slotDuration:Int = overallDuration / NbSlots

  private val slots:Array[Int] = Array.fill(NbSlots)(0)
  private var nimMaxAccurate =  false
  private var min:Int = 0
  private var max:Int = 0

  def setTravelDurationAtSlot(slotNumber:Int, duration:Int){
    slots(slotNumber) = duration
    nimMaxAccurate = false
  }
  def getTravelDurationAtSlot(slotNumber:Int):Int = slots(slotNumber)

  private def updateMinMax(){
    if(!nimMaxAccurate){
      nimMaxAccurate = true
      min = Int.MaxValue
      max = Int.MinValue
      for(v <- slots){
        if(v < min) min = v
        if(max < v) max = v
      }
    }
  }

  private def rectifySlot(slotNr:Int):Int = {
    val tmp:Int = slotNr % NbSlots
    if (tmp < 0) return tmp + NbSlots
    return tmp
  }

  override def getTravelDuration(leaveTime: Int): Int =
    slots(rectifySlot(leaveTime / slotDuration))

  override def getMinTravelDuration: Int = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Int = {
    updateMinMax()
    max
  }
}

/**
 * Represents a TTF as a piecewise linear function
 *
 * Notice that the representation is modulo, if asked for a time after the last point,
 * it is assumed to start again at position zero in time,
 * so that linear interpolation might happen between the last point and the first point, shifted by overallDuration
 *
 * @param NbPoints the number of points to consider
 * @param overallDuration the duration to consider
 * @author Renaud De Landtsheer
 * */
class TTFSegments(val NbPoints:Int, val overallDuration:Int) extends PrimitiveTravelTimeFunction {

  private val pointX:Array[Int] = Array.fill(NbPoints)(0)
  private val pointY:Array[Int] = Array.fill(NbPoints)(0)

  private var nimMaxAccurate =  false
  private var min:Int = 0
  private var max:Int = 0

  /**throws an error if the X is smaller than the predecessor's X, or if the slope is too steep*/
  def setPoint(pointNr:Int, pointX:Int, pointY:Int){
    this.pointX(pointNr) = pointX
    this.pointY(pointNr) = pointY
    nimMaxAccurate = false
    if(pointNr != 0){

      val firstId = pointNr - 1
      val secondId = pointNr

      val intervalX = this.pointX(secondId) - this.pointX(firstId)
      if(intervalX <= 0) throw new Error("TTF segments are going backward in time")

      val intervalY =  this.pointY(secondId) - this.pointY(firstId)
      if(intervalX < - intervalY) throw new Error("slope is too steep")
    }
  }

  def getPointX(pointNr:Int):Int = pointX(pointNr)
  def getPointY(pointNr:Int):Int = pointY(pointNr)

  private def updateMinMax(){
    if(!nimMaxAccurate){
      nimMaxAccurate = true
      min = Int.MaxValue
      max = Int.MinValue
      for(v <- pointY){
        if(v < min) min = v
        if(max < v) max = v
      }
    }
  }

  private def findLastPointBefore(x:Int): Int = {
    var up:Int = NbPoints - 1
    var down:Int = -1
    while (down + 1 < up) {
      val mid:Int = (up + down) / 2
      if (pointX(mid) == x) {
        return mid
      } else if (pointX(mid) < x) {
        down = mid
      } else {
        up = mid
      }
    }
    if (pointX(up) <= x) up
    else down
  }

  override def getTravelDuration(leaveTime: Int): Int = {
    val pointBefore = findLastPointBefore(leaveTime)
    if(pointBefore == NbPoints){
      linearInterpol(leaveTime,
        pointX(pointBefore), pointY(pointBefore),
        pointX(0) + overallDuration,  pointY(0))
    }else{
      val pointAfter = pointBefore + 1
      linearInterpol(leaveTime,
        pointX(pointBefore), pointY(pointBefore),
        pointX(pointAfter),  pointY(pointAfter))
    }
  }

  @inline
  private def linearInterpol(X:Int, X1:Int, Y1:Int, X2:Int, Y2:Int):Int = {
    (X - X1) * (Y2 - Y1) / (X2 - X1) + Y1
  }

  override def getMinTravelDuration: Int = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Int = {
    updateMinMax()
    max
  }
}
