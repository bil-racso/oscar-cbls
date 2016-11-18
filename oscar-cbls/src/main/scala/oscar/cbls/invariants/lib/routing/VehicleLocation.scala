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

abstract class VehicleLocation(_v : Int, _depth:Int){
  val v = _v
  val depth = _depth
  def regularize():ConcreteVehicleLocation =  ConcreteVehicleLocation(Array.tabulate(v)(((car:Int)=> posOfVehicle(car))))

  def push(oldToNewfunction:(Int)=> Option[Int]):VehicleLocation

  def posOfVehicle( vehicle:Int ):Int

  def vehicleReachingPosition(position:Int): Int ={
    var upperVehicle:Int = v -1
    var upperVehiclePosition = posOfVehicle(upperVehicle)
    if(position>=upperVehiclePosition) return upperVehicle
    var lowerVehicle = 0
    var lowerVehiclePosition = 0
    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = posOfVehicle(midVehicle)
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




object ConcreteVehicleLocation{
  def apply(value: Array[Int]): ConcreteVehicleLocation = new ConcreteVehicleLocation(value)
  def unapply(arg: ConcreteVehicleLocation): Option[Array[Int]] =Some(arg.startPositionOfVehicle)
}

class ConcreteVehicleLocation(value:Array[Int]) extends VehicleLocation(value.length,0){

  private val startPositionOfVehicle : Array[Int]= Array.tabulate(v)(((car:Int)=> value(car)))

  def apply(vehicle:Int): Int ={
    assert(vehicle < v && vehicle>=0)
    startPositionOfVehicle(vehicle)
  }

  override def regularize(): ConcreteVehicleLocation = this
  override def push(oldToNewfunction:(Int)=> Option[Int]): VehicleLocation = StackedVehicleLocation(oldToNewfunction,this)
  override def posOfVehicle(vehicle: Int): Int = startPositionOfVehicle(vehicle)

  override def toString: String = "concrete "+startPositionOfVehicle.mkString(",")
}

object StackedVehicleLocation{
  def apply(function:(Int)=> Option[Int], prev: VehicleLocation): StackedVehicleLocation = new StackedVehicleLocation( function, prev)
  def unapply(arg: StackedVehicleLocation): Option[( (Int=>Option[Int]),VehicleLocation)] = Some( arg.function,arg.prev)
}

class StackedVehicleLocation( val function:Int=> Option[Int], val prev: VehicleLocation) extends VehicleLocation(prev.v,prev.depth+1){

  override def push(oldToNewfunction:(Int)=> Option[Int]): VehicleLocation = StackedVehicleLocation(oldToNewfunction,this)
  override def posOfVehicle(vehicle: Int): Int = function(prev.posOfVehicle(vehicle)).get

  override def toString: String = " stacked ( depth = "+depth+" prev : "+prev+")"
}







/*
object ConcreteVehicleLocation{
  def apply(value: Array[Int]): ConcreteVehicleLocation = new ConcreteVehicleLocation(value)
  def unapply(arg: ConcreteVehicleLocation): Option[Array[Int]] =Some(arg.startPositionOfVehicle)
}

class ConcreteVehicleLocation(value:Array[Int]) extends VehicleLocation(value.length){

  private val startPositionOfVehicle : Array[Int]= Array.tabulate(v)(((car:Int)=> value(car)))
  def apply(vehicle:Int): Int ={
    assert(vehicle < v && vehicle>=0)
    startPositionOfVehicle(vehicle)
  }

  override def regularize(): ConcreteVehicleLocation = this
  override def push(function:(Int)=> Option[Int]): VehicleLocation = StackedVehicleLocation((posit)=>function(posOfVehicle(posit)).get,this)
  override def posOfVehicle(vehicle: Int): Int = startPositionOfVehicle(vehicle)

  override def toString: String = "concrete "+startPositionOfVehicle.mkString(",")
}


object StackedVehicleLocation{
  def apply(function:(Int)=> Int, origin: VehicleLocation): StackedVehicleLocation = new StackedVehicleLocation( function, origin)
  def unapply(arg: StackedVehicleLocation): Option[( (Int=>Int),VehicleLocation)] = Some( arg.function,arg.origin)
}

class StackedVehicleLocation( val function:Int=> Int, val origin: VehicleLocation) extends VehicleLocation(origin.v){
  override def push(oldToNewfunction:(Int)=> Option[Int]): VehicleLocation = StackedVehicleLocation((pos) =>  oldToNewfunction(posOfVehicle(pos)).get,origin)
  override def posOfVehicle(vehicle: Int): Int = function(vehicle)

  override def toString: String = " stacked ( origin : "+origin+")"
}
*/
