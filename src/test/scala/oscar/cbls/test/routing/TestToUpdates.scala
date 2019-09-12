package oscar.cbls.test.routing

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

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing._

/**
 * Created by rdl on 03-01-17.
 */
object TestToUpdates extends App{

  val m = new Store(checker = Some(new ErrorChecker()))

  val n = 15
  val v = 2
  val startingValue:Array[IntValue] = Array(CBLSIntConst(0),CBLSIntConst(1))

  val route = new CBLSSeqVar(m,
    initialValue = IntSequence(0 until v),
    maxVal = n-1,
    n = "route")

  //op: (fromNode,toNode,contentAtFromNode)=> contentAtToNode
  def op(fromNode:Int,toNode:Int,contentAtFromNode:Int) = if(toNode%2 == 0) fromNode + toNode + contentAtFromNode else toNode

  val (contentAtNode,contentAtEnd,lastPointOfVehicle,inv) = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,startingValue,-1,fullDebug=true)

  m.close()

  route.insertAtPosition(2,1)
  route.insertAtPosition(3,2)
  route.insertAtPosition(4,3)
  route.insertAtPosition(5,4)
  route.insertAtPosition(6,5)
  route.insertAtPosition(7,6)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))
  println(inv)
  println()

route.move(3,5,6,true)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))
  println(inv)
  println()

  /*

  route.move(1,4,5,true)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))


  route.move(2,4,0,true)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))

  route.move(1,2,4,false)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))
*/
}
