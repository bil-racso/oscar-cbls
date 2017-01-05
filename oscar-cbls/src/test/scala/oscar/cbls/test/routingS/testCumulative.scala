package oscar.cbls.test.routingS

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation.{IntValue, CBLSIntConst, CBLSSeqVar, Store}
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.routing.ForwardCumulativeIntegerDimensionOnVehicle

/**
 * Created by rdl on 03-01-17.
 */
object TestCumulative extends App{

  val m = new Store(checker = Some(new ErrorChecker()))

  val n = 10
  val v = 2
  val startingValue:Array[IntValue] = Array(CBLSIntConst(0),CBLSIntConst(1))

  val route = new CBLSSeqVar(m,
    initialValue = IntSequence(0 until v),
    maxVal = n-1,
    n = "route")

  //op: (fromNode,toNode,contentAtFromNode)=> contentAtToNode
  def op(fromNode:Int,toNode:Int,contentAtFromNode:Int) = if(toNode%2 == 0) fromNode + toNode + contentAtFromNode else toNode

  val  (contentAtNode,contentAtEnd,lastPointOfVehicle) = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,startingValue,-1)

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

  println()

  route.insertAtPosition(9,5)

  route.insertAtPosition(8,8)

  route.move(2,6,8,false)



  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))

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
