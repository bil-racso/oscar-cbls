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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer and Florent Ghilain.
  ******************************************************************************/

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.algo.heap.BinomialHeap
import oscar.cbls.invariants.core.propagation.Checker


/**
 * This invariants maintains data structures representing a VRP and his
 * characteristics like the length of route, the position of points in route, etc.. .
 *
 * Info : the indices from 0 to V-1 (in the Next, PositionInRoute and RouteNr array) are the starting
 * points of vehicles.
 * @param V the number of vehicles.
 * @param Next the array of successors of each points (deposits and customers) of the VRP.
 * @param PositionInRoute the position in route of each points, N is the value of unrouted node.
 * @param RouteNr the route number of each points, V is the value of unrouted node.
 * @param RouteLength the length of each route.
 * @param LastInRoute the last point in each route.
 */
case class Routes(V: Int,
                  Next:Array[IntVar],
                  PositionInRoute:Array[IntVar],
                  RouteNr:Array[IntVar],
                  RouteLength:Array[IntVar],
                  LastInRoute:Array[IntVar]) extends Invariant {
  val UNROUTED = Next.length
  val ArrayOfUnregisterKeys = registerStaticAndDynamicDependencyArrayIndex(Next)
  finishInitialization()
  for(v <- PositionInRoute){v.setDefiningInvariant(this)}
  for(v <- RouteNr){v.setDefiningInvariant(this)}
  for(v <- RouteLength){v.setDefiningInvariant(this)}
  for(v <- LastInRoute){v.setDefiningInvariant(this)}

  for (v <- 0 until V) DecorateVehicleRoute(v)


  override def toString():String ={
    var toReturn:String = ""
    toReturn +="\nNext array: ["
    for (v <- Next){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn +="Position array: ["
    for (v <- PositionInRoute){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn +="RouteNr array: ["
    for (v <- RouteNr){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn +="RouteLength array: ["
    for (v <- RouteLength){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn +="LastInRoute array: ["
    for (v <- LastInRoute){toReturn += (""+v.getValue(true) +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn
  }

  def DecorateVehicleRoute(v:Int){
    var currentID = v
    var currentPosition = 1
    PositionInRoute(v) := 0
    RouteNr(v) := v
    while(Next(currentID).value !=v){

      assert(Next(currentID).value>v)

      currentID = Next(currentID).value
      PositionInRoute(currentID) := currentPosition
      RouteNr(currentID) := v
      currentPosition +=1
    }
    LastInRoute(v) := currentID
    RouteLength(v) := PositionInRoute(currentID).getValue(true)+1
  }

  var ToUpdate:List[Int] = List.empty
  var ToUpdateCount:Int = 0

  override def notifyIntChanged(v: IntVar, i: Int, OldVal: Int, NewVal: Int){
    unregisterDynamicallyListenedElement(ArrayOfUnregisterKeys(i))
    ArrayOfUnregisterKeys(i) = null
    ToUpdate = i :: ToUpdate
    ToUpdateCount +=1
    scheduleForPropagation()
  }

  @inline
  final def isUpToDate(node:Int):Boolean = {
    ((RouteNr(node).getValue(true) == RouteNr(Next(node).value).getValue(true))
      && ((PositionInRoute(node).getValue(true) + 1)% Next.length == PositionInRoute(Next(node).value).getValue(true)))
  }

  override def performPropagation(){
    //le numéro de noeud, son ancienne position dans le circuit
    val heap = new BinomialHeap[(Int,Int)]((a:(Int,Int)) => a._2, ToUpdateCount)
    for (node <- ToUpdate){
      if(Next(node).value == UNROUTED){
        //node is unrouted now
        RouteNr(node) := V
        PositionInRoute(node) := UNROUTED
        ArrayOfUnregisterKeys(node) = registerDynamicallyListenedElement(Next(node),node)
      }else if(isUpToDate(node)){
        ArrayOfUnregisterKeys(node) = registerDynamicallyListenedElement(Next(node),node)
      }else{
        heap.insert((node,PositionInRoute(node).getValue(true)))
      }
    }
    ToUpdate = List.empty
    ToUpdateCount = 0

    while(!heap.isEmpty){
      val currentNodeForUpdate = heap.popFirst()._1
      DecorateRouteStartingFromAndUntilConformOrEnd(currentNodeForUpdate)
      ArrayOfUnregisterKeys(currentNodeForUpdate)
        = registerDynamicallyListenedElement(Next(currentNodeForUpdate),currentNodeForUpdate)
    }
  }

  /**
   *
   * @param nodeID is the node whose next hjas changed
   */
  def DecorateRouteStartingFromAndUntilConformOrEnd(nodeID:Int){
    var currentNode = nodeID
    var nextNode = Next(currentNode).value
    while(!isUpToDate(currentNode) && nextNode >= V){
      PositionInRoute(nextNode) := (PositionInRoute(currentNode).getValue(true)+ 1)
      RouteNr(nextNode) := RouteNr(currentNode).getValue(true)
      currentNode = nextNode
      nextNode = Next(currentNode).value
    }
    if (nextNode<V){
      LastInRoute(nextNode) := currentNode
      RouteLength(nextNode) := PositionInRoute(currentNode).getValue(true) + 1
    }
  }

  override def checkInternals(c:Checker){
    for(n <- Next.indices){
      val next = Next(n).value
      if (next != UNROUTED){
        c.check(RouteNr(next).value == RouteNr(n).value)
        if(next < V){
          c.check(PositionInRoute(next).value == 0)
          c.check(RouteNr(next).value == next)
        }
        else{
          c.check(PositionInRoute(next).value == (PositionInRoute(n).value +1)%(RouteLength(RouteNr(n).value).value))
          c.check(RouteNr(n).value == RouteNr(next).value)
        }
      }
      else{
        c.check(RouteNr(n).value == V)
        c.check(PositionInRoute(n).value == UNROUTED)
      }
    }
  }
}
object Routes{
  def buildRoutes(Next:Array[IntVar], V:Int):Routes = {
    val m:Model = InvariantHelper.findModel(Next)

    val PositionInRoute = Array.tabulate(Next.length)(i => IntVar(m, 0, Next.length,Next.length, "PositionInRouteOfPt" + i))
    val RouteNr = Array.tabulate(Next.length)(i => IntVar(m, 0, V,V, "RouteNrOfPt" + i))
    val RouteLength = Array.tabulate(V)(i => IntVar(m,0,Next.length,0,"Route "+i+"-Lenght"))
    val lastInRoute = Array.tabulate(V)(i => IntVar(m,0,Next.length,i,"LastInRoute "+i))

    Routes(V, Next, PositionInRoute, RouteNr, RouteLength,lastInRoute)
  }
}
