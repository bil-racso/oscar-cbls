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



import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

//TODO CHK => bytw todo ?

/**
  * Created by  Jannou Brohée on 22/11/16.
  */

object GenericCumulativeIntegerDimensionOnVehicleWithVar {



  /**
    * Implements a GenericCumulativeIntegerDimensionOnVehicle Invariant
    *
    * @param routes The sequence representing the route associated at each vehicle
    * @param n the maximum number of nodes
    * @param v the number of vehicles
    * @param op a function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
    * @return the capacity of each node in the sequence representinf the route associeted at each vehicle
    */
  def apply(routes:ChangingSeqValue, n:Int,v:Int,op :(Int,Int,Int)=>Int,initValue:Array[CBLSIntVar], minContent :Int=Int.MinValue, maxContent:Int=Int.MaxValue,maxStack:Int =4):Array[CBLSIntVar] ={
    var output: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), "capacity at node("+node.toString+")"))
    new GenericCumulativeIntegerDimensionOnVehicleWithVar(routes, n, v, op, initValue,output,maxStack).getOutput()
    output
  }
}

/**
  * Maintains the current capacity of each vehicle at each node after a SeqUpdate
  *
  * @param routes The sequence representing the route associated at each vehicle
  * @param n the maximum number of nodes
  * @param v the number of vehicles
  * @param op a function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
  * @param initValue an array giving the initial capacity of a vehicle at his starting node (0, v-1)
  * @param output The array which store, for any node, the capacity of the vehicle associated at the node
  */
class GenericCumulativeIntegerDimensionOnVehicleWithVar(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, initValue :Array[CBLSIntVar], output:Array[CBLSIntVar],maxStack:Int)
  extends GenericCumulativeIntegerDimensionOnVehicle(routes, n, v, op, Array.tabulate(v)((car:Int)=>0),output,maxStack)
    with IntNotificationTarget {

  private var todo : RedBlackTreeMap[List[(Int,Int)]]= RedBlackTreeMap.empty[List[(Int,Int)]]

  require(initValue.length==v)
  require( output.length==n)


  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for (car  <- 0 until v) registerStaticAndDynamicDependency(initValue(car),car)
  for(i <- output) i.setDefiningInvariant(this)

  computeAndAffectContentAndVehicleStartPositionsFromScratch()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    todo = digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes)
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit ={
    todo = todo.insert(id, smartPrepend(0,0,todo.getOrElse(id,List.empty[(Int,Int)])))
    scheduleForPropagation()
  }


  /** this is the propagation method that should be overridden by propagation elements.
    * notice that it is only called in a propagation wave if:
    * 1: it has been registered for propagation since the last time it was propagated
    * 2: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
    * it only propagates the ones that come in the predecessors of the targeted propagation element
    * overriding this method is optional, so an empty body is provided by default */
  override def performPropagation(): Unit = {
    todo match {
      case null => computeAndAffectContentAndVehicleStartPositionsFromScratch()
      case tree =>
        for(car <- tree.keys)  {
          val lst = todo.get(car).get
          if (lst.nonEmpty) updateVehicleContent(routes.newValue,lst,  positionOfVehicle(car)  ,car)
        }
    }
    todo= RedBlackTreeMap.empty[List[(Int,Int)]]
  }

  override def getContentAtVehicleStart(vehicle: Int): Int = initValue(vehicle).newValue

  override def updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastNotified(value: IntSequence): RedBlackTreeMap[List[(Int, Int)]] =  todo

}
