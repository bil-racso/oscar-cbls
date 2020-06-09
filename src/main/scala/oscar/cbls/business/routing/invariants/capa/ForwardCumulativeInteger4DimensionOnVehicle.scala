package oscar.cbls.business.routing.invariants.capa

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

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, ChangingSeqValue, Domain, DomainRange, IntNotificationTarget, IntValue}
import oscar.cbls.core.propagation.Checker

object ForwardCumulativeInteger4DimensionOnVehicle {
  /**
   * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
   * @param content1AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param content2AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param default1ForUnroutedNodes is the content1 of a node that is not routed
   * @param default2ForUnroutedNodes is the content2 of a node that is not routed
   * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
   * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
   * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invairant
   * @return  (content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle) content at end is the content when back at the vehicle start
   */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Long,Long,Long,Long)=>(Long,Long,Long,Long),
            content1AtStart:Array[IntValue],
            content2AtStart:Array[IntValue],
            content3AtStart:Array[IntValue],
            content4AtStart:Array[IntValue],
            default1ForUnroutedNodes:Long,
            default2ForUnroutedNodes:Long,
            default3ForUnroutedNodes:Long,
            default4ForUnroutedNodes:Long,
            minContent:Long = 0L,
            maxContent:Long = Long.MaxValue,
            contentName:String = "content"):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],
    Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar]) ={

    val content1AtNode = Array.tabulate[CBLSIntVar](n)((node: Int) => CBLSIntVar(routes.model, default1ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default1ForUnroutedNodes), contentName + "1 at node "+node))
    val content2AtNode = Array.tabulate[CBLSIntVar](n)((node: Int) => CBLSIntVar(routes.model, default2ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default2ForUnroutedNodes), contentName + "2 at node "+node))
    val content3AtNode = Array.tabulate[CBLSIntVar](n)((node: Int) => CBLSIntVar(routes.model, default3ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default3ForUnroutedNodes), contentName + "3 at node "+node))
    val content4AtNode = Array.tabulate[CBLSIntVar](n)((node: Int) => CBLSIntVar(routes.model, default4ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default4ForUnroutedNodes), contentName + "4 at node "+node))

    val content1AtEnd = Array.tabulate[CBLSIntVar](v)((vehicle: Int) => CBLSIntVar(routes.model, default1ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default1ForUnroutedNodes), contentName + "1 at end of route " + vehicle))
    val content2AtEnd = Array.tabulate[CBLSIntVar](v)((vehicle: Int) => CBLSIntVar(routes.model, default2ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default2ForUnroutedNodes), contentName + "2 at end of route " + vehicle))
    val content3AtEnd = Array.tabulate[CBLSIntVar](v)((vehicle: Int) => CBLSIntVar(routes.model, default3ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default3ForUnroutedNodes), contentName + "3 at end of route " + vehicle))
    val content4AtEnd = Array.tabulate[CBLSIntVar](v)((vehicle: Int) => CBLSIntVar(routes.model, default4ForUnroutedNodes, Domain.coupleToDomain(minContent,maxContent).union(default4ForUnroutedNodes), contentName + "4 at end of route " + vehicle))

    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, DomainRange(0L,n), "last point of vehicle" + vehicle))

    new ForwardCumulativeInteger4DimensionOnVehicle(routes,n,v,op,
      content1AtStart,content2AtStart,content3AtStart,content4AtStart,
      content1AtNode,content2AtNode,content3AtNode,content4AtNode,
      content1AtEnd,content2AtEnd,content3AtEnd,content4AtEnd,
      lastPointOfVehicle,
      default1ForUnroutedNodes,default2ForUnroutedNodes,default3ForUnroutedNodes,default4ForUnroutedNodes)
    (content1AtNode,content2AtNode,content3AtNode,content4AtNode,
      content1AtEnd,content2AtEnd,content3AtEnd,content4AtEnd,lastPointOfVehicle)
  }
}

class ForwardCumulativeInteger4DimensionOnVehicle(routes:ChangingSeqValue,
                                                  n:Int,
                                                  v:Int,
                                                  op:(Int,Int,Long,Long,Long,Long)=>(Long,Long,Long,Long),
                                                  content1AtStart:Array[IntValue],
                                                  content2AtStart:Array[IntValue],
                                                  content3AtStart:Array[IntValue],
                                                  content4AtStart:Array[IntValue],

                                                  content1AtNode:Array[CBLSIntVar],
                                                  content2AtNode:Array[CBLSIntVar],
                                                  content3AtNode:Array[CBLSIntVar],
                                                  content4AtNode:Array[CBLSIntVar],

                                                  content1AtEnd:Array[CBLSIntVar],
                                                  content2AtEnd:Array[CBLSIntVar],
                                                  content3AtEnd:Array[CBLSIntVar],
                                                  content4AtEnd:Array[CBLSIntVar],

                                                  lastPointOfVehicle:Array[CBLSIntVar],

                                                  defaultVehicleContent1ForUnroutedNodes:Long,
                                                  defaultVehicleContent2ForUnroutedNodes:Long,
                                                  defaultVehicleContent3ForUnroutedNodes:Long,
                                                  defaultVehicleContent4ForUnroutedNodes:Long)


  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v) with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(content1AtStart)
  registerStaticAndDynamicDependencyArrayIndex(content2AtStart)
  registerStaticAndDynamicDependencyArrayIndex(content3AtStart)
  registerStaticAndDynamicDependencyArrayIndex(content4AtStart)

  finishInitialization()

  for(i <- content1AtNode) i.setDefiningInvariant(this)
  for(i <- content2AtNode) i.setDefiningInvariant(this)
  for(i <- content3AtNode) i.setDefiningInvariant(this)
  for(i <- content4AtNode) i.setDefiningInvariant(this)


  for(i <- content1AtEnd) i.setDefiningInvariant(this)
  for(i <- content2AtEnd) i.setDefiningInvariant(this)
  for(i <- content3AtEnd) i.setDefiningInvariant(this)
  for(i <- content4AtEnd) i.setDefiningInvariant(this)

  for(i <- lastPointOfVehicle) i.setDefiningInvariant(this)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long){
    toUpdateZonesAndVehicleStartAfter match {
      case None => ;
      case Some((toUpdateZones,vehicleLocation)) =>
        toUpdateZonesAndVehicleStartAfter = Some((toUpdateZones.insert(id, smartPrepend(0,0,toUpdateZones.getOrElse(id, List.empty[(Int,Int)]))),vehicleLocation))
    }
    scheduleForPropagation()
  }

  /**
   * @param vehicle
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtStart(vehicle : Int) : Boolean = {
    val newValue1 = content1AtStart(vehicle).value
    val newValue2 = content2AtStart(vehicle).value
    val newValue3 = content3AtStart(vehicle).value
    val newValue4 = content4AtStart(vehicle).value

    val oldValue1 = content1AtNode(vehicle).newValue
    val oldValue2 = content2AtNode(vehicle).newValue
    val oldValue3 = content3AtNode(vehicle).newValue
    val oldValue4 = content4AtNode(vehicle).newValue

    if(oldValue1 == newValue1 && oldValue2 == newValue2 && oldValue3 == newValue3 && oldValue4 == newValue4){
      false
    }else{
      content1AtNode(vehicle) := newValue1
      content2AtNode(vehicle) := newValue2
      content3AtNode(vehicle) := newValue3
      content4AtNode(vehicle) := newValue4

      true
    }
  }

  /**
   * @param prevNode
   * @param node
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtNode(prevNode : Int, node : Int) : Boolean = {
    val oldValue1 = content1AtNode(node).newValue
    val oldValue2 = content2AtNode(node).newValue
    val oldValue3 = content3AtNode(node).newValue
    val oldValue4 = content4AtNode(node).newValue

    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2,newValue3,newValue4) = op(prevNode,node,
      content1AtNode(prevNode).newValue,
      content2AtNode(prevNode).newValue,
      content3AtNode(prevNode).newValue,
      content4AtNode(prevNode).newValue)

    if(oldValue1 != newValue1 || oldValue2 != newValue2 || oldValue3 != newValue3 || oldValue4 != newValue4) {
      content1AtNode(node) := newValue1
      content2AtNode(node) := newValue2
      content3AtNode(node) := newValue3
      content4AtNode(node) := newValue4
      true
    }else{
      false
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int){
    lastPointOfVehicle(vehicle) := lastNode
    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2,newValue3,newValue4) = op(lastNode,vehicle,
      content1AtNode(lastNode).newValue,
      content2AtNode(lastNode).newValue,
      content3AtNode(lastNode).newValue,
      content4AtNode(lastNode).newValue
    )
    content1AtEnd(vehicle) := newValue1
    content2AtEnd(vehicle) := newValue2
    content3AtEnd(vehicle) := newValue3
    content4AtEnd(vehicle) := newValue4
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]){
    for(node <- unroutedNodes) {
      content1AtNode(node) := defaultVehicleContent1ForUnroutedNodes
      content2AtNode(node) := defaultVehicleContent2ForUnroutedNodes
      content3AtNode(node) := defaultVehicleContent3ForUnroutedNodes
      content4AtNode(node) := defaultVehicleContent4ForUnroutedNodes
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c,routes.value)
  }
  def check(c : Checker,s:IntSequence){

    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    def op4(fromNode:Int,toNode:Int,content:(Long,Long,Long,Long)) = op(fromNode,toNode,content._1,content._2,content._3,content._4)

    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) =
      AbstractVehicleCapacity.computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(
        n,
        v,
        op4,
        v => (content1AtStart(v).value,content2AtStart(v).value,content3AtStart(v).value,content4AtStart(v).value),
        s,
        (defaultVehicleContent1ForUnroutedNodes,defaultVehicleContent2ForUnroutedNodes,defaultVehicleContent3ForUnroutedNodes,defaultVehicleContent4ForUnroutedNodes))

    val currentVehicleLocation = this.toUpdateZonesAndVehicleStartAfter.get._2

    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == s.positionOfAnyOccurrence(vehicle).get,
        Some("Found start of vehicle(" + vehicle + "):=" + vehicleLocation.startPosOfVehicle(vehicle) +
          " should be :=" + s.positionOfAnyOccurrence(vehicle) +" seq :"+ s.mkString(",")))
      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }

    for(node <- 0 until n){
      c.check(nodeToContent(node) == (content1AtNode(node).newValue,content2AtNode(node).newValue,content3AtNode(node).newValue,content4AtNode(node).newValue),
        Some("Vehicle content at node(" + node + ") at pos : "+ s.positionsOfValue(node)+ " := " +
          (content1AtNode(node).newValue,content2AtNode(node).newValue,content3AtNode(node).newValue,content4AtNode(node).newValue) +
          " should be :=" + nodeToContent(node)+ " routes:" + s.mkString(",")))
    }

    for(vehicle <- 0 until v){
      c.check((content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue,content3AtEnd(vehicle).newValue,content4AtEnd(vehicle).newValue) == vehicleToContentAtEnd(vehicle),
        Some("Error on vehicle content at end vehicle:" + vehicle + " contentAtEnd(vehicle).newValue:" +
          (content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue,content3AtEnd(vehicle).newValue,content4AtEnd(vehicle).newValue) + " should be:" +  vehicleToContentAtEnd(vehicle)))
    }
  }
}
