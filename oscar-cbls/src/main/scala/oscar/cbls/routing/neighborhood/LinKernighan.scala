package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart

import scala.collection.mutable.ListBuffer

/**
  * Created by fabian on 18-04-16.
  */
case class LinKernighan(routeNr:Int,
                        override val vrp: VRP with PositionInRouteAndRouteNr with Predecessors with ClosestNeighbors with HopDistance,
                        k:Int = 5,
                        neighborhoodName: String = "LinKernighan",
                        best: Boolean = false,
                        hotRestart: Boolean = true) extends EasyRoutingNeighborhood[LinKernighanMove](best, vrp, neighborhoodName) {

  var selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
  var routeLength = selectedRoute.length
  /*
   * This variable contains the state of each node of the selected route.
   * If the value is false, that means that the node has already been selected to create a new link.
   * (if it has already been selected you can't use it again)
   * If the value is true, that means that you can use this node to create a new link.
   */
  var availableNode : Array[Boolean] = new Array[Boolean](selectedRoute.length)
  /*
   * This variable represents the broken links
   */
  var oldLink : Array[Int] = new Array[Int](selectedRoute.length)
  /*
   * This variable represents the created links
   */
  var newLink : Array[Int] = new Array[Int](selectedRoute.length)
  var startIndice = 0
  var currentDelta = 0
  var firstNode = 0

  val isAvailable:(Int,Int)=>Boolean = (node:Int, toNode:Int) =>{
    if(vrp.routeNr(node).value == routeNr && vrp.preds(node).value != toNode && vrp.next(node).value != toNode && node != toNode)
      availableNode(vrp.positionInRoute(node).value)
    else
      false
  }

  override def exploreNeighborhood(): Unit = {

    selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
    routeLength = selectedRoute.length
    availableNode = Array.tabulate(routeLength)(n => true)
    oldLink = Array.tabulate(routeLength)(n => vrp.N)
    newLink = Array.tabulate(routeLength)(n => vrp.N)
    currentDelta = 0

    println(selectedRoute.toList)

    val iterationSchemeOnZone =
    if (hotRestart && !best) HotRestart(selectedRoute, startIndice).toArray
    else selectedRoute

    for(firstNode <- iterationSchemeOnZone.indices){
      this.firstNode = firstNode + startIndice
      for(secondNode <- List((firstNode-1+routeLength)%routeLength,(firstNode+1)%routeLength)){
        availableNode(secondNode+startIndice) = false
        availableNode(this.firstNode) = false
        oldLink(this.firstNode) = secondNode
        oldLink(secondNode+startIndice) = firstNode
        internalExploration(secondNode+startIndice)
        oldLink(this.firstNode) = vrp.N
        oldLink(secondNode+startIndice) = vrp.N
      }
    }
  }

  def internalExploration(node:Int): Unit ={

    def checkDelta(predsNode:Int, baseNode:Int,promisingNode:Int): Boolean ={
      if(currentDelta + (vrp.distanceFunction(baseNode,promisingNode) -
        vrp.distanceFunction(predsNode,baseNode)) < 0) {
        currentDelta += vrp.distanceFunction(baseNode, promisingNode) - vrp.distanceFunction(predsNode, baseNode)
        true
      }
      else
        false
    }

    val promisingNodes = vrp.kNearest(k,isAvailable(_,selectedRoute(node)))(selectedRoute(node))
    //we browse the promising nodes
    for(pN <- promisingNodes){
      //If the the length of the link cut is longer than the link we want to create we keep the move
      if(checkDelta(selectedRoute(vrp.preds(node).value),selectedRoute(node),pN)) {
        //We disable the node (so we can't use it anymore) and we create the new link
        val positionPN = vrp.positionInRoute(pN).value
        availableNode(positionPN) = false
        newLink(node) = positionPN
        newLink(positionPN) = node

        //We select the next node to proceed (either the prev or the next of the current node)
        for(newNode <- List((positionPN-1+routeLength)%routeLength,(positionPN+1)%routeLength)){
          /*
            * If the new route create by linking the newNode and the first node is better than the old one
            * we keep the new route
           */
          availableNode(newNode) = false
          newLink(newNode) = firstNode
          newLink(firstNode) = newNode
          oldLink(newNode) = positionPN
          oldLink(positionPN) = newNode
          encode(oldLink,newLink,selectedRoute,firstNode,availableNode)
          println(affects)
          println("SelectedRoute : " + selectedRoute.toList)
          println("OldLink : " + oldLink.toList)
          println("NewLink : " + newLink.toList)
          println("Availability : " + availableNode.toList)
          if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())){
            startIndice = (firstNode + 1)%routeLength
            return
          }
          /*
           * Else we check for other solution by searching deeper (we increase the level of the Lin-Kernighan algorithm)
           */
          newLink(newNode) = vrp.N
          newLink(firstNode) = vrp.N
          internalExploration(newNode)
          //If we are here that means that no solution has been discovered by going this way so we backtrack
          oldLink(newNode) = vrp.N
          oldLink(positionPN) = vrp.N
          availableNode(newNode) = true
        }
        availableNode(positionPN) = true
        newLink(positionPN) = vrp.N
        newLink(node) = vrp.N
        currentDelta -=
          vrp.distanceFunction(selectedRoute(node), pN) -
            vrp.distanceFunction(selectedRoute(vrp.preds(node).value), selectedRoute(node))
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): LinKernighanMove = {
    null
  }

  def encode(oldLink:Array[Int],newLink:Array[Int],selectedRoute:Array[Int],firstNode:Int,availableNode:Array[Boolean]): Unit = {
    var currentNodeIndice = firstNode
    var segmentStartNodeIndice = firstNode
    var oldNodeIndice = 0
    var direction = 1
    var stop = false
    /*
    println("SelectedRoute : " + selectedRoute.toList)
    println("OldLink : " + oldLink.toList)
    println("NewLink : " + newLink.toList)
    println("Availability : " + availableNode.toList)*/
    //TODO : Find the problem that make the route cycling possible relation with the base value of oldLink/newLink
    while (!stop){
      /*println("CurrentNodeIndice : " + currentNodeIndice)
      println(availableNode(currentNodeIndice))*/
      while (availableNode(currentNodeIndice)) {
        currentNodeIndice = (currentNodeIndice + direction + routeLength)%routeLength
      }
      if (currentNodeIndice == firstNode && segmentStartNodeIndice != firstNode)
        stop = true

      if(!stop && direction == -1) {
        val s = cut(selectedRoute(segmentStartNodeIndice),selectedRoute(currentNodeIndice))
        val reverseS = reverse(s)
        insert(reverseS,selectedRoute(oldNodeIndice))
      }
      else if(!stop && currentNodeIndice != segmentStartNodeIndice){
        val s = cut(selectedRoute(segmentStartNodeIndice),selectedRoute(currentNodeIndice))
        insert(s, selectedRoute(oldNodeIndice))
      }

      /*
       * If a certain condition is met, the next segment must be reverse
       */
      println(currentNodeIndice)
      println(newLink(currentNodeIndice))
      println(oldLink(newLink(currentNodeIndice)))
      if(oldLink(newLink(currentNodeIndice)) > newLink(currentNodeIndice))
        direction = -1
      else
        direction = 1
      //println("Direction = " + direction)
      /*
       * We assign the new value of each variable
       */
      oldNodeIndice = currentNodeIndice
      segmentStartNodeIndice = newLink(currentNodeIndice)
      currentNodeIndice = (newLink(currentNodeIndice)+direction+routeLength)%routeLength
    }
  }
}

case class LinKernighanMove(availableNodes:Array[Boolean],
                            selectedRoute:Array[Int],
                            oldLink:Array[Int],
                            newLink:Array[Int],
                            firstNode:Int,
                            override val objAfter: Int,
                            override val neighborhood: LinKernighan,
                            override val neighborhoodName: String = "LinKernighanMove") extends VRPMove(objAfter, neighborhood, neighborhoodName){
  override def encodeMove(): Unit = {
    neighborhood.encode(oldLink,newLink,selectedRoute,firstNode,availableNodes)
  }

  override def impactedPoints: List[Int] = selectedRoute.toList.filter(availableNodes(_))
}
