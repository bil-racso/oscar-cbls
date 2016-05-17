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
package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart


/**
  * Perform one iteration of the Lin-Kernighan algorithm.
  * The Lin-Kernighan algorithm is an algorithm working only one a single route
  * that perform an p-opt move where p isn't defined at first but
  * increase or decrease during the execution of the algorithm.
  * Once the algorithm has found a p-opt move that could reduce the objectif value, it performs the move.
  *
  * @param routeNr the number of the route we are working on
  * @param vrp the vrp
  * @param k the number of node we consider for the k-nearest.
  *          If you k is too small, you'll have big lines crossing the map.
  * @param p the max depth of the algorithm.
  *          For large amount of nodes I would suggest a small p and a large k.
  *          It'll be faster and the result is quite good.
  *          For instance with 200 nodes, p = 5 and k = 10, it takes 11746 ms to find a solution.
  *          KEEP in MIND that each time you increase p, you multiply the number of possible move by k
  * @param neighborhoodName the name of the neighborhood
  * @param best If you want to perform the best move. BUT I would NOT use it if I where you. The run time explodes with it.
  * @param hotRestart If you want to use hotRestart
  * @author fabian.germeau@student.vinci.be
  */
case class LinKernighan(routeNr:Int,
                        override val vrp: VRP with PositionInRouteAndRouteNr with Predecessors with HopClosestNeighbors,
                        k:Int = 20,
                        p:Int = 5,
                        neighborhoodName: String = "LinKernighan",
                        best: Boolean = false,
                        hotRestart: Boolean = false) extends EasyRoutingNeighborhood[LinKernighanMove](best, vrp, neighborhoodName) {

  vrp.computeClosestNeighbors()
  //A copy of the route we are working on and his length
  var selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
  var routeLength = selectedRoute.length
  //An array that keep the position of each node
  var positionInRoute : Array[Int] = new Array[Int](vrp.N)
  //A copy of the distanceMatrix
  var distanceMatrix : Array[Array[Int]] = new Array[Array[Int]](selectedRoute.length)

  /*
   * This variable contains the state of each node of the selected route.
   * If the value is false, that means that the node has already been selected to create a new link.
   * (if it has already been selected you can't use it again)
   * If the value is true, that means that you can use this node to create a new link.
   */
  var availableNode : Array[Boolean] = new Array[Boolean](selectedRoute.length)
  /*
   * This variable represents the broken links.
   * If the value is vrp.N, it means that this node has no broken links
   */
  var oldLink : Array[Int] = new Array[Int](selectedRoute.length)
  /*
   * This variable represents the created links.
   * If the value is vrp.N, it means that this node has no newly created links
   */
  var newLink : Array[Int] = new Array[Int](selectedRoute.length)
  /*
   * This variable contains the extremities of each newly created segment.
   * A newly created segment is an untouched sub-segment of the original route
   * For instance if we execute a 2-opt with the 1st, 2nd, 4th and 5th node
   * we will have two newly created segments, one starting at the 2nd node and finishing at the 4th one and
   * another one starting at the 5th node and finishing at the 1st one.
   */
  var segmentExtremities : Array[Int] = new Array[Int](selectedRoute.length)

  /*
   * Respectively :
   * the index of the first node we are working with (used for hotRestart)
   * the current benefit of the move (without the last link)
   * the index of the current first node
   * this value simply says if the first node (of the first cut couple) is before the second
   */
  var startIndex = 0
  var currentDelta = 0
  var currentP = 1
  var firstNode = 0
  var firstBeforeSecond = false

  /*
   * This method links two nodes together.
   * If old is true, it means that we are linking two previously unlinked nodes (we are backtracking)
   */
  def linkNode(n1:Int,n2:Int,old:Boolean): Unit ={
    if(old) {
      oldLink(n1) = vrp.N
      oldLink(n2) = vrp.N
      availableNode(n1) = true
      availableNode(n2) = true
    }
    else {
      newLink(n1) = n2
      newLink(n2) = n1
    }
  }

  /*
   * This method unlinks two nodes together.
   * If old is true, it means that we are unlinking two previously linked nodes (we are backtracking)
   */
  def unLinkNode(n1:Int,n2:Int,old:Boolean): Unit ={
    if(old) {
      oldLink(n1) = n2
      oldLink(n2) = n1
      availableNode(n1) = false
      availableNode(n2) = false
    }
    else{
      newLink(n1) = vrp.N
      newLink(n2) = vrp.N
    }
  }

  /**
    * This method maintains the segmentExtremities variable by inserting a new couple of nodes.
    * @param node1 the first node of the segment
    * @param node2 the second node of the segment
    */
  def addSegmentExtremities(node1:Int,node2:Int): Unit ={
    var current = firstNode
    var right = 0
    var left = 0
    if(node1 < firstNode){
      if(!firstBeforeSecond)
        current = prevNode(current)
      val start = current
      do
        current = prevNode(segmentExtremities(current))
      while(current > node1 && current < start)
      left = nextNode(current)
      right = segmentExtremities(left)
    }else{
      if(firstBeforeSecond)
        current = nextNode(current)
      val start = current
      do
        current = nextNode(segmentExtremities(current))
      while(current < node1 && current > start)
      right = prevNode(current)
      left = segmentExtremities(right)
    }
    if(node1 == 0 && node2 == routeLength - 1){
      segmentExtremities(left) = node2
      segmentExtremities(node2) = left
      segmentExtremities(node1) = right
      segmentExtremities(right) = node1
    }
    else if (node1 < node2 || (node1 == routeLength - 1 && node2 == 0)) {
      segmentExtremities(left) = node1
      segmentExtremities(node1) = left
      segmentExtremities(node2) = right
      segmentExtremities(right) = node2
    } else {
      segmentExtremities(left) = node2
      segmentExtremities(node2) = left
      segmentExtremities(node1) = right
      segmentExtremities(right) = node1
    }
    //println("after",segmentExtremities.toList)
  }

  /*
   * This method maintains the segmentExtremities variable by removing a couple of nodes.
   */
  def removeSegmentExtremities(node1:Int,node2:Int): Unit ={
    segmentExtremities(segmentExtremities(node1)) = segmentExtremities(node2)
    segmentExtremities(segmentExtremities(node2)) = segmentExtremities(node1)
    segmentExtremities(node1) = vrp.N
    segmentExtremities(node2) = vrp.N
  }

  /*
   * This method simply return the next node
   */
  def nextNode(node:Int): Int ={
    (node+1)%routeLength
  }

  /*
   * This method simply return the previous node
   */
  def prevNode(node:Int): Int ={
    if(node == 0)
      routeLength-1
    else
      node-1
  }

  override def exploreNeighborhood(): Unit = {
    //We first reset all the used values
    selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
    routeLength = selectedRoute.length
    availableNode = Array.tabulate(routeLength)(n => true)
    oldLink = Array.tabulate(routeLength)(n => vrp.N)
    newLink = Array.tabulate(routeLength)(n => vrp.N)
    segmentExtremities = Array.tabulate(routeLength)(n => vrp.N)
    currentDelta = 0
    currentP = 1
    firstBeforeSecond = false
    positionInRoute = Array.tabulate(vrp.N)(n => -1)
    distanceMatrix = Array.tabulate(routeLength)(n => Array.tabulate(routeLength)(n1 => Int.MaxValue))
    for(n <- selectedRoute.indices){
      positionInRoute(selectedRoute(n)) = n
      for(n2 <- selectedRoute.indices){
        distanceMatrix(n)(n2) = vrp.distanceFunction(selectedRoute(n),selectedRoute(n2))
      }
    }


    val iterationSchemeOnZone =
    if (hotRestart && !best) HotRestart(selectedRoute, startIndex).toArray
    else selectedRoute

    for(firstNode <- iterationSchemeOnZone.indices){
      this.firstNode = firstNode
      for(secondNode <- List(nextNode(firstNode),prevNode(firstNode))) {
        firstBeforeSecond = !firstBeforeSecond
        unLinkNode(this.firstNode, secondNode, true)
        segmentExtremities(firstNode) = secondNode
        segmentExtremities(secondNode) = firstNode
        if (internalExploration(secondNode)) {
          startIndex = nextNode(startIndex)
          return
        }
        segmentExtremities(firstNode) = vrp.N
        segmentExtremities(secondNode) = vrp.N
        linkNode(this.firstNode, secondNode, true)
      }
    }
  }

  /**
    *
    * @param node the node we are working with
    * @return
    */
  def internalExploration(node:Int): Boolean ={
    /**
      * This method check if the sum of all the links's length we have created is smaller than
      * the sum of all links's length we have removed
      *
      * @param predsNode
      * @param baseNode
      * @param promisingNode
      * @return
      */
    def checkDelta(predsNode:Int, baseNode:Int,promisingNode:Int): Boolean ={
      if(currentDelta + (distanceMatrix(baseNode)(promisingNode) -
        distanceMatrix(predsNode)(baseNode)) < 0) {
        currentDelta += distanceMatrix(baseNode)(promisingNode) - distanceMatrix(predsNode)(baseNode)
        true
      }
      else
        false
    }

    /**
      * This method check if the removal of the link linking
      * @param node
      * @param baseNode
      * @param newNode
      * @return
      */
    def isAllowed(node:Int, baseNode:Int, newNode:Int): Boolean ={
      if(prevNode(newNode) == oldLink(newNode)){
        if(nextNode(baseNode) == node) {
          if(segmentExtremities(newNode) == oldLink(newNode))
            return false
          else if(segmentExtremities(newNode) < newNode){
            if(baseNode > newNode || baseNode < segmentExtremities(newNode))
              return false
          }else{
            if(baseNode > newNode && baseNode < segmentExtremities(newNode))
              return false
          }
        }
      }else{
        if(prevNode(baseNode) == node){
          if(segmentExtremities(newNode) == oldLink(newNode))
            return false
          else if(segmentExtremities(newNode) > newNode){
            if(baseNode < newNode || baseNode > segmentExtremities(newNode))
              return false
          }else{
            if(baseNode < newNode && baseNode > segmentExtremities(newNode))
              return false
          }
        }
      }
      if(firstBeforeSecond) {
        if (nextNode(node) != baseNode) {
          if (firstNode < segmentExtremities(firstNode)) {
            if (node < firstNode || node > segmentExtremities(firstNode))
              return true
            else
              return false
          }
          else {
            if (node > firstNode || node < segmentExtremities(firstNode))
              return false
            else
              return true
          }
        }else
          return true
      }else{
        if (prevNode(node) != baseNode) {
          if (firstNode > segmentExtremities(firstNode)) {
            if (node > firstNode || node < segmentExtremities(firstNode))
              return true
            else
              return false
          }
          else {
            if (node < firstNode || node > segmentExtremities(firstNode))
              return false
            else
              return true
          }
        }else
          return true
      }
    }

    def getPossibleOldLink(pos:Int,newNode:Int):List[Int] = {
      val preds = prevNode(pos)
      val next = nextNode(pos)
      var res:List[Int] = List.empty
      if(availableNode(preds) && isAllowed(preds,pos,newNode))
        res = preds :: res
      if(availableNode(next) && isAllowed(next,pos,newNode))
        res = next :: res
      res
    }

    /**
      * This method check if the route containing the node is cycling.
      * @param node
      * @return
      */
    def isCycling(node:Int): Boolean ={
      var counter = 0
      var currentNode = node
      var linkedNode = node
      do{
        currentNode = linkedNode
        currentNode = segmentExtremities(currentNode)
        linkedNode = newLink(currentNode)
        counter += 1
      }while(currentNode != node && linkedNode != node)

      if(counter != currentP)
        true
      else
        false
    }

    /**
      * This method return the K promissing nodes of the in parameter
      * @param node
      * @return
      */
    def getKPromisingNodes(node:Int): List[Int] ={
      val nodesList = Array.tabulate(routeLength)(n => n)
      val res = nodesList.filter(availableNode(_)).sortWith(distanceMatrix(node)(_) < distanceMatrix(node)(_)).take(k).toList
      res
    }


    //We increase depth
    currentP += 1

    //we browse the promising nodes
    for(positionPN <- getKPromisingNodes(node)){
      //If the the length of the link cut is longer than the link we want to create we keep the move
      if(checkDelta(oldLink(node),node,positionPN)) {
        linkNode(node,positionPN,false)

        //We select the next node to proceed (either the prev or the next of the current node)
        for(oldNode <- getPossibleOldLink(positionPN,node)) {
          /*
            * If the new route create by linking the newNode and the first node is better than the old one
            * we keep the new route
           */
          unLinkNode(positionPN,oldNode,true)
          addSegmentExtremities(positionPN,oldNode)
          if(currentDelta + distanceMatrix(oldNode)(firstNode) - distanceMatrix(oldNode)(positionPN) < 0) {
            linkNode(oldNode, firstNode, false)
            if (!isCycling(oldNode)) {
              encode(oldLink, newLink, selectedRoute, firstNode, availableNode)
              if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
                return true
              }
            }
            /*
           * Else we check for other solution by searching deeper (we increase the level of the Lin-Kernighan algorithm)
           */
            unLinkNode(oldNode, firstNode, false)
          }
          if(currentP < p && internalExploration(oldNode))
            return true
          //If we are here that means that no solution has been discovered by going this way so we backtrack
          linkNode(positionPN,oldNode,true)
          removeSegmentExtremities(positionPN,oldNode)
        }
        currentDelta -=
        distanceMatrix(node)(positionPN) -
        distanceMatrix(oldLink(node))(node)
        unLinkNode(node,positionPN,false)
      }
    }
    currentP -= 1
    false
  }

  override def instantiateCurrentMove(newObj: Int): LinKernighanMove = {
    LinKernighanMove(availableNode,selectedRoute,oldLink,newLink,firstNode,newObj,this,neighborhoodName)
  }

  def encode(oldLink:Array[Int],newLink:Array[Int],selectedRoute:Array[Int],firstNode:Int,availableNode:Array[Boolean]): Unit = {
    var currentNodeIndice = if(firstBeforeSecond)segmentExtremities(nextNode(firstNode))else segmentExtremities(firstNode)
    var segmentStartNodeIndice = vrp.N
    var insertNodeIndice = vrp.N
    var reverseSegment:Boolean = false
    var stop = false

    //This variable contains all the segments that just need to be assign to the next value of a node
    var toAssign:List[(Int,Int,Int)] = Nil
    //This variable contains all the segments that need to be reverse and assign to the next value of a node
    var toReverseList:List[(Int,Int,Int)] = Nil
    while (!stop){
      if ((firstBeforeSecond && segmentStartNodeIndice == nextNode(firstNode))
        || (!firstBeforeSecond && segmentStartNodeIndice == firstNode))
        stop = true

      if(reverseSegment) {
        //We don't get the value of the node : prevNode(currentNodeIndice)  because we need his position to order the list
        toReverseList = (prevNode(currentNodeIndice), selectedRoute(segmentStartNodeIndice), selectedRoute(insertNodeIndice))::toReverseList
      }
      else if(segmentStartNodeIndice != vrp.N){
        toAssign = (selectedRoute(segmentStartNodeIndice),selectedRoute(currentNodeIndice), selectedRoute(insertNodeIndice))::toAssign
      }

      /*
       * We assign the new value of each variable
       */
      insertNodeIndice = currentNodeIndice
      segmentStartNodeIndice = newLink(currentNodeIndice)
      currentNodeIndice = segmentExtremities(segmentStartNodeIndice)
      /*
       * If a certain condition is met, the next segment must be reverse
       */
      if(nextNode(segmentStartNodeIndice) == oldLink(segmentStartNodeIndice))
        reverseSegment = true
      else
        reverseSegment = false
    }
    var cuttedSegments:List[(Segment,Int)] = Nil
    for(r <- toReverseList)
      cuttedSegments = (cut(selectedRoute(r._1),r._2),r._3)::cuttedSegments
    for(cs <- cuttedSegments) {
      val rs = reverse(cs._1)
      insert(rs, cs._2)
    }

    for(a <- toAssign){
      addMove(affectFromConst(a._3, a._1))
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
