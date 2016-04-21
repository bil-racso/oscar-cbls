package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart


/**
  * Created by fabian on 18-04-16.
  */
case class LinKernighan(routeNr:Int,
                        override val vrp: VRP with PositionInRouteAndRouteNr with Predecessors with HopClosestNeighbors,
                        k:Int = 10,
                        neighborhoodName: String = "LinKernighan",
                        best: Boolean = false,
                        hotRestart: Boolean = false) extends EasyRoutingNeighborhood[LinKernighanMove](best, vrp, neighborhoodName) {

  vrp.computeClosestNeighbors()
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
  var firstBeforeSecond = false

  val isAvailable:(Int,Int)=>Boolean = (node:Int, toNode:Int) =>{
    if(vrp.routeNr(node).value == routeNr && node != toNode) {
      availableNode(vrp.positionInRoute(node).value)
    }
    else
      false
  }

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

  def nextNode(node:Int): Int ={
    (node+1)%routeLength
  }

  def prevNode(node:Int): Int ={
    if(node == 0)
      routeLength-1
    else
      node-1
  }

  override def exploreNeighborhood(): Unit = {

    selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
    routeLength = selectedRoute.length
    availableNode = Array.tabulate(routeLength)(n => true)
    oldLink = Array.tabulate(routeLength)(n => vrp.N)
    newLink = Array.tabulate(routeLength)(n => vrp.N)
    currentDelta = 0
    firstBeforeSecond = false

    val iterationSchemeOnZone =
    if (hotRestart && !best) HotRestart(selectedRoute, startIndice).toArray
    else selectedRoute

    for(firstNode <- iterationSchemeOnZone.indices){
      this.firstNode = firstNode
      for(secondNode <- List(nextNode(firstNode),prevNode(firstNode))) {
        firstBeforeSecond = !firstBeforeSecond
        unLinkNode(this.firstNode, secondNode, true)
        if (internalExploration(secondNode)) {
          startIndice = nextNode(startIndice)
          return
        }
        linkNode(this.firstNode, secondNode, true)
      }
    }
  }

  def internalExploration(node:Int): Boolean ={

    def checkDelta(predsNode:Int, baseNode:Int,promisingNode:Int): Boolean ={
      if(currentDelta + (vrp.distanceFunction(baseNode,promisingNode) -
        vrp.distanceFunction(predsNode,baseNode)) < 0) {
        currentDelta += vrp.distanceFunction(baseNode, promisingNode) - vrp.distanceFunction(predsNode, baseNode)
        true
      }
      else
        false
    }

    //TODO : Do it smarter
    def isAllowed(node:Int, baseNode:Int, newNode:Int): Boolean ={
      var current = newNode
      do{
        if(oldLink(newNode) < newNode)
          current = nextNode(current)
        else
          current = prevNode(current)
      }
      while(availableNode(current) && current != baseNode && current != node)
      if(current == baseNode)
        false
      else {
        current = node
        while(availableNode(current) && current != baseNode){
          if(firstBeforeSecond)
            current = nextNode(current)
          else
            current = prevNode(current)
        }
        if(current == firstNode)
          false
        else
          true
      }
    }

    def getPossibleOldLink(pos:Int,newNode:Int):List[Int] = {
      val preds = prevNode(pos)
      val next = nextNode(pos)
      var res:List[Int] = List.empty
      if(availableNode(preds) && isAllowed(preds,pos,newNode))
        res = preds :: res
      if (availableNode(next) && isAllowed(next,pos,newNode))
        res = next :: res
      res
    }

    def isCycling(node:Int): Boolean ={

      var counter = 0

      def endSubSegment(n:Int): Int = {
        var current = n
        do{
          assert(counter < routeLength+1,"Arg ! \n" + oldLink.toList + "\n" + newLink.toList)
          if (oldLink(n) == prevNode(n))
          current = nextNode(current)
          else
          current = prevNode(current)
          counter += 1
        }while(availableNode(current))
        current
      }

      var currentNode = node
      var linkedNode = node
      do{
        currentNode = linkedNode
        currentNode = endSubSegment(currentNode)
        linkedNode = newLink(currentNode)
        counter += 1
      }while(currentNode != node && linkedNode != node)

      if(counter != routeLength)
        true
      else
        false
    }

    def getKPromisingNodes(node:Int): List[Int] ={
      val promisingNodes = vrp.closestNeighbors(node).iterator
      var counter = 0
      var res:List[Int] = Nil
      while(promisingNodes.hasNext && counter < k){
        val current = promisingNodes.next()
        val index = selectedRoute.indexOf(current)
        if(index != -1 && availableNode(index)) {
          res = index :: res
          counter += 1
        }
      }
      res
    }


    //val promisingNodes = vrp.kNearest(k,isAvailable(_,selectedRoute(node)))(selectedRoute(node))
    //we browse the promising nodes
    for(positionPN <- getKPromisingNodes(node)){
      //If the the length of the link cut is longer than the link we want to create we keep the move
      if(checkDelta(selectedRoute(oldLink(node)),selectedRoute(node),selectedRoute(positionPN))) {
        linkNode(node,positionPN,false)

        //We select the next node to proceed (either the prev or the next of the current node)
        for(oldNode <- getPossibleOldLink(positionPN,node)) {
          /*
            * If the new route create by linking the newNode and the first node is better than the old one
            * we keep the new route
           */
          unLinkNode(positionPN,oldNode,true)
          linkNode(oldNode,firstNode,false)
          if(!isCycling(oldNode)){
            encode(oldLink, newLink, selectedRoute, firstNode, availableNode)
            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
              return true
            }
          }
          /*
           * Else we check for other solution by searching deeper (we increase the level of the Lin-Kernighan algorithm)
           */
          unLinkNode(oldNode,firstNode,false)
          if(internalExploration(oldNode))
            return true
          //If we are here that means that no solution has been discovered by going this way so we backtrack
          linkNode(positionPN,oldNode,true)
        }
        unLinkNode(node,positionPN,false)
        currentDelta -=
          vrp.distanceFunction(selectedRoute(node), selectedRoute(positionPN)) -
            vrp.distanceFunction(selectedRoute(oldLink(node)), selectedRoute(node))
      }
    }
    false
  }

  override def instantiateCurrentMove(newObj: Int): LinKernighanMove = {
    LinKernighanMove(availableNode,selectedRoute,oldLink,newLink,firstNode,newObj,this,neighborhoodName)
  }

  def encode(oldLink:Array[Int],newLink:Array[Int],selectedRoute:Array[Int],firstNode:Int,availableNode:Array[Boolean]): Unit = {
    var currentNodeIndice = if(firstBeforeSecond)nextNode(nextNode(firstNode))else nextNode(firstNode)
    var segmentStartNodeIndice = vrp.N
    var oldNodeIndice = vrp.N
    var direction = 1
    var stop = false
    var toAssign:List[(Int,Int,Int)] = Nil
    var toReverseList:List[(Int,Int,Int)] = Nil
    //TODO : Find a better way to stop the while loop
    while (!stop){
      while (availableNode(currentNodeIndice)) {
        currentNodeIndice = (currentNodeIndice + direction + routeLength)%routeLength
      }
      if ((firstBeforeSecond && segmentStartNodeIndice == nextNode(firstNode))
        || (!firstBeforeSecond && segmentStartNodeIndice == firstNode))
        stop = true

      if(direction == -1) {
        //We don't get the value of the node : prevNode(currentNodeIndice)  because we need his position to order the list
        toReverseList = (prevNode(currentNodeIndice), selectedRoute(segmentStartNodeIndice), selectedRoute(oldNodeIndice))::toReverseList
      }
      else if(segmentStartNodeIndice != vrp.N){
        toAssign = (selectedRoute(segmentStartNodeIndice),selectedRoute(currentNodeIndice), selectedRoute(oldNodeIndice))::toAssign
      }

      /*
       * If a certain condition is met, the next segment must be reverse
       */

      if(oldLink(newLink(currentNodeIndice)) == routeLength-1 && newLink(currentNodeIndice) == 0)
      direction = 1
      else if(oldLink(newLink(currentNodeIndice)) == 0 && newLink(currentNodeIndice) == routeLength-1)
      direction = -1
      else if(oldLink(newLink(currentNodeIndice)) > newLink(currentNodeIndice))
      direction = -1
      else
      direction = 1
      /*
       * We assign the new value of each variable
       */
      oldNodeIndice = currentNodeIndice
      segmentStartNodeIndice = newLink(currentNodeIndice)
      currentNodeIndice = (newLink(currentNodeIndice)+direction+routeLength)%routeLength
    }

    toReverseList.sortWith(_._1 > _._1)
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

  override def reset(): Unit = {
    selectedRoute = vrp.getRouteOfVehicle(routeNr).toArray
    routeLength = selectedRoute.length
    availableNode = Array.tabulate(routeLength)(n => true)
    oldLink = Array.tabulate(routeLength)(n => vrp.N)
    newLink = Array.tabulate(routeLength)(n => vrp.N)
    currentDelta = 0
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
