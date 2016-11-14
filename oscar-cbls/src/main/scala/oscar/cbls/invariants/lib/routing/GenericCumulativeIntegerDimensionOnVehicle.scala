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


package oscar.cbls.invariants.lib.routing

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, ErrorChecker}

/**
  * Created by  Jannou Brohée on 3/10/16.
  */

object GenericCumulativeIntegerDimensionOnVehicle {

  /**
    * Implements a GenericCumulativeIntegerDimensionOnVehicle Invariant
    *
    * @param routes The sequence representing the route associated at each vehicle
    * @param n the maximum number of nodes
    * @param v the number of vehicles
    * @param op a function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
    * @return the capacity of each node in the sequence representinf the route associeted at each vehicle
    */
  def apply(routes:ChangingSeqValue,n:Int,v:Int,op :(Int,Int,Int)=>Int,initValue:Array[Int]):Array[CBLSIntVar] ={
    //TODO change routes.domain
    var output: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, Int.MinValue, routes.domain, "capacity at node("+node.toString+")"))
    new GenericCumulativeIntegerDimensionOnVehicle(routes, n, v, op, initValue,output)
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
class GenericCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, initValue :Array[Int], output:Array[CBLSIntVar])
  extends Invariant()
    with SeqNotificationTarget {
  require(initValue.length==v)
  require( output.length==n)

  var outputList: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, Int.MinValue, routes.domain, "capacity at node("+node.toString+")"))

  private var startPosOfVehicle : Array[Int]= Array.tabulate(v)(((car:Int)=> Int.MinValue)) // this will be updated by computeContentAndVehicleStartPositionsFromScratch
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- output) {
    i.setDefiningInvariant(this)
  }

  computeContentAndVehicleStartPositionsFromScratch(routes.newValue)



  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){

    val zoneToCompute = updateVehicleStartPositionsAndSearchZoneToUpdate(changes)
    zoneToCompute match {
      case null => computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
      case tree => for(car <- 0 until startPosOfVehicle.length)  {
        val lst = zoneToCompute.getOrElse(car,List.empty[(Int,Int)])
        if (lst.nonEmpty) updateContentForSelectedZones(routes.newValue,lst,startPosOfVehicle(car),car)
      }
    }


    this.checkInternals(new ErrorChecker())
  }


  private def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] ={
    //println("ADD ("+zoneStart+","+zoneEnd+") ")
    require(zoneStart<=zoneEnd)
    assert(list.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2).eq(list))
    list match{
      case Nil => (zoneStart,zoneEnd) :: list
      case (a,b)::tail =>
        if(zoneEnd>=a-1 && zoneEnd<=b) (Math.min(zoneStart,a), Math.max(zoneEnd,b)) :: tail
        else if (b>=zoneStart && a <=zoneStart)smartPrepend(a, Math.max(zoneEnd,b), tail)
         else if(b<zoneStart ) throw new Error("not sorted :( b:"+b+"zoneStart:"+zoneStart)
        else (zoneStart,zoneEnd)::list
    }
  }


  /**
    * Search the zones where changes occur following a SeqUpdate
    * @param changes the SeqUpdate
    * @return a list that specify mandatory zones which must be computed. A zone is represented by his start and end position : (startPositionIncluded, endPositionIncluded). Note that the list is sorted by position ((x,y) <= (x',y') iff y <= x'  )
    */
  private def updateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate) : RedBlackTreeMap[List[(Int,Int)]]= {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>
            //update vehciles pos
            val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,s.newValue, v)
            for(id<- car+1 until v) startPosOfVehicle.update(id,startPosOfVehicle(id)+1)
            val posOfTag = startPosOfVehicle(car)

            def shiftByOne(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail => smartPrepend(s.oldPosToNewPos(a+posOfTag).get-posOfTag,s.oldPosToNewPos(b+posOfTag).get-posOfTag, shiftByOne(tail))
              }
            }

            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match{
                case Nil => List((pos-posOfTag,(math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))-posOfTag))
                case (startZone, endZone) :: tail =>
                  val end = endZone + posOfTag
                  if(end < pos) smartPrepend(startZone,endZone,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tail))
                  else if(end == pos) smartPrepend(startZone,endZone+1,shiftByOne(tail))
                  else smartPrepend(pos-posOfTag,(math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1  else changes.newValue.size - 1)-posOfTag),shiftByOne(list))
              }
            }
            tree.insert(car,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tree.getOrElse(car,List.empty[(Int,Int)])) )
        }

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>
            val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
            for(id<- car+1 until v) startPosOfVehicle.update(id,startPosOfVehicle(id)-1)
            val posOfTag = startPosOfVehicle(car)
            def shiftByOne(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail => smartPrepend(r.oldPosToNewPos(a+posOfTag).get-posOfTag,r.oldPosToNewPos(b+posOfTag).get-posOfTag, shiftByOne(tail))
              }
            }

            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => if ((car != v - 1 && pos != startPosOfVehicle(car + 1)) || (car == v - 1 && (pos < changes.newValue.size))) List((pos-posOfTag, pos-posOfTag)) else list
                case (startZone, endZone) :: tail =>
                  val start = startZone + posOfTag
                  val end = endZone + posOfTag
                  if (end < pos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tail))
                  else if (end == pos) if(end>start) smartPrepend(startZone, r.oldPosToNewPos(end-1).get-startPosOfVehicle(car), shiftByOne(tail)) else  shiftByOne(tail)
                  else if (start == pos) if(end>start) smartPrepend(startZone, r.oldPosToNewPos(end-1).get-startPosOfVehicle(car), shiftByOne(tail)) else  shiftByOne(tail)
                  else smartPrepend(startZone, r.oldPosToNewPos(end).get-startPosOfVehicle(car), shiftByOne(tail))
              }
            }
            tree.insert(car, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tree.getOrElse(car,List.empty[(Int,Int)])))
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>

            val vehicleSource = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded,prev.newValue,v)
            val vehicleDestination =RoutingConventionMethods.searchVehicleReachingPosition(after,prev.newValue,v)
            val case1:Boolean = after<fromIncluded
            val delta = toIncluded - fromIncluded +1

            def insertInList(lst:List[(Int,Int)], toInsert:List[(Int,Int)]):List[(Int,Int)] ={
              toInsert match{
                case Nil => lst
                case (s,e)::tail =>
                  lst match{
                    case Nil =>  toInsert
                    case (start,end)::tail =>
                      if(s>end) (start,end) :: insertInList(lst.drop(1),toInsert)
                      else if(s==start && end == e) insertInList(lst,toInsert.drop(1))
                      else smartPrepend(s,e, insertInList(lst,toInsert.drop(1)))
                  }
              }
            }
            var toReinsertInTheDestinationSideList:List[(Int,Int)]=List.empty[(Int,Int)]

            def updateListOfZoneToUpdateAfterMove(internalTree:List[(Int,Int)],posOfTag:Int,sourceSide:Boolean=true):List[(Int,Int)]={
              internalTree match {
                case Nil => internalTree
                case (startZone, endZone) :: tail =>
                  val start = startZone + posOfTag
                  val end = endZone + posOfTag
                  if (sourceSide) {
                    if (end < fromIncluded) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail, posOfTag))
                    else if (start > toIncluded) smartPrepend(startZone - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail, posOfTag))
                    else {
                      toReinsertInTheDestinationSideList=  insertInList(toReinsertInTheDestinationSideList,List.apply((m.oldPosToNewPos(Math.max(start, fromIncluded)).get - m.oldPosToNewPos(startPosOfVehicle(vehicleDestination)).get,
                        m.oldPosToNewPos(Math.min(toIncluded, end)).get - m.oldPosToNewPos(startPosOfVehicle(vehicleDestination)).get)))
                      val toReturn = if (end > toIncluded) smartPrepend(((toIncluded + 1) - posOfTag) - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail, posOfTag)) else updateListOfZoneToUpdateAfterMove(tail, posOfTag)
                      if (start >= fromIncluded) toReturn
                      else smartPrepend(startZone, ((fromIncluded - 1) - posOfTag),toReturn)
                    }
                  }
                  else{
                    if (end <= after) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail,posOfTag,sourceSide))
                    else if(start>after) smartPrepend(startZone+delta,endZone+delta,updateListOfZoneToUpdateAfterMove(tail,posOfTag,sourceSide))
                    else  smartPrepend(startZone,after-posOfTag,smartPrepend(((after+1)-posOfTag)+delta,endZone+delta,updateListOfZoneToUpdateAfterMove(tail,posOfTag,sourceSide)))
                  }
              }
            }

            var tmp = tree.insert(vehicleSource,updateListOfZoneToUpdateAfterMove(tree.getOrElse(vehicleSource, List.empty[(Int,Int)]),startPosOfVehicle(vehicleSource)))
            tmp = tmp.insert(vehicleDestination,updateListOfZoneToUpdateAfterMove(tmp.getOrElse(vehicleDestination, List.empty[(Int,Int)]), startPosOfVehicle(vehicleDestination),false))

            // maj position des marqueur de vehicule
            if (vehicleSource!= vehicleDestination) {
              var car = if(case1) vehicleDestination+1 else vehicleSource+1
              while (car <= (if(case1) vehicleSource else vehicleDestination) ) {
                startPosOfVehicle.update(car, m.oldPosToNewPos(startPosOfVehicle(car)).get)
                car += 1
              }
            }
            val vehiculeOfSrc = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded-1,prev.newValue,v)
            var toReinsertInTheSourceSideList :List[(Int,Int)]= List.empty[(Int,Int)]
            if(case1) {
              var dst =  if (vehicleDestination != v - 1)  math.min(m.oldPosToNewPos(after+1).get, startPosOfVehicle(vehicleDestination + 1)-1) // juste pour savoir si
              else  math.min(m.oldPosToNewPos(after+1).get, routes.newValue.size-1)
              dst -= startPosOfVehicle(vehicleDestination)
              toReinsertInTheDestinationSideList =  if (flip) insertInList(toReinsertInTheDestinationSideList,List.apply((m.oldPosToNewPos(toIncluded).get-startPosOfVehicle(vehicleDestination), dst)))
              else insertInList(insertInList(toReinsertInTheDestinationSideList,List.apply((dst,dst)) ) , List.apply((m.oldPosToNewPos(fromIncluded).get-startPosOfVehicle(vehicleDestination), m.oldPosToNewPos(fromIncluded).get-startPosOfVehicle(vehicleDestination)) ))

              dst = if ((vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < startPosOfVehicle(vehiculeOfSrc + 1)) && toIncluded+1 <= routes.newValue.size-1) toIncluded+1
              // sinon on a rien a calculer
              else -1
              if(dst != -1){
                dst-= startPosOfVehicle(vehicleSource)
                toReinsertInTheSourceSideList = insertInList(toReinsertInTheSourceSideList,List.apply((dst,dst)))
              }
            } else {
              /* s'il n'y a pas de marqueur ou qu'on est pas sur un marqueur*/
              var dst = if (vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < startPosOfVehicle(vehiculeOfSrc + 1)) fromIncluded
              else -1 //sinon par de calcule

              if (dst != -1) {
                dst -= startPosOfVehicle(vehicleSource)
                toReinsertInTheSourceSideList = insertInList(toReinsertInTheSourceSideList,List.apply((dst,dst)))
                val vehiculeOfNodeFollowingAfter = RoutingConventionMethods.searchVehicleReachingPosition(after + 1,prev.newValue,v)
                dst = if ((after == routes.newValue.size - 1) || (vehiculeOfNodeFollowingAfter != vehicleDestination)) after else after + 1
                dst -= startPosOfVehicle(vehicleDestination)
                toReinsertInTheDestinationSideList = if (flip) insertInList(toReinsertInTheDestinationSideList,List.apply((m.oldPosToNewPos(toIncluded).get - startPosOfVehicle(vehicleDestination), dst)))
                else insertInList(insertInList(toReinsertInTheDestinationSideList, List.apply((dst, dst))), List.apply((m.oldPosToNewPos(after).get + 1 - startPosOfVehicle(vehicleDestination), m.oldPosToNewPos(after).get + 1 - startPosOfVehicle(vehicleDestination))))
              }
            }
            tmp = tmp.insert(vehicleSource,insertInList(tmp.getOrElse(vehicleSource, List.empty[(Int,Int)]),toReinsertInTheSourceSideList))
            tmp.insert(vehicleDestination,insertInList(tmp.getOrElse(vehicleDestination, List.empty[(Int,Int)]),toReinsertInTheDestinationSideList))
        }
      case SeqUpdateAssign(value : IntSequence) => null
      case SeqUpdateLastNotified(value:IntSequence) =>
        require (value quickEquals routes.value)
        RedBlackTreeMap.empty[List[(Int,Int)]]
      case s@SeqUpdateDefineCheckpoint(prev:SeqUpdate,_) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev)
      case u@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack)
    }
  }

  /**
    * Returns the capacity associated with a node.
    * @param nodeId the id of the node
    * @param outputInternal the array to consult
    * @return the capacity of the node
    */
  private def getRemainingCapacityOfNode(nodeId: Int, outputInternal : Array[CBLSIntVar]=output): Int = {
    outputInternal(nodeId).newValue
  }

  /**
    * Overridden the old capacity of a node by the new value.
    * @param currentNode the id of the node
    * @param valueOfCurrentNode the new capacity associated with the node
    * @param outputInternal the array containing the capacity to override
    */
  private def setRemainingCapacityOfNode(currentNode: Int, valueOfCurrentNode: Int, outputInternal : Array[CBLSIntVar] = output): Unit = {
    outputInternal(currentNode) := valueOfCurrentNode
  }

  /**
    * Computes the capacity of each node from scratch
    * @param s sequence of Integers representing the routes
    * @param outputInternal the array containing the capacitys
    * @param startPosOfVehiculeInternal the array which store, for any vehicles, the position of the starting node of the vehicle
    */
  def computeContentAndVehicleStartPositionsFromScratch(s:IntSequence, outputInternal: Array[CBLSIntVar] = output, startPosOfVehiculeInternal : Array[Int]=startPosOfVehicle) {
    var current = s.explorerAtPosition(0).get
    var currentCar = current.value
    startPosOfVehiculeInternal(currentCar) =  current.position
    var valueOfCurrentNode =  initValue(current.value)
    setRemainingCapacityOfNode(current.value, valueOfCurrentNode, outputInternal)
    while(!current.next.isEmpty){
      val previous = current
      val valueOfPresiousNode = valueOfCurrentNode
      current = current.next.get
      if(current.value < v) {
        //sauver valeur du dernier node du vehicule
        currentCar = current.value
        startPosOfVehiculeInternal(currentCar) = current.position
        valueOfCurrentNode = initValue(current.value)
      }else valueOfCurrentNode =  op(previous.value,current.value,valueOfPresiousNode) //sinon on continue sur le meme vehicule
      setRemainingCapacityOfNode(current.value, valueOfCurrentNode,outputInternal )
    }
  }

  /**
    * Computes the capacity of nodes concerned by a SeqUpdate
    * @param s the sequence after the SeqUpdate
    * @param lst the list containing positions where calculations must be performed
    */
  def updateContentForSelectedZones(s:IntSequence, lst: List[(Int, Int)],posOfTag:Int,car:Int){
    /*
    val iter = lst.toIterator
    var nextZone = iter.next()
    var start = 0
    var end = 0

    def computePositionOfZone(): Unit ={
      start = nextZone._1 + posOfTag
      end = nextZone._2 + posOfTag
    }

    var cdt:Boolean =  false
    def takeNextZone(): Unit ={nextZone = if(iter.hasNext)  iter.next() else null}
    var upperBound =  if (car != v - 1) startPosOfVehicle(car + 1) else routes.newValue.size
    computePositionOfZone()
    var current = s.explorerAtPosition(start).get// first is mandatory ...
    def checkIfNextZoneAndUpdate(): Unit ={
      if((cdt || current.position==upperBound-1 ) && nextZone != null){// si on peut arreter pour cette zone ==> recup zone suivant sinon on fait rien et on continue
        cdt = false
        computePositionOfZone()
        current= s.explorerAtPosition( Math.max(start-1,current.position)).get
        takeNextZone
      }
    }

    var previousPos = s.explorerAtPosition(startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position))).get
    var valueOfPreviousNode=initValue(previousPos.value)// valeur du noeud précédent

    if(current.position > previousPos.position) {// maj noeud precedent s'il existe
      previousPos = s.explorerAtPosition(current.position-1).get
      valueOfPreviousNode = getRemainingCapacityOfNode(previousPos.value) }

    var valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)// calcule  valeur du noeud
    var oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)

    if(current.position==end ) cdt = valueOfCurrentNode==oldValueOfCurrentNode
    if(!cdt) setRemainingCapacityOfNode(current.value, valueOfCurrentNode)//maj capa

    if(current.position==end ){ // si fin de zone et qu'il en reste encore
      checkIfNextZoneAndUpdate()
    }

    // tant qu'on doit continuer et qu'on depasse pas le vehicule des noeud qu'on veut maj
    while(!cdt && current.position<upperBound-1 ){
      previousPos=current// maj noeud precedent
      current = current.next.get//maj noeud courant

      if(current.position <= end){// tant qu'on est dans la zone mandat
        valueOfPreviousNode=getRemainingCapacityOfNode(previousPos.value)
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)
        setRemainingCapacityOfNode(current.value,valueOfCurrentNode)
        if(current.position==end ){
          cdt=  valueOfCurrentNode==oldValueOfCurrentNode
          checkIfNextZoneAndUpdate()
        }
      }
      else{ //sinon
        if(nextZone != null && current.position >=nextZone._1 +posOfTag && current.position ==nextZone._2 +posOfTag) takeNextZone // si je depasse deja la prochaine nextZone de pos je recupère la suivante

        valueOfPreviousNode = valueOfCurrentNode
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)
        cdt = valueOfCurrentNode==oldValueOfCurrentNode //verif cond ( on est plus dans la zone mandat donc dès que cdt ==> on arrete (ou on passe a la zone suivant s'il y en a )
        if(!cdt) setRemainingCapacityOfNode(current.value,valueOfCurrentNode)// maj capa si on doit la changer
        checkIfNextZoneAndUpdate()
      }
    }
  }

*/
    val iter = lst.toIterator
    var nextZone = iter.next()
    var start = 0
    var end = 0

    def computePositionOfZone(): Unit ={
      start = nextZone._1 + posOfTag
      end = nextZone._2 + posOfTag
    }
    def takeNextZone(): Unit ={nextZone = if(iter.hasNext)  iter.next() else null}


    computePositionOfZone

    var current = s.explorerAtPosition(start).get// first is mandatory ...
    var upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position) != v-1) // limite sup pour le noeud
      startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position)+1)
    else routes.newValue.size
    var previousPos = s.explorerAtPosition(startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position))).get // recup noeud précédent
    var valueOfPreviousNode=initValue(previousPos.value)// valeur du noeud précédent

    if(current.position > previousPos.position) { // maj noeud precedent s'il existe
      previousPos = s.explorerAtPosition(current.position-1).get
      valueOfPreviousNode = getRemainingCapacityOfNode(previousPos.value) }

    var valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode) // calcule  valeur du noeud
    var oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)

    var cdt:Boolean = if(current.position==end ) /* si j'ai (deja) finis  ==> verif cdt */ valueOfCurrentNode==oldValueOfCurrentNode else  false

    setRemainingCapacityOfNode(current.value, valueOfCurrentNode)//maj capa

    if(current.position==end && nextZone != null){// si fin de zone et qu'il en reste encore
      if((cdt || current.position==upperBound-1 )){// si on peut arreter pour cette zone ==> recup zone suivant sinon on fait rien et on continue
        cdt = false
        computePositionOfZone
        current= s.explorerAtPosition(start-1).get // if(start >= current.position) newRoute.explorerAtPosition(start-1).get else current
        upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
          startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
        else routes.newValue.size
        nextZone= if(iter.hasNext)  /* recupere nextZone suivant si elle existe*/ iter.next() else null
      }
    }

    while(!cdt && current.position<upperBound-1 ){  // tant qu'on doit continuer et qu'on depasse pas le vehicule des noeud qu'on veut maj
      previousPos=current // maj noeud precedent
      current = current.next.get //maj noeud courant

      if(current.position <= end){
        valueOfPreviousNode=getRemainingCapacityOfNode(previousPos.value)
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)
        setRemainingCapacityOfNode(current.value,valueOfCurrentNode)
        if(current.position==end ){
          cdt=  valueOfCurrentNode==oldValueOfCurrentNode
          if((cdt || current.position==upperBound-1 ) && nextZone != null){
            cdt = false
            computePositionOfZone
            current= s.explorerAtPosition(start-1).get
            upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
              startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
            else routes.newValue.size
            takeNextZone
          }
        }
      }
      else{ //sinon
        if(nextZone != null && current.position >=nextZone._1 + posOfTag && current.position ==nextZone._2+ posOfTag ){ // si je depasse deja la prochaine nextZone de pos je recupère la suivante
          takeNextZone
        }
        valueOfPreviousNode = valueOfCurrentNode
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)

        cdt = valueOfCurrentNode==oldValueOfCurrentNode //verif cond ( on est plus dans la zone mandat donc dès que cdt ==> on arrete (ou on passe a la zone suivant s'il y en a )
        setRemainingCapacityOfNode(current.value,valueOfCurrentNode)

        if((cdt || current.position==upperBound-1 ) && nextZone != null){  //Si je peut arreter mais que j'ai encore des nextZone ==> maj
          cdt = false
          computePositionOfZone
          current=  if(start-1 >= current.position) s.explorerAtPosition(start-1).get else current
          upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
            startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
          else routes.newValue.size
          takeNextZone
        }
      }
    }
  }

  override def checkInternals(c: Checker): Unit = {
    var outputCheck: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(new Store(), Int.MinValue, routes.domain, "capacity at this f**king piece of cr*p of node :D ("+node.toString+")"))
    var startPosOfVehiculeCheck : Array[Int]= Array.tabulate(v)(((car:Int)=> 0))
    computeContentAndVehicleStartPositionsFromScratch(routes.newValue, outputCheck,startPosOfVehiculeCheck)
    for(car <- 0 until v){
      c.check(startPosOfVehiculeCheck(car) equals startPosOfVehicle(car ), Some("Founded start of car(" + car + "):=" + startPosOfVehicle(car) + " should be :=" + startPosOfVehiculeCheck(car)+" seq :"+routes.newValue.mkString(",")))

    }
    for (node <- routes.newValue) {
      c.check(getRemainingCapacityOfNode(node,outputCheck) equals getRemainingCapacityOfNode(node), Some("Founded Capacity at node(" + node + ") pos ("+routes.newValue.positionsOfValue(node).firstKey +"):=" + getRemainingCapacityOfNode(node) + " should be :=" + getRemainingCapacityOfNode(node,outputCheck)+" seq :"+routes.newValue.mkString(",")))
    }
  }

}