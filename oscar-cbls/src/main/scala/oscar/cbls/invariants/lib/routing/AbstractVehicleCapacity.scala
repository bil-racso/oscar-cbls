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

import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.{IntSequence, IntSequenceExplorer}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, SchedulingHandler}


/**
  * Created by  Jannou Brohée on 18/11/16.
  */


/**
  * Maintains the content of vehicles at each node and the starting position of each vehicle
  * @param routes The sequence representing the route associated at each vehicle
  * @param n The maximum number of nodes
  * @param v The number of vehicles
  * @param initInt The initial capacity of a node when it is not yet in the route
  * @param initValue An array giving the initial capacity of a vehicle at his starting node (0, v-1)
  * @param op A function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
  */
abstract class AbstractVehicleCapacity(routes:ChangingSeqValue, n:Int, v:Int, initInt:Int, initValue:Array[Int], op :(Int,Int,Int)=>Int )
  extends Invariant()   {

  /**
    * Returns the capacity associated with a node.
    * @param nodeId the id of the node
    * @param save false to return the capacity save at the previous checpoint, true to return the capacity calculated at the last movement
    * @return the capacity of the node
    */
  def  getVehicleContentAtNode(nodeId: Int): Int //

  /**
    * Overridden the old capacity of a node by the new value.
    * @param currentNode the id of the node
    * @param valueOfCurrentNode the new capacity associated with the node
    * @param save false to override the current capacity, true to save the current capacity
    */
  def setVehicleContentAtNode(currentNode: Int, currentPosition : Int, valueOfCurrentNode:Int): Unit

  /**
    * Returns the position of the first node associated with the given vehicle
    * @param vehicle the concerned vehicle
    * @return the position of the first node associated with the vehicle
    */
  def positionOfVehicle(vehicle:Int): Int

  /**
    * Returns the vehicle reaching the node at the given position
    * @param position the position of the concerned node
    * @return the vehicle reaching the position
    */
  def vehicleReachingPosition(position:Int):Int

  /**
    *Computes content of vehicle and their starting position from scratch
    * @param s the sequence
    * @return (contentAtnode,VehicleLocation)
    */
  def computeContentAndVehicleStartPositionsFromScratch(s:IntSequence):(Array[Int],VehicleLocation) = {
    var current = s.explorerAtPosition(0).get
    var currentCar = current.value
    var tmpVehicleLocation : Array[Int]=Array.tabulate(v)(((car:Int)=> 0))
    var tmpCapacity : Array[Int]=Array.tabulate(n)(((node:Int)=> initInt))
    tmpVehicleLocation(currentCar) = current.position
    var valueOfCurrentNode =  contentAtStartingNodeOfVehicle(current.value)
    tmpCapacity(current.value)= valueOfCurrentNode
    while(!current.next.isEmpty){
      val previous = current
      val valueOfPresiousNode = valueOfCurrentNode
      current = current.next.get
      if(current.value < v){
        currentCar = current.value
        tmpVehicleLocation(currentCar) = current.position
        valueOfCurrentNode = contentAtStartingNodeOfVehicle(current.value)
      } else valueOfCurrentNode =  op(previous.value,current.value,valueOfPresiousNode)
      tmpCapacity(current.value)= valueOfCurrentNode
    }
    (tmpCapacity,ConcreteVehicleLocation(tmpVehicleLocation))
  }

  /**
    *
    * @param zoneStart
    * @param zoneEnd
    * @param list
    * @return
    *        @author renaud.delandtsheer@cetic.be
    */
  def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] ={
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
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the insert
    * @param map
    * @param s
    * @param changes
    * @return
    */
  def updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastInsert(map: RedBlackTreeMap[List[(Int, Int)]], s: SeqUpdateInsert, changes:SeqUpdate):RedBlackTreeMap[List[(Int, Int)]] = {
    map match {
      case null => null
      case tree =>
        pushOnTopOfStack((posit) => s.oldPosToNewPos(posit))
        val car = vehicleReachingPosition(s.pos)
        val posOfTag = positionOfVehicle(car)

        def shiftPositionsAfterTheInsertPos(list: List[(Int, Int)], insertPosIsNotStartPos: Int = 1): List[(Int, Int)] = {
          list match {
            case Nil => list
            case (a, b) :: tail => (a + insertPosIsNotStartPos, b + 1) :: shiftPositionsAfterTheInsertPos(tail)
          }
        }

        val relativePos = s.pos - posOfTag
        def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list: List[(Int, Int)]): List[(Int, Int)] = {
          list match {
            case Nil => List((relativePos, (math.min(s.pos + 1, if (car != v - 1) positionOfVehicle(car + 1) - 1 else changes.newValue.size - 1)) - posOfTag))
            case (startZone, endZone) :: tail =>
              if (endZone < relativePos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tail))
              else if (endZone == relativePos) smartPrepend(startZone, endZone + 1, shiftPositionsAfterTheInsertPos(tail))
              else smartPrepend(relativePos, (math.min(s.pos + 1, if (car != v - 1) positionOfVehicle(car + 1) - 1 else changes.newValue.size - 1) - posOfTag), shiftPositionsAfterTheInsertPos(list, if (startZone < relativePos && endZone > relativePos) 0 else 1))
          }
        }
        tree.insert(car, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tree.getOrElse(car, List.empty[(Int, Int)])))
    }
  }

  /**
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the remove
    * @param map
    * @param r
    * @param changes
    * @return
    */
  def updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastRemove(map: RedBlackTreeMap[List[(Int, Int)]], r: SeqUpdateRemove, changes:SeqUpdate): RedBlackTreeMap[List[(Int, Int)]] = {
    map match{
      case null => null
      case tree =>
        setVehicleContentAtNode(r.removedValue , r.position,initInt)
        val car = vehicleReachingPosition(r.position)
        pushOnTopOfStack((posit)=>r.oldPosToNewPos(posit))
        val relativePos = r.position - positionOfVehicle(car)

        def shiftByOneToLeft(list:List[(Int,Int)]):List[(Int,Int)]= {
          list match {
            case Nil => list
            case (a, b) :: tail => (a - 1, b - 1) :: shiftByOneToLeft(tail)
          }
        }

        def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list:List[(Int,Int)]):List[(Int,Int)]= {
          list match {
            case Nil => if ((car != v - 1 && r.position != positionOfVehicle(car + 1)) || (car == v - 1 && (r.position < changes.newValue.size)))
              List((relativePos, relativePos))
            else list
            case (startZone, endZone) :: tail =>
              if (endZone < relativePos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tail))
              else if (endZone >= relativePos && startZone <= relativePos)
                if(endZone >startZone ) smartPrepend(startZone, endZone-1, shiftByOneToLeft(tail)) else { if ((car != v - 1 && r.position != positionOfVehicle(car + 1)) || (car == v - 1 && (r.position < changes.newValue.size))) (startZone, endZone) :: shiftByOneToLeft(tail) else shiftByOneToLeft(tail)}
              else smartPrepend(relativePos, relativePos, shiftByOneToLeft(list))
          }
        }
        tree.insert(car, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tree.getOrElse(car,List.empty[(Int,Int)])))

    }
  }

  /**
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the move
    * @param map
    * @param m
    * @param changes
    * @return
    */
  def updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastMove(map: RedBlackTreeMap[List[(Int, Int)]], m: SeqUpdateMove, changes: SeqUpdate): RedBlackTreeMap[List[(Int, Int)]] = {
    map match{
      case null => null
      case tree =>
        val vehicleSource = vehicleReachingPosition(m.fromIncluded)
        val vehicleDestination =vehicleReachingPosition(m.after)
        val vehiculeOfSrc = vehicleReachingPosition(m.fromIncluded-1)
        val vehiculeOfNodeFollowingAfter = vehicleReachingPosition(m.after+1)
        val isMovedToLeft:Boolean = m.after<m.fromIncluded
        val relativeFromIncluded = m.fromIncluded - positionOfVehicle(vehicleSource)
        val relativeToIncluded = m.toIncluded -positionOfVehicle(vehicleSource)
        val relativeAfter = m.after - positionOfVehicle(vehicleDestination)
        val delta = m.toIncluded - m.fromIncluded +1
        val newAfter = m.oldPosToNewPos(m.after).get
        val beforeFrom = m.fromIncluded-1
        val newBeforeFrom = m.oldPosToNewPos(beforeFrom).get
        var toReinsertInTheOtherSide= List.empty[(Int,Int)]

        def insertInList(lst:List[(Int,Int)], toInsert:List[(Int,Int)]):List[(Int,Int)] ={
          toInsert match{
            case Nil => lst
            case (s,e)::tail =>
              lst match{
                case Nil =>  toInsert
                case (start,end)::tail =>
                  if(s>end) smartPrepend(start,end, insertInList(lst.drop(1),toInsert))
                  else if(s==start && end == e) insertInList(lst,toInsert.drop(1))
                  else smartPrepend(s,e, insertInList(lst,toInsert.drop(1)))
              }
          }
        }

        def updateListOfZoneToUpdateAfterMove(listOfZonesForVehicle:List[(Int,Int)],sourceSide:Boolean=true):List[(Int,Int)]={
          listOfZonesForVehicle match {
            case Nil => listOfZonesForVehicle
            case (startZone, endZone) :: tail =>
              if (sourceSide) {// traitement du coté [from,to]
                if (endZone < relativeFromIncluded) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail)) // on avance
                else if (startZone > relativeToIncluded) smartPrepend(startZone - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail))// on decale
                else {// on gere les deplacement due au mouvement
                  if(!m.flip)  toReinsertInTheOtherSide = (Math.max(startZone, relativeFromIncluded)-relativeFromIncluded,Math.min(relativeToIncluded, endZone)-relativeFromIncluded) ::toReinsertInTheOtherSide
                  val toReturn = if (endZone > relativeToIncluded) smartPrepend(relativeToIncluded+1 - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail))
                  else updateListOfZoneToUpdateAfterMove(tail)
                  if (startZone >= relativeFromIncluded) toReturn
                  else smartPrepend(startZone, relativeFromIncluded - 1,toReturn)
                }
              }
              else{// traitement coté after
                if (endZone <= relativeAfter) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail,sourceSide))// on avance
                else if(startZone>relativeAfter) smartPrepend(startZone+delta,endZone+delta,updateListOfZoneToUpdateAfterMove(tail,sourceSide))// on decale
                else  smartPrepend(startZone,relativeAfter,smartPrepend(relativeAfter+1+delta,endZone+delta,updateListOfZoneToUpdateAfterMove(tail,sourceSide)))// si la zone contient after+1
              }
          }
        }

        var tmp = tree.insert(vehicleSource,updateListOfZoneToUpdateAfterMove(tree.getOrElse(vehicleSource, List.empty[(Int,Int)])))
        tmp=  tmp.insert(vehicleDestination,updateListOfZoneToUpdateAfterMove(tmp.getOrElse(vehicleDestination, List.empty[(Int,Int)]),false))//, startPosOfVehicle(vehicleDestination)

        pushOnTopOfStack((pos)=>m.oldPosToNewPos(pos))
        val newRelativeAfter = newAfter - positionOfVehicle(vehicleDestination)
        var toReinsertInTheDestinationSideList:List[(Int,Int)]=List.empty[(Int,Int)]

        for(elt <- toReinsertInTheOtherSide) toReinsertInTheDestinationSideList = ((newRelativeAfter+1+elt._1), (newRelativeAfter+1+elt._2)) :: toReinsertInTheDestinationSideList

        if(isMovedToLeft) {
          var dst =  if (vehicleDestination != v - 1)  math.min(m.oldPosToNewPos(m.after+1).get, positionOfVehicle(vehicleDestination + 1)-1)-positionOfVehicle(vehicleDestination) // juste pour savoir si
          else  math.min(m.oldPosToNewPos(m.after+1).get, routes.newValue.size-1)-positionOfVehicle(vehicleDestination)
          toReinsertInTheDestinationSideList =  if (m.flip) insertInList(toReinsertInTheDestinationSideList, List.apply((m.oldPosToNewPos(m.toIncluded).get-positionOfVehicle(vehicleDestination), dst)))
          else insertInList(insertInList(toReinsertInTheDestinationSideList,List.apply((dst,dst)) ) , List.apply((m.oldPosToNewPos(m.fromIncluded).get-positionOfVehicle(vehicleDestination), m.oldPosToNewPos(m.fromIncluded).get-positionOfVehicle(vehicleDestination)) ))
          dst = if ((vehiculeOfSrc == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < positionOfVehicle(vehiculeOfSrc + 1)) && m.toIncluded+1 <= routes.newValue.size-1) m.toIncluded+1  -positionOfVehicle(vehicleSource) else -1
          if(dst != -1) tmp = tmp.insert(vehicleSource,insertInList(tmp.getOrElse(vehicleSource, List.empty[(Int,Int)]),List.apply((dst,dst))))
        }
        else {
          var dst = if (vehiculeOfSrc == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < positionOfVehicle(vehiculeOfSrc + 1)) m.fromIncluded else -1
          dst-= positionOfVehicle(vehicleSource)
          if(dst > -1)   tmp = tmp.insert(vehicleSource,insertInList(tmp.getOrElse(vehicleSource, List.empty[(Int,Int)]),List.apply((dst,dst))))
          dst = if((m.after == routes.newValue.size-1) || ( vehiculeOfNodeFollowingAfter!=vehicleDestination ))  newRelativeAfter+delta else newRelativeAfter+delta+1
          toReinsertInTheDestinationSideList = if (m.flip) insertInList(toReinsertInTheDestinationSideList,List.apply((newRelativeAfter+1, dst)))
          else insertInList(insertInList(toReinsertInTheDestinationSideList, List.apply((dst, dst))), List.apply((newRelativeAfter+1,newRelativeAfter+1)))
        }
        tmp.insert(vehicleDestination,insertInList(tmp.getOrElse(vehicleDestination, List.empty[(Int,Int)]),toReinsertInTheDestinationSideList))
    }
  }

  /**
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after last notified
    * @param value
    * @return
    */
  def updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastNotified(value: IntSequence): RedBlackTreeMap[List[(Int, Int)]] = {
    require (value quickEquals routes.value)
    RedBlackTreeMap.empty[List[(Int,Int)]]
  }

  /**
    * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the last assign
    * @param value
    * @return
    */
  def UpdateVehicleStartPositionsAndSearchZoneToUpdateAfterAssign(value: IntSequence): RedBlackTreeMap[List[(Int, Int)]] = null

  /**
    * Computes the capacity of nodes concerned by a SeqUpdate
    * @param s the sequence after the SeqUpdate
    * @param lst the list containing positions where calculations must be performed
    */
  def updateContentForSelectedZones(s:IntSequence, lst: List[(Int, Int)],posOfTag:Int,car:Int){
    /*
               Initialisation of variable and def methods
                                                                  */
    var upperBound =  if (car != v - 1) positionOfVehicle(car + 1) else routes.newValue.size//upper is start of next vehicle OR end of seq
    val iter = lst.toIterator
    var nextZone : (Int,Int) = null
    var startOfCurrentZone = 0
    var endOfCurrentZone = 0
    var valueOfCurrentNode =0
    var oldValueOfCurrentNode =0
    var valueOfPreviousNode=0
    var previousPos:IntSequenceExplorer = null
    def updateStartAndEndOfZone(): Unit ={
      startOfCurrentZone = nextZone._1 + posOfTag
      endOfCurrentZone = nextZone._2 + posOfTag
    }
    def takeNextZone(): Unit ={nextZone = if(iter.hasNext)  iter.next() else null}
    takeNextZone()// take first Zone
    updateStartAndEndOfZone()
    takeNextZone()
    var current = s.explorerAtPosition(startOfCurrentZone).get
    var haveToVisitNextNode:Boolean = true
    def  checkIfHaveToVisitNextNode(): Unit ={ haveToVisitNextNode = oldValueOfCurrentNode!=valueOfCurrentNode}
    def updateZoneToVisit(): Unit ={
      if((!haveToVisitNextNode || current.position==upperBound-1 ) && nextZone != null){// si on peut arreter pour cette zone ==> recup zone suivant sinon on fait rien et on continue
        haveToVisitNextNode = true
        updateStartAndEndOfZone()
        current= s.explorerAtPosition( Math.max(startOfCurrentZone-1,current.position)).get
        takeNextZone()
      }
    }
    /*

               And now, here's how to do the job
                                                                */
    // handle first node
    previousPos = s.explorerAtPosition(positionOfVehicle(vehicleReachingPosition(current.position))).get
    valueOfCurrentNode = contentAtStartingNodeOfVehicle(car)
    oldValueOfCurrentNode = getVehicleContentAtNode(current.value)


    if(current.position > previousPos.position) {// maj noeud precedent s'il existe
      previousPos = s.explorerAtPosition(current.position-1).get
      valueOfPreviousNode = getVehicleContentAtNode(previousPos.value)
      valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)// maj  valeur du noeud
    }
    /*  <  */ setVehicleContentAtNode(current.value,current.position, valueOfCurrentNode)//maj capa
    if(current.position==endOfCurrentZone ){
      checkIfHaveToVisitNextNode()
      updateZoneToVisit()
    } /*  > */                        //  who to put this into the while ? ( current.position <= endOfCurrentZone)

    while(haveToVisitNextNode && current.position<=upperBound-2 ){
      previousPos=current// maj noeud precedent
      current = current.next.get
      oldValueOfCurrentNode = getVehicleContentAtNode(current.value)
      if(current.position <= endOfCurrentZone){// tant qu'on est dans la zone mandat
        valueOfPreviousNode=getVehicleContentAtNode(previousPos.value)
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        setVehicleContentAtNode(current.value,current.position, valueOfCurrentNode)
        if(current.position==endOfCurrentZone ){
          checkIfHaveToVisitNextNode()
          updateZoneToVisit()
        }
      } else{ // on sort de la zone car la propagation du changement n'est pas terminé
        if(nextZone != null && current.position >=nextZone._1 +posOfTag && current.position ==nextZone._2 +posOfTag) takeNextZone() // si je depasse deja la prochaine nextZone de pos je recupère la suivante
        valueOfPreviousNode = valueOfCurrentNode
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        checkIfHaveToVisitNextNode() //verif cond ( on est plus dans la zone mandat donc dès que haveToVisitNextNode ==> on arrete (ou on passe a la zone suivant s'il y en a )
        if(haveToVisitNextNode) setVehicleContentAtNode(current.value,current.position, valueOfCurrentNode)// maj capa si on doit la changer
        updateZoneToVisit()
      }
    }
  }

  /**
    * Update the stack of vehicles location
    * @param oldToNewfunction  a function representing the change of vehicles location
    */
  def pushOnTopOfStack(oldToNewfunction:(Int)=> Option[Int]) :Unit

  def contentAtStartingNodeOfVehicle(vehicle:Int): Int = initValue(vehicle)

  override def checkInternals(c: Checker): Unit = {
    val (capaToChekc, stackToChekc) = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
    for(node <- 0 until n) c.check(capaToChekc(node) equals getVehicleContentAtNode(node), Some("Founded Capacity at node(" + node + ") at pos : "+ routes.newValue.positionsOfValue(node)+ " :=" + getVehicleContentAtNode(node) + " should be :=" + capaToChekc(node)))
    for(car <- 0 until v)c.check(stackToChekc.posOfVehicle(car) equals positionOfVehicle(car ), Some("Founded start of car(" + car + "):=" + positionOfVehicle(car) + " should be :=" + stackToChekc.posOfVehicle(car)+" seq :"+routes.newValue.mkString(",")))
  }
}

