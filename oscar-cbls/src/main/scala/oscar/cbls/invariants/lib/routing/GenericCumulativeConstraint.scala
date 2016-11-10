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

import oscar.cbls.algo.boolArray.MagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
/**
  * Created by  Jannou Brohée on 17/10/16.
  */

object GenericCumulativeConstraint {

  /**
    * Implements a GenericCumulativeConstraint Invariant
    *
    * @param routes The sequence representing the route associated at each vehicle
    * @param n the maximum number of nodes
    * @param v the number of vehicles
    * @param op a function which returns the capacity change between two nodes
    * @param cMax  threshold
    * @return a tuple containing the global violation and the violation per vehicle
    */

  def apply(routes:ChangingSeqValue,n:Int,v:Int,op :(Int,Int,Int)=>Int, cMax : Int, initValue :Array[Int]):(CBLSIntVar,Array[CBLSIntVar]) ={
    var violPerV: Array[CBLSIntVar] = Array.tabulate(v)((car: Int) => CBLSIntVar(routes.model, 0, routes.domain, "violation for vehicle("+car.toString+")"))
    var globalViol:CBLSIntVar=  CBLSIntVar(routes.model,0,routes.domain,"Global Violation of Capacity")
    globalViol= new GenericCumulativeConstraint(routes,n,v,op,cMax,initValue,globalViol,violPerV).GlobalViolation
    (globalViol,violPerV)
  }
}

/**
  * Implements a GenericCumulativeConstraint Invariant
  * Compute the exceedation of the threshold by the op function for a vehicle at a node ( done for each node of each vehicle)
  *
  * @param routes The sequence representing the route associated at each vehicle
  * @param n the maximum number of nodes
  * @param v the number of vehicles
  * @param op a function which returns the capacity change between two nodes
  * @param cMax threshold
  * @param initValue an array giving the initial capacity of a vehicle at his starting node (0, v-1)
  */
class GenericCumulativeConstraint(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, cMax : Int, initValue :Array[Int], globalViolation: CBLSIntVar, vehicleViolation:Array[CBLSIntVar])
  extends Invariant()  with SeqNotificationTarget {

  require(initValue.length==v)
  private var capacityAtNode : Array[Int]= Array.tabulate(n)((node:Int)=> 0)
  private var changedCapacityAtNode : Array[Int]= Array.tabulate(n)((node:Int)=> 0)

  private var changedNodeSinceCheckpoint:MagicBoolArray= MagicBoolArray(n)
  private var changedVehicleViolationSinceCheckpoint:MagicBoolArray= MagicBoolArray(v)
  private var changedPositionOfTagfVehicleSinceCheckpoint:MagicBoolArray= MagicBoolArray(v)

  private var changedGlobalViolation:CBLSIntVar=  CBLSIntVar(routes.model,0,routes.domain,"changed Global Violation of Capacity")

  private var chk : Boolean = false
  private var chkV : Boolean = false

  private var positionOfTagVehicule : Array[Int]= Array.tabulate(v)((car:Int)=> Int.MinValue)
  private var changedPositionOfTagVehicule : Array[Int]= Array.tabulate(v)((car: Int) => car)

  private var changedVehicleViolation: Array[CBLSIntVar] = Array.tabulate(v)((car: Int) => CBLSIntVar(routes.model, 0, routes.domain, "changed violation for vehicle("+car.toString+")"))

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  globalViolation.setDefiningInvariant(this)

  computeFromScratch(routes.newValue)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){

    val impactedZone = searchZoneToCompute(changes)
    impactedZone match {
      case null =>
        computeFromScratch(routes.newValue)
      case  list => if (impactedZone.nonEmpty)computeViolationFromSomWhere(routes.newValue,impactedZone)
    }
    //this.checkInternals(new ErrorChecker())
  }



  /**
    *
    * @param zoneStart
    * @param zoneEnd
    * @param list
    * @return
    *        @author renaud.delandtsheer@cetic.be
    */
  private def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] ={
    // println("ADD ("+zoneStart+","+zoneEnd+") ")
    require(zoneStart<=zoneEnd)
    assert(list.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2).eq(list))
    list match{
      case Nil => (zoneStart,zoneEnd) :: list
      case (a,b)::tail =>
        if(zoneEnd>=a-1 && zoneEnd<=b) {
          (Math.min(zoneStart,a), Math.max(zoneEnd,b)) :: tail
        }else if (b>=zoneStart && a <=zoneStart){
          smartPrepend(a, Math.max(zoneEnd,b), tail)
        } else if(b<zoneStart ) {
          throw new Error("not sorted :( b:"+b+"zoneStart:"+zoneStart)
        }else {
          // a > end
          (zoneStart,zoneEnd)::list
        }
    }
  }



  /**
    * Search the zones where changes occur following a SeqUpdate
    * @param changes the SeqUpdate
    * @return a list of pairs of positions.
    */


  private def searchZoneToCompute(changes:SeqUpdate) :  List[(Int,Int)] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        searchZoneToCompute(prev) match{
          case null => null
          case list =>
            for(id<- RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s.newValue,pos)+1 until v) recordChangeOfVehicleTag(id,startPosOfVehicle(id)+1)
            val car = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(changes.newValue,pos)
            def shiftByOne(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail => smartPrepend(s.oldPosToNewPos(a).get,s.oldPosToNewPos(b).get, shiftByOne(tail))
              }
            }
            /**
              *
              * @param list
              * @return
              *         @author renaud.delandtsheer@cetic.be
              */
            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match{
                case Nil =>
                  List((pos,math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1)))
                case (a,b) :: tail =>
                  if(b < pos) smartPrepend(a,b,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tail))
                  else if(b == pos) smartPrepend(a,b+1,shiftByOne(tail))
                  else /*b est après pos*/ smartPrepend(pos,math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1),shiftByOne(list))
              }
            }

            updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list)



        }
      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        searchZoneToCompute(prev) match{
          case null => null
          case list =>
            val car = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,pos)
            for(id<- car+1 until v)
              recordChangeOfVehicleTag(id,startPosOfVehicle(id)-1)

            def shiftByOne(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail => smartPrepend(r.oldPosToNewPos(a).get,r.oldPosToNewPos(b).get, shiftByOne(tail))
              }
            }

            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null):List[(Int,Int)]= {
              list match {
                case Nil => {
                  if ((car != v - 1 && pos != startPosOfVehicle(car + 1)) || (car == v - 1 && (pos < changes.newValue.size)))  List((pos, pos))
                  else list
                }
                case (a, b) :: tail =>
                  if (b < pos) smartPrepend(a, b, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tail))
                  else if (b == pos)  if(b>a) smartPrepend(a, r.oldPosToNewPos(b-1).get, shiftByOne(tail)) else  shiftByOne(tail)
                  else if (a == pos) if(b>a) smartPrepend(a, r.oldPosToNewPos(b-1).get, shiftByOne(tail)) else  shiftByOne(tail)
                  else smartPrepend(a, r.oldPosToNewPos(b).get, shiftByOne(tail))
              }
            }
            val capa = getRemainingCapacityOfNode(r.removedValue,changedNodeSinceCheckpoint(r.removedValue))
            val violationToRemove = if(capa>cMax) capa-cMax else if(capa<0) math.abs(capa) else 0
            recordChangeOfVehicleViolation(car,getViolationOfVehicle(car)-violationToRemove)
            recordChangeOfGlobalViolation(GlobalViolation.newValue-violationToRemove)
            setRemainingCapacityOfNode(r.removedValue, 0,recordMove(r.removedValue))
            updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove()



        }
      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        searchZoneToCompute(prev) match{
          case null => null
          case list =>
            val initVehicule = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,fromIncluded)
            val destVehicule = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,after)
            var tmp = list
            val case1:Boolean = after<fromIncluded
            var test = tmp.filterNot((elt:(Int,Int))=> if (case1) elt._1> toIncluded+1 || elt._2<=after
            else elt._2<= fromIncluded-1 || elt._1 > after +1)
            var toReinsert:QList[(Int,Int)]=null
            for(elt <- test){
              if(case1 && (elt._1> after && elt._2< fromIncluded-1) ) {
                // entre after et from
                toReinsert = QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(elt._2).get), toReinsert)
              }else if(!case1 && ( elt._1> toIncluded+1 && elt._2<= after) ) {
                //entre to et aft
                toReinsert = QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(elt._2).get), toReinsert)
              }else if(elt._1>=fromIncluded && elt._2<=toIncluded) {
                // dans l'intrs
                toReinsert = if (!flip) QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(elt._2).get), toReinsert)
                else QList((m.oldPosToNewPos(elt._2).get, m.oldPosToNewPos(elt._1).get), toReinsert)
              }else if (elt._1 >= fromIncluded && elt._1 <= toIncluded && elt._2 > toIncluded) {
                // start dans interval
                toReinsert = if (case1) QList((toIncluded + 1, elt._2), (if(!flip) QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(toIncluded).get), toReinsert) else QList((m.oldPosToNewPos(toIncluded).get, m.oldPosToNewPos(elt._1).get), toReinsert)))
                else QList((fromIncluded, m.oldPosToNewPos(elt._2).get), (if(!flip) QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(toIncluded).get), toReinsert) else QList((m.oldPosToNewPos(toIncluded).get, m.oldPosToNewPos(elt._1).get), toReinsert)))
              }else if(elt._1<fromIncluded && elt._2>= fromIncluded && elt._2<= toIncluded) {
                // end dans interval
                toReinsert = if (case1) QList((m.oldPosToNewPos(elt._1).get, toIncluded), (if(!flip) QList((m.oldPosToNewPos(fromIncluded).get, m.oldPosToNewPos(elt._2).get), toReinsert)
                else QList((m.oldPosToNewPos(elt._2).get, m.oldPosToNewPos(fromIncluded).get), toReinsert))) else QList((elt._1, fromIncluded - 1), (if(!flip) QList((m.oldPosToNewPos(fromIncluded).get, m.oldPosToNewPos(elt._2).get), toReinsert) else QList((m.oldPosToNewPos(elt._2).get, m.oldPosToNewPos(fromIncluded).get), toReinsert)))
              }else if (elt._1 < fromIncluded && elt._2 > toIncluded) {
                // interval dans pos
                toReinsert = if (case1) QList((m.oldPosToNewPos(elt._1).get, elt._2), (if(!flip) QList((m.oldPosToNewPos(fromIncluded).get, m.oldPosToNewPos(toIncluded).get), toReinsert) else QList((m.oldPosToNewPos(toIncluded).get, m.oldPosToNewPos(fromIncluded).get), toReinsert) ))
                else QList((elt._1, m.oldPosToNewPos(elt._2).get), QList((m.oldPosToNewPos(fromIncluded).get, m.oldPosToNewPos(toIncluded).get), toReinsert))
              }else if((case1 && (elt._1 > after && elt._2 == fromIncluded-1)) || (!case1 && (elt._1== toIncluded+1 && elt._2<= after))) {
                //
                toReinsert = QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(elt._2).get), toReinsert)
              }else if (elt._1<=after && after < elt._2 && (case1 || elt._1>toIncluded ) ) {
                //
                toReinsert = if (case1) QList((m.oldPosToNewPos(after + 1).get, m.oldPosToNewPos(elt._2).get), QList((elt._1, after), toReinsert))
                else QList((after + 1, elt._2), QList((m.oldPosToNewPos(elt._1).get, m.oldPosToNewPos(after).get), toReinsert))
              }
            }
            tmp = tmp.diff(test)
            var iter = toReinsert.iterator
            while(iter.hasNext){
              val item = iter.next()
              // println(item )
              tmp = insertInList(tmp,item._1,item._2)
            }

            // maj position des marqueur de vehicule
            if (initVehicule!= destVehicule) {
              var car = if(case1) destVehicule+1 else initVehicule+1
              while (car <= (if(case1) initVehicule else destVehicule) ) {
                recordChangeOfVehicleTag(car,m.oldPosToNewPos(startPosOfVehicle(car)).get)
                car += 1
              }
            }

            if(case1) {
              var dst =  if (destVehicule != v - 1)  math.min(m.oldPosToNewPos(after+1).get, startPosOfVehicle(destVehicule + 1)-1) // juste pour savoir si
              else  math.min(m.oldPosToNewPos(after+1).get, routes.newValue.size-1)
              tmp = if (flip) insertInList(tmp, m.oldPosToNewPos(toIncluded).get, dst) else insertInList(insertInList(tmp, m.oldPosToNewPos(fromIncluded).get
                , m.oldPosToNewPos(fromIncluded).get), dst,dst)

              // calculer la valeur du noeud qu'on avait juste après toIncluded (avant le move)
              val vehiculeOfSrc = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,fromIncluded-1)
              // si le noeud aprs toinclude n'est pas un marqueur OU s'il y a un node apres toincluded (i.e. c'est pas le dernier node de la seq)
              dst = if ((vehiculeOfSrc == v - 1 || (toIncluded + 1) < startPosOfVehicle(vehiculeOfSrc + 1)) && toIncluded+1 <= routes.newValue.size-1) toIncluded+1
              // sinon on a rien a calculer
              else -1
              if(dst != -1) tmp =insertInList(tmp, dst,dst)

            } else {
              val vehiculeOfSrc = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded-1, prev.newValue, v)  // le vehicule avant from
              var dst = if (vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < startPosOfVehicle(vehiculeOfSrc + 1) )// s'il n'y a pas de marqueur ou qu'on est pas sur un marqueur
                fromIncluded
              else -1 //sinon par de calcule
              if(dst != -1) tmp = insertInList(tmp, dst,dst)
              val vehiculeOfNodeFollowingAfter = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,after+1)
              dst = if((after == routes.newValue.size-1) || ( vehiculeOfNodeFollowingAfter!=destVehicule ))  after else after+1
              tmp = if (flip) insertInList(tmp, m.oldPosToNewPos(toIncluded).get, dst) else
                insertInList(insertInList(tmp, m.oldPosToNewPos(after).get+1, m.oldPosToNewPos(after).get+1), dst,dst)
            }
            var node = prev.newValue.explorerAtPosition(fromIncluded).get
            // si on deplace une sous-seq d'un vehicle a un autre  ==> maj viol du vehicul src
            var work = true
            while(work){

              val newVehicle = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(changes.newValue,m.oldPosToNewPos(node.position).get)
              val oldVehcile = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,node.position)
              val capOfNode = getRemainingCapacityOfNode(node.value,changedNodeSinceCheckpoint(node.value))
              val violationAtNode = if (capOfNode>cMax) capOfNode-cMax else if (capOfNode<0) Math.abs(capOfNode) else 0
              if( newVehicle != oldVehcile)  {
                recordChangeOfVehicleViolation(oldVehcile, getViolationOfVehicle(oldVehcile)-violationAtNode)
                recordChangeOfVehicleViolation(newVehicle,getViolationOfVehicle(newVehicle)+ violationAtNode)
              }
              work = node.position<toIncluded
              if(work) node = node.next.get
            }
            node = null
            tmp
        }
      case SeqUpdateAssign(value : IntSequence) =>   null
      case SeqUpdateLastNotified(value:IntSequence) =>
        require (value quickEquals routes.value)
        List.empty[(Int,Int)]
      case s@SeqUpdateDefineCheckpoint(prev:SeqUpdate,_) =>
        val toReturn = searchZoneToCompute(prev)
        saveCheckPoint(true)
        toReturn
      case u@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) => // restaurer avant chkpt
        recovery()
        List.empty[(Int,Int)]
    }
  }

  /**
    * Assignes the result of computeViolationFromScratch to the concerned variables
    * @param route sequence of Integers representing the routes
    */
  private def computeFromScratch(route:IntSequence): Unit ={
    val result = computeViolationFromScratch(route)
    globalViolation:=result._1
    capacityAtNode=result._2.clone()
    for(car <- 0 until v )
      vehicleViolation(car):= result._3(car)
    positionOfTagVehicule = result._4.clone()
  }

  /**
    * For each vehicle, computes from scratch the vehicle capacity  for each node assigned at this vehicle,
    * the final violation for the vehicle and the global violation which is the sum of vehicle violations for this route
    * @param route sequence of Integers representing the routes
    * @return (global violation, capacity by node), vehicle violation, tag vehicle position )
    */
  def computeViolationFromScratch(route:IntSequence) :(Int,Array[Int],Array[Int],Array[Int]) = {
    val tmpchk = chk
    val violV : Array[Int]= Array.tabulate(v)((node:Int)=> 0)
    val startPosOfVehiculeInternal : Array[Int]= Array.tabulate(v)((node:Int)=> 0)
    val capa : Array[Int]= Array.tabulate(n)((node:Int)=> 0)



    var current = route.explorerAtPosition(0).get
    var currentCar = current.value
    startPosOfVehiculeInternal.update(currentCar, current.position)
    var valueOfCurrentNode =  initValue(current.value)
    violV.update(currentCar, (if (valueOfCurrentNode > cMax) cMax - valueOfCurrentNode else if (valueOfCurrentNode < 0)  Math.abs(valueOfCurrentNode) else 0))
    capa(current.value)= valueOfCurrentNode

    while(!current.next.isEmpty){
      val previous = current
      val valueOfPresiousNode = valueOfCurrentNode
      current = current.next.get
      if(current.value < v){
        //sauver valeur du dernier node du vehicule
        currentCar = current.value
        startPosOfVehiculeInternal.update(currentCar, current.position)
        valueOfCurrentNode = initValue(current.value)
        capa(current.value)=  valueOfCurrentNode
        violV.update(currentCar, (if (valueOfCurrentNode > cMax) violV(currentCar)+ (valueOfCurrentNode -cMax ) else if (valueOfCurrentNode < 0)  violV(currentCar)+  Math.abs(valueOfCurrentNode) else violV(currentCar)))
      }
      //sinon on continue sur le meme vehicule
      else{
        valueOfCurrentNode =  op(previous.value,current.value,valueOfPresiousNode)
        capa(current.value)=  valueOfCurrentNode
        violV.update(currentCar, (if (valueOfCurrentNode > cMax) violV(currentCar)+ (valueOfCurrentNode -cMax) else if (valueOfCurrentNode < 0)  violV(currentCar)+  Math.abs(valueOfCurrentNode) else violV(currentCar)))
      }
    }
    var viol:Int = 0
    violV.foreach((elt:Int)=> viol+= elt)
    (viol,capa,violV,startPosOfVehiculeInternal)
  }

  /**
    * Computes capacity at each node and the global violation from a given area
    * @param newRoute the sequence after the SeqUpdate
    * @param lst the list containing positions where calculations must be performed
    */
  def computeViolationFromSomWhere(newRoute:IntSequence,lst: List[(Int, Int)]){

    // si on a un chk pt on sauve les capa ET la viol qui va avec

    val iter = lst.toIterator
    var pair = iter.next() // première pair
    var start = pair._1
    var end = pair._2
    pair = if(iter.hasNext)  iter.next() else null // paire suivant si elle existe

    // premier noeud
    var current= newRoute.explorerAtPosition(start).get// first is mandatory ...
    // limite sup pour le noeud
    var upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position) != v-1)
      startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position)+1)
    else routes.newValue.size
    // noeud  precedent
    var previousPos = newRoute.explorerAtPosition(startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position))).get
    // valeur du noeud précédent
    var valueOfPresiousNode=initValue(previousPos.value)
    // maj noeud precedent s'il existe
    if(current.position > previousPos.position) {
      previousPos = newRoute.explorerAtPosition(current.position-1).get
      valueOfPresiousNode = getRemainingCapacityOfNode(previousPos.value,changedNodeSinceCheckpoint(previousPos.value)) }
    var valueOfCurrentNode = op(previousPos.value,current.value,valueOfPresiousNode)
    var oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value,changedNodeSinceCheckpoint(current.value))

    var cdt:Boolean = if(current.position==end ) /* si j'ai (deja) finis  ==> verif cdt */ valueOfCurrentNode==oldValueOfCurrentNode else  false
    setRemainingCapacityOfNode(current.value, valueOfCurrentNode,recordMove(current.value))

    computeViolationOfNode(current.value, oldValueOfCurrentNode,RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position))

    if(current.position==end && pair != null){
      if((cdt || current.position==upperBound-1 )){
        //  println("next Pair ")
        cdt = false
        end = pair._2
        start = pair._1
        current= newRoute.explorerAtPosition(start-1).get // if(start >= current.position) newRoute.explorerAtPosition(start-1).get else current
        upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start) != v-1) // maj upp si change de vehicul
          startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start)+1)
        else routes.newValue.size
        pair= if(iter.hasNext)  /* recupere pair suivant si elle existe*/ iter.next() else null
      }
    }

    while(!cdt && current.position<upperBound-1 ){
      previousPos=current
      current = current.next.get // passe au noeud suivant

      // tant que  pos obliatoire
      if(current.position <= end){
        valueOfPresiousNode=getRemainingCapacityOfNode(previousPos.value,changedNodeSinceCheckpoint(previousPos.value))
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPresiousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value,changedNodeSinceCheckpoint(current.value))
        setRemainingCapacityOfNode(current.value, valueOfCurrentNode,recordMove(current.value))


        computeViolationOfNode(current.value, oldValueOfCurrentNode,RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position))


        if(current.position==end ){

          cdt=  valueOfCurrentNode== oldValueOfCurrentNode
          if((cdt || current.position==upperBound-1 ) && pair != null){
            cdt = false
            end = pair._2
            start = pair._1
            current= newRoute.explorerAtPosition(start-1).get
            upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start) != v-1) // maj upp si change de vehicul
              startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start)+1)
            else routes.newValue.size
            pair= if(iter.hasNext) iter.next() else null
          }
        }
      }
      else{
        if(pair != null && current.position >=pair._1 && current.position ==pair._2 ){ // si je depasse deja la prochaine pair de pos je recupère la suivante
          pair= if(iter.hasNext) iter.next() else null
        }

        valueOfPresiousNode = valueOfCurrentNode
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPresiousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value,changedNodeSinceCheckpoint(current.value))
        setRemainingCapacityOfNode(current.value, valueOfCurrentNode,recordMove(current.value))
        computeViolationOfNode(current.value, oldValueOfCurrentNode,RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,current.position))
        cdt= valueOfCurrentNode== oldValueOfCurrentNode
        //gestion des pairs
        if((cdt || current.position==upperBound-1 ) && pair != null){
          cdt = false
          end = pair._2
          start = pair._1
          current=  if(start-1 >= current.position) newRoute.explorerAtPosition(start-1).get else current
          upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start) != v-1) // maj upp si change de vehicul
            startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(newRoute,start)+1)
          else routes.newValue.size
          pair= if(iter.hasNext) iter.next() else null
        }
      }
    }
  }



  /**
    * Returns the capacity associated with a node.
    * @param nodeId the id of the node
    * @param save false to return the capacity save at the previous checpoint, true to return the capacity calculated at the last movement
    * @return the capacity of the node
    */
  def getRemainingCapacityOfNode(nodeId: Int, save:Boolean): Int = {  //save:Boolean = changedNodeSinceCheckpoint.get(nodeId) doesnt work :(
    if(save) changedCapacityAtNode(nodeId) else capacityAtNode(nodeId)
  }


  /**
    * Overridden the old capacity of a node by the new value.
    * @param currentNode the id of the node
    * @param valueOfCurrentNode the new capacity associated with the node
    * @param save false to override the current capacity, true to save the current capacity
    */
  def setRemainingCapacityOfNode(currentNode: Int, valueOfCurrentNode: Int, save:Boolean): Unit = {
    if (save) changedCapacityAtNode(currentNode) =valueOfCurrentNode  else capacityAtNode(currentNode) =valueOfCurrentNode
  }




  /**
    * Compute the Global degree of violation after a update of the capacity of a node
    * @param currentNode the id of the node
    * @param oldValueOfCurrentNode the old capacity of the node
    */
  def computeViolationOfNode(currentNode: Int, oldValueOfCurrentNode:Int=0, vehicle:Int ): Unit = {
    val currentCapacityOfNode = getRemainingCapacityOfNode(currentNode, isMoved(currentNode))

    // old violation
    val oldViolationOfNode = if (oldValueOfCurrentNode > cMax)  oldValueOfCurrentNode - cMax
    else if (oldValueOfCurrentNode < 0)  Math.abs(oldValueOfCurrentNode) else 0

    // nw viol
    val currentViolation = if (currentCapacityOfNode > cMax) currentCapacityOfNode-  cMax  else if (currentCapacityOfNode < 0)  Math.abs(currentCapacityOfNode) else 0

    var oldViolation = getViolationOfVehicle(vehicle)
    recordChangeOfVehicleViolation(vehicle, oldViolation+ (currentViolation-oldViolationOfNode))
    recordChangeOfGlobalViolation(GlobalViolation.newValue+(getViolationOfVehicle(vehicle)-oldViolation))
  }



  /**
    * Returns the Global Violation of the current configuration of the nodes
    * @return violation
    */
  def GlobalViolation: CBLSIntVar = {
    if(chkV) changedGlobalViolation else globalViolation
  }


  def recordChangeOfGlobalViolation(value:Int, save:Boolean=chk) {
    if(save) {
      changedGlobalViolation(chk)
      changedGlobalViolation.:=(value)
    } else  globalViolation.:=(value)
  }

  /**
    * Saves the current Global Violation
    * @param value true if you want the save, false otherwise
    */
  def changedGlobalViolation(value : Boolean): Unit = {
    chkV = value
  }


  /**
    * Saves the current context (capacity of each node and the global violation)
    * @param _chk true to save false otherwise
    */
  def saveCheckPoint(_chk: Boolean): Unit ={
    chk = _chk
    if(chk){
      globalViolation:=GlobalViolation.newValue
      changedGlobalViolation(false)
      var iter = changedVehicleViolationSinceCheckpoint.indicesAtTrue
      while(iter.hasNext){
        val id  = iter.next()
        recordChangeOfVehicleViolation(id,getViolationOfVehicle(id),false)
      }
      changedVehicleViolationSinceCheckpoint.all_=(false)
      iter = changedNodeSinceCheckpoint.indicesAtTrue
      while (iter.hasNext){
        val tmp = iter.next()
        setRemainingCapacityOfNode(tmp, getRemainingCapacityOfNode(tmp,save = true),save = false)
      }
      changedNodeSinceCheckpoint.all_=(false)
      iter = changedPositionOfTagfVehicleSinceCheckpoint.indicesAtTrue
      while (iter.hasNext){
        val car = iter.next()
        recordChangeOfVehicleTag(car,changedPositionOfTagVehicule(car),false)
      }
      changedPositionOfTagfVehicleSinceCheckpoint.all_=(false)
    }
  }

  /**
    * Recovers the saved context
    */
  def recovery(): Unit ={
    changedNodeSinceCheckpoint.all_=(false)
    changedVehicleViolationSinceCheckpoint.all_=(false)
    changedPositionOfTagfVehicleSinceCheckpoint.all_=(false)
    changedGlobalViolation(false)
  }



  /**
    * Check if the capacity of the given node is changed
    * @param nodeId the id of the node
    * @return true is capacity has changed false otherwise
    */
  def isMoved(nodeId:Int):Boolean={
    changedNodeSinceCheckpoint(nodeId)
  }

  /**
    * Records the last movement of the given node
    * @param id the id of the current node
    * @return true is saved, false otherwise
    */
  def recordMove(id:Int): Boolean ={
    changedNodeSinceCheckpoint.update(id,chk)
    chk
  }

  /**
    * Returns the violation degree of the given vehicle
    * @param vehicle the vehicle to consider
    * @param save only for checkInternals
    * @return the current violation of the vehicle
    */
  def getViolationOfVehicle(vehicle:Int, saved:Int=2): Int ={
    if(saved==2)  {if(changedVehicleViolationSinceCheckpoint(vehicle)) changedVehicleViolation(vehicle).newValue else vehicleViolation(vehicle).newValue}
    else {if(saved==1) changedVehicleViolation(vehicle).newValue else vehicleViolation(vehicle).newValue}
  }

  /**
    * Records the last violation degree of the given vehicle
    * @param vehicle the vehicle to consider
    * @param value the new value of violation
    * @param save (optional) true if checkpoint, false otherwise
    */
  def recordChangeOfVehicleViolation(vehicle:Int, value:Int, save:Boolean=chk) {
    if(save) {
      changedVehicleViolationSinceCheckpoint(vehicle)=true
      changedVehicleViolation(vehicle) := value
    } else  vehicleViolation(vehicle) := value
  }

  /**
    * Returns the position of the tag of the given vehicle
    * @param vehicle the vehicle to consider
    * @return the position in the sequence of the first node of the given vehicle
    */
  def startPosOfVehicle(vehicle:Int): Int ={
    if(changedPositionOfTagfVehicleSinceCheckpoint(vehicle)) changedPositionOfTagVehicule(vehicle) else positionOfTagVehicule(vehicle)
  }

  /**
    * Records the last tag position of the tag of the given vehicle
    * @param vehicle the vehicle to consider
    * @param value the value of the tag
    * @param save (optional) true if checkpoint, false otherwise
    */
  def recordChangeOfVehicleTag(vehicle:Int, value:Int, save:Boolean=chk) {
    if(save) {
      changedPositionOfTagfVehicleSinceCheckpoint.update(vehicle,true)
      changedPositionOfTagVehicule.update(vehicle, value)
    } else  positionOfTagVehicule.update(vehicle, value)
  }

  /**
    * Insert a new pair into a list or update the list
    * @param list the list in which a pair must be inserted
    * @param start the start position
    * @param end the end position
    * @return the list containing the new pair according right behavior :
    *         =1.=
    *         the new pair is contained into a pair already present => do nothing
    *         =2.=
    *
    *         the new pair is covered (not all the pair, only a piece)
    *         ==2-I.==
    *          [Start -------(NStart ----End] ---------NEnd) ==> [start,NEnd)
    *         ==2-II.==
    *         (NStart --------[Start------------NEnd)-------End] ==> (Nstart End]
    *         =3.=
    *         the new pair is right next to a old pair => merge both of
    *         ==3-I.==
    *         [Start----End](NStart-----NEnd) ==> [Start,NEnd)
    *         ==3-II.==
    *         (NStart-----NEnd)[Start----End] ==> (NStart,End]
    *         =4.=
    *         add a new pair otherwise
    *         NB : the list is sorted
    */
  private def insertInList(list:List[(Int,Int)],start:Int,end:Int): List[(Int, Int)] = {
    require(start <= end)
    var concernedByNewPair = list.filterNot((elt:(Int,Int))=> elt._1> end+1 || elt._2< start-1)
    val effectiveStart = if (concernedByNewPair.nonEmpty) math.min(start, concernedByNewPair.head._1) else start
    val effectiveEnd = if (concernedByNewPair.nonEmpty) Math.max(end, concernedByNewPair.last._2) else end
    var listTmp:List[(Int,Int)] = list
    listTmp = listTmp.diff(concernedByNewPair)
    listTmp = (effectiveStart,effectiveEnd) +:listTmp
    listTmp=listTmp.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2)
    listTmp
  }

  override def checkInternals(c: Checker): Unit = {

    val values = computeViolationFromScratch(routes.newValue)

    for (node <- routes.newValue) {
      c.check(values._2(node) equals getRemainingCapacityOfNode(node, isMoved(node)),
        Some("Founded capacity at node(" + node + "):=" + getRemainingCapacityOfNode(node,isMoved(node)) + " should be :=" + values._2(node)))
    }

    c.check(GlobalViolation.newValue equals values._1, Some("Founded Violation :="+GlobalViolation.newValue+" should be :="+values._1))

    for(car <- 0 until v) {
      c.check(startPosOfVehicle(car) equals values._4(car), Some("Founded start pos of vehicle("+car+") :="+startPosOfVehicle(car)+" should be :="+values._4(car)))
    }

    for(car <- 0 until v) {
      c.check(getViolationOfVehicle(car) equals values._3(car), Some("Founded violation of vehicle("+car+") :="+getViolationOfVehicle(car)+" should be :="+values._3(car)))
    }
  }
}

