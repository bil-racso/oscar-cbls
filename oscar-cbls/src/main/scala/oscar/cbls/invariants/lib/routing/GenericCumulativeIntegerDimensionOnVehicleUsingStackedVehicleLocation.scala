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
  * Created by  Jannou Brohée on 8/11/16.
  */

object GenericCumulativeIntegerDimensionOnVehicleUsingStackedVehicleLocation {

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
    new GenericCumulativeIntegerDimensionOnVehicleUsingStackedVehicleLocation(routes, n, v, op, initValue,output)
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
class GenericCumulativeIntegerDimensionOnVehicleUsingStackedVehicleLocation(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, initValue :Array[Int], output:Array[CBLSIntVar])
  extends Invariant()
    with SeqNotificationTarget {

  require(initValue.length==v)
  require( output.length==n)
  //VehiculeLocation=ConcreteVehicleLocation(Array.tabulate(v)(((car:Int)=> -1))) // this will be updated by computeContentAndVehicleStartPositionsFromScratch
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- output) {
    i.setDefiningInvariant(this)
  }

  private var stack = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
  // println("stack after scratch ?  "+stack)
  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    // println(changes)
    // stack=stack.regularize()
    val zoneToCompute = updateVehicleStartPositionsAndSearchZoneToUpdate(changes)
    //println("stack for changes ? "+stack)
    zoneToCompute match {
      case null => stack = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
      case tree =>
        for(car <- tree.keys)  {
          val lst = zoneToCompute.get(car).get

          if (lst.nonEmpty) updateContentForSelectedZones(routes.newValue,lst,stack.posOfVehicle(car),car)
        }
    }
    this.checkInternals(new ErrorChecker())
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
    // println("smart "+zoneStart+" , "+zoneEnd)
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
  private def updateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate) : RedBlackTreeMap[List[(Int,Int)]] = {

    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>
            // println("ici  ? ")
            stack = stack.push((posit)=>s.oldPosToNewPos(posit)) //up tag pos
          val car = stack.vehicleReachingPosition(pos)
            val posOfTag = stack.posOfVehicle(car)

            def shiftBy(list:List[(Int,Int)],deltaStart:Int=1):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail => (a+deltaStart,b+1) :: shiftBy(tail)
              }
            }

            val relativePos = pos - posOfTag
            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => List((relativePos, (math.min(pos + 1, if (car != v - 1) stack.posOfVehicle(car + 1) - 1 else changes.newValue.size - 1) )- posOfTag))
                case (startZone, endZone) :: tail =>
                  if (endZone < relativePos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tail))
                  else if (endZone == relativePos) smartPrepend(startZone, endZone + 1, shiftBy(tail))
                  else smartPrepend(relativePos, (math.min(pos + 1, if (car != v - 1) stack.posOfVehicle(car + 1) - 1 else changes.newValue.size - 1) - posOfTag), shiftBy(list, if (startZone  < relativePos && endZone > relativePos) 0 else 1))
              }
            }
            /*def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list:List[(Int,Int)]):List[(Int,Int)]= {
                          list match {
                            case Nil => List((pos - posOfTag, (math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1)) - posOfTag))
                            case (startZone, endZone) :: tail =>
                              val end = endZone + posOfTag
                              if (end < pos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tail))
                              else if (end == pos) smartPrepend(startZone, endZone + 1, shiftBy(tail))
                              else smartPrepend(pos - posOfTag, (math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1) - posOfTag), shiftBy(list, if (startZone + posOfTag < pos && end > pos) 0 else 1))
                          }
                        }*/
            tree.insert(car,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(tree.getOrElse(car,List.empty[(Int,Int)])) )

        }
      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>
            //  println("stack for changes ? "+stack)
            //println("REMOVE at pos :"+pos+" "+changes.newValue.mkString(",")+" <== "+prev.newValue.mkString(","))
            val car = stack.vehicleReachingPosition(pos)
            stack = stack.push((posit)=>r.oldPosToNewPos(posit))//.updateForDelete(pos)
          val relativePos = pos - stack.posOfVehicle(car)

            def shiftByOne(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => list
                case (a,b) :: tail =>  smartPrepend(a-1,b-1, shiftByOne(tail))
              }
            }

            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list:List[(Int,Int)]):List[(Int,Int)]= {
              list match {
                case Nil => if ((car != v - 1 && pos != stack.posOfVehicle(car + 1)) || (car == v - 1 && (pos < changes.newValue.size)))
                  List((relativePos, relativePos))
                else list
                case (startZone, endZone) :: tail =>
                  if (endZone < relativePos) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tail))
                  else if (endZone >= relativePos && startZone <= relativePos)
                    if(endZone >startZone ) smartPrepend(startZone, endZone-1, shiftByOne(tail)) else  shiftByOne(tail)
                  else{
                    smartPrepend(relativePos, relativePos, shiftByOne(list))
                  }
              }
            }

            tree.insert(car, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tree.getOrElse(car,List.empty[(Int,Int)])))
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case tree =>
            val vehicleSource = stack.vehicleReachingPosition(fromIncluded)
            val vehicleDestination =stack.vehicleReachingPosition(after)
            val vehiculeOfSrc = stack.vehicleReachingPosition(fromIncluded-1)
            val vehiculeOfNodeFollowingAfter = stack.vehicleReachingPosition(after+1)
            val case1:Boolean = after<fromIncluded
            val relativeFromIncluded = fromIncluded - stack.posOfVehicle(vehicleSource)
            val relativeToIncluded = toIncluded -stack.posOfVehicle(vehicleSource)
            val relativeAfter = after - stack.posOfVehicle(vehicleDestination)
            val delta = toIncluded - fromIncluded +1
            val  newAfter = m.oldPosToNewPos(after).get
            val beforeFrom = fromIncluded-1
            val newBeforeFrom = m.oldPosToNewPos(beforeFrom).get


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

            var toReinsertInTheOtherSide= List.empty[(Int,Int)]


            def updateListOfZoneToUpdateAfterMove(listOfZonesForVehicle:List[(Int,Int)],sourceSide:Boolean=true):List[(Int,Int)]={
              listOfZonesForVehicle match {
                case Nil => listOfZonesForVehicle
                case (startZone, endZone) :: tail =>
                  if (sourceSide) {// traitement du coté [from,to]
                    if (endZone < relativeFromIncluded) smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail)) // on avance
                    else if (startZone > relativeToIncluded) smartPrepend(startZone - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail))// on decale
                    else {// on gere les deplacement due au mouvement
                      if(!flip)  toReinsertInTheOtherSide = (Math.max(startZone, relativeFromIncluded)-relativeFromIncluded,Math.min(relativeToIncluded, endZone)-relativeFromIncluded) ::toReinsertInTheOtherSide
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


            stack = stack.push((pos)=>m.oldPosToNewPos(pos)) // .updateForMove(fromIncluded,toIncluded,after,if(case1) toIncluded-fromIncluded+1 else -(toIncluded-fromIncluded+1));
          //println("check ? "+startPosOfVehicle)
          val newRelativeAfter = newAfter - stack.posOfVehicle(vehicleDestination)
            var toReinsertInTheDestinationSideList:List[(Int,Int)]=List.empty[(Int,Int)]

            // peut mieux faire  ? TODO
            for(elt <- toReinsertInTheOtherSide) toReinsertInTheDestinationSideList = ((newRelativeAfter+1+elt._1), (newRelativeAfter+1+elt._2)) :: toReinsertInTheDestinationSideList

            if(case1) {
              var dst =  if (vehicleDestination != v - 1)  math.min(m.oldPosToNewPos(after+1).get, stack.posOfVehicle(vehicleDestination + 1)-1)-stack.posOfVehicle(vehicleDestination) // juste pour savoir si
              else  math.min(m.oldPosToNewPos(after+1).get, routes.newValue.size-1)-stack.posOfVehicle(vehicleDestination)
              toReinsertInTheDestinationSideList =  if (flip) insertInList(toReinsertInTheDestinationSideList, List.apply((m.oldPosToNewPos(toIncluded).get-stack.posOfVehicle(vehicleDestination), dst)))
              else insertInList(insertInList(toReinsertInTheDestinationSideList,List.apply((dst,dst)) ) , List.apply((m.oldPosToNewPos(fromIncluded).get-stack.posOfVehicle(vehicleDestination), m.oldPosToNewPos(fromIncluded).get-stack.posOfVehicle(vehicleDestination)) ))

              dst = if ((vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < stack.posOfVehicle(vehiculeOfSrc + 1)) && toIncluded+1 <= routes.newValue.size-1) toIncluded+1  -stack.posOfVehicle(vehicleSource) else -1
              if(dst != -1) tmp = tmp.insert(vehicleSource,insertInList(tmp.getOrElse(vehicleSource, List.empty[(Int,Int)]),List.apply((dst,dst))))

            }
            else {
              var dst = if (vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < stack.posOfVehicle(vehiculeOfSrc + 1)) fromIncluded else -1
              dst-= stack.posOfVehicle(vehicleSource)
              if(dst > -1)   tmp = tmp.insert(vehicleSource,insertInList(tmp.getOrElse(vehicleSource, List.empty[(Int,Int)]),List.apply((dst,dst))))
              dst = if((after == routes.newValue.size-1) || ( vehiculeOfNodeFollowingAfter!=vehicleDestination ))  newRelativeAfter+delta else newRelativeAfter+delta+1
              toReinsertInTheDestinationSideList = if (flip) insertInList(toReinsertInTheDestinationSideList,List.apply((newRelativeAfter+1, dst)))
              else insertInList(insertInList(toReinsertInTheDestinationSideList, List.apply((dst, dst))), List.apply((newRelativeAfter+1,newRelativeAfter+1)))
            }

            tmp.insert(vehicleDestination,insertInList(tmp.getOrElse(vehicleDestination, List.empty[(Int,Int)]),toReinsertInTheDestinationSideList))
        }
      case SeqUpdateAssign(value : IntSequence) =>
        stack=stack.regularize()
        null
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
  def computeContentAndVehicleStartPositionsFromScratch(s:IntSequence, outputInternal: Array[CBLSIntVar] = output): VehicleLocation ={ // todo changes output en array int ;)
  var current = s.explorerAtPosition(0).get
    var currentCar = current.value
    //println("scratch " +currentCar+" "+current.value)
    var tmp : Array[Int]=Array.tabulate(v)(((car:Int)=> 0))
    tmp(currentCar) = current.position
    var valueOfCurrentNode =  initValue(current.value)
    setRemainingCapacityOfNode(current.value, valueOfCurrentNode, outputInternal)
    while(!current.next.isEmpty){
      val previous = current
      val valueOfPresiousNode = valueOfCurrentNode
      current = current.next.get
      if(current.value < v){
        //sauver valeur du dernier node du vehicule
        currentCar = current.value
        //println("scratch " +currentCar+" "+current.value)
        tmp(currentCar) = current.position
        valueOfCurrentNode = initValue(current.value)
      }
      //sinon on continue sur le meme vehicule
      else valueOfCurrentNode =  op(previous.value,current.value,valueOfPresiousNode)
      setRemainingCapacityOfNode(current.value, valueOfCurrentNode,outputInternal )
    }
    new ConcreteVehicleLocation(tmp)
  }

  /**
    * Computes the capacity of nodes concerned by a SeqUpdate
    * @param s the sequence after the SeqUpdate
    * @param lst the list containing positions where calculations must be performed
    */
  def updateContentForSelectedZones(s:IntSequence, lst: List[(Int, Int)],posOfTag:Int,car:Int){
    // println("car ? "+ car +"\nseq ="+s.mkString(","))
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
    var upperBound =  if (car != v - 1) stack.posOfVehicle(car + 1) else routes.newValue.size
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
    //println(stack.vehicleReachingPosition(current.position)+" must be the car : " +car )
    // require(car == stack.vehicleReachingPosition(current.position))
    //println(stack.posOfVehicle(stack.vehicleReachingPosition(current.position))+" must be := "+s.positionsOfValue(stack.vehicleReachingPosition(current.position)).firstKey )
    //require(s.positionsOfValue(car).firstKey==stack.posOfVehicle(stack.vehicleReachingPosition(current.position)))
    var previousPos = s.explorerAtPosition(stack.posOfVehicle(stack.vehicleReachingPosition(current.position))).get
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

  override def checkInternals(c: Checker): Unit = {
    // println(startPosOfVehicle)
    var outputCheck: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(new Store(), Int.MinValue, routes.domain, "capacity at this f**king piece of cr*p of node :D ("+node.toString+")"))
    var startPosOfVehiculeCheck : VehicleLocation =  computeContentAndVehicleStartPositionsFromScratch(routes.newValue, outputCheck)//vehicleStartArray(v)// Array[Int]= Array.tabulate(v)(((car:Int)=> 0))

    for(car <- 0 until v){
      c.check(startPosOfVehiculeCheck.posOfVehicle(car) equals stack.posOfVehicle(car ), Some("Founded start of car(" + car + "):=" + stack.posOfVehicle(car) + " should be :=" + startPosOfVehiculeCheck.posOfVehicle(car)+" seq :"+routes.newValue.mkString(",")))

    }
    for (node <- routes.newValue) {
      c.check(getRemainingCapacityOfNode(node,outputCheck) equals getRemainingCapacityOfNode(node), Some("Founded Capacity at node(" + node + ") at pos : "+ routes.newValue.positionsOfValue(node).firstKey +"  at car :"+stack.vehicleReachingPosition(routes.newValue.positionsOfValue(node).firstKey)+" :=" + getRemainingCapacityOfNode(node) + " should be :=" + getRemainingCapacityOfNode(node,outputCheck)));//+ "\nstack : "+toprintln + "\nlastlast = "+lastlastChanges+"\nlast = "+lastChanges+ "\nseq :"+routes.newValue.mkString(",")))
    }

  }
}