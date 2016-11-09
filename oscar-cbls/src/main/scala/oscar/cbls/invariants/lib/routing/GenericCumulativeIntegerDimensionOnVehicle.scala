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

  private var startPosOfVehicle : Array[Int]= Array.tabulate(v)(((car:Int)=> Int.MinValue)) // this will be updated by computeContentAndVehicleStartPositionsFromScratch
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- output) {
    i.setDefiningInvariant(this)
  }

  computeContentAndVehicleStartPositionsFromScratch(routes.newValue)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    // println(changes)
    val zoneToCompute = updateVehicleStartPositionsAndSearchZoneToUpdate(changes)
    zoneToCompute match {
      case null => computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
      case list => if (zoneToCompute.nonEmpty)updateContentForSelectedZones(routes.newValue,zoneToCompute)
    }
    this.checkInternals(new ErrorChecker())
  }

  private def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] ={
    // println("ADD ("+zoneStart+","+zoneEnd+") to list ?"+list +"("+(if(list.nonEmpty)list.tail+","+list.head else ""))
    require(zoneStart<=zoneEnd)
    assert(list.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2).eq(list))
    val toReturn = (list match{
      case Nil => (zoneStart,zoneEnd) :: list
      case (a,b)::tail =>
        if(zoneEnd>=a-1 && zoneEnd<=b) {
          (Math.min(zoneStart,a), Math.max(zoneEnd,b)) :: tail

        } else if(a<zoneEnd ) {
          throw new Error("not sorted :( ")
        }else {
          // a > end

          (zoneStart,zoneEnd)::list
        }
    })
    //println(toReturn)
    toReturn
  }

  /**
    * Search the zones where changes occur following a SeqUpdate
    * @param changes the SeqUpdate
    * @return a list that specify mandatory zones which must be computed. A zone is represented by his start and end position : (startPositionIncluded, endPositionIncluded). Note that the list is sorted by position ((x,y) <= (x',y') iff y <= x'  )
    */
  private def updateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate) : List[(Int,Int)] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case list =>
            //update vehciles pos
            for(id<- RoutingConventionMethods.searchVehicleReachingPosition(pos,s.newValue, v)+1 until v) startPosOfVehicle.update(id,startPosOfVehicle(id)+1)




            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null):List[(Int,Int)]= {
              if (list.isEmpty && lastZone!=null){
                if(lastZone._2<pos-1) {
                  val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue, v)
                  var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                  return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                } else list
              } else if(list.isEmpty && lastZone==null){
                val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue, v)
                var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
              } else {
                val currentZone = list.head
                // println(currentZone + " pos inser " + pos + "seq " + s.newValue.mkString(","))
                var start = currentZone._1
                var end = s.oldPosToNewPos(currentZone._2).get
                if (currentZone._1 > pos) {
                  // check si last est plus petit de pos ==> insert
                  if(lastZone==null || lastZone._2< pos-1){
                    val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue,v)
                    var zone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list, zone))
                  } else{
                    start = s.oldPosToNewPos(currentZone._1).get
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }
                } else if (currentZone._1 == pos ) {
                  start = pos
                  val zone = (start, end)
                  return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                } else { // pos est après cur1
                  if(currentZone._2<pos-1){
                    return smartPrepend(currentZone._1,currentZone._2,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1),currentZone))
                  } else if(currentZone._2>=pos ){
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }else if(end==pos-1){
                    //  println("2pos "+pos+" end "+end)
                    val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue,v)
                    end = math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1)
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  } else {
                    //println("pos "+pos+" end "+end)
                    val zone = (start, pos)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }
                }
              }
            }

            /* TODO DRAFT

             def updateListOfZoneToUpdateAfterInsertReverse(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null):List[(Int,Int)]= {
              if (list.isEmpty && lastZone!=null){ //debut de la liste
                if(lastZone._2<pos-1) {
                  val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue, v)
                  var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                  return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                } else list
              } else if(list.isEmpty && lastZone==null){ // liste vide
                val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue, v)
                var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
              } else {
                val currentZone = list.last
                // println(currentZone + " pos inser " + pos + "seq " + s.newValue.mkString(","))
                var start = s.oldPosToNewPos(currentZone._1).get
                var end = s.oldPosToNewPos(currentZone._2).get
                if (start > pos) { // on continue
                  // check si last est plus petit de pos ==> insert
                  if(lastZone==null ){
                    val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue,v)
                    var zone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list, zone))
                  } else{ //cas de base
                    start = s.oldPosToNewPos(currentZone._1).get
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }
                } else if (currentZone._1 == pos ) {
                  start = pos
                  val zone = (start, end)
                  return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                } else { // pos est après cur1
                  if(currentZone._2<pos-1){
                    //fin ici
                    return smartPrepend(currentZone._1,currentZone._2,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1),currentZone))
                  } else if(currentZone._2>=pos ){
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }else if(end==pos-1){
                    //  println("2pos "+pos+" end "+end)
                    val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue,v)
                    end = math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1)
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  } else {
                    //println("pos "+pos+" end "+end)
                    val zone = (start, pos)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert(list.drop(1), zone))
                  }
                }
              }
            }
             */


            /*var tmp = list
              val test = tmp.filterNot((elt:(Int,Int))=> elt._2<pos)
              var toReinsert:QList[(Int,Int)]=null
              for(elt <- test){
                if (elt._2>=pos && elt._1>=pos ) toReinsert = QList((s.oldPosToNewPos(elt._1).get, s.oldPosToNewPos(elt._2).get), toReinsert)
                else if (elt._1<pos && elt._2>=pos) toReinsert = QList((elt._1, s.oldPosToNewPos(elt._2).get), toReinsert)
              }
              tmp = tmp.diff(test)
              var iter = toReinsert.iterator
              while(iter.hasNext){
                val item = iter.next()
                tmp = insertInList(tmp,item._1,item._2)
              }
              val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,changes.newValue, v)
              insertInList(tmp,pos,math.min(pos+1, if( car != v-1 ) startPosOfVehicle(car+1)-1 else changes.newValue.size-1))
            */
              updateListOfZoneToUpdateAndSearchZoneToUpdateAfterInsert()

        }
      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case list =>
            val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
            for(id<- car+1 until v) startPosOfVehicle.update(id,startPosOfVehicle(id)-1)
           // println("remov pose "+pos)



           /* if (elt._1>pos )  toReinsert =  QList((r.oldPosToNewPos(elt._1).get, r.oldPosToNewPos(elt._2).get), toReinsert) // cas base update
            else if (elt._1<pos && elt._2>pos)  toReinsert =   QList((elt._1, r.oldPosToNewPos(elt._2).get), toReinsert)  //remov dans zone
            else if( elt._1==pos && elt._2 > pos)  toReinsert =   QList((elt._1+1, r.oldPosToNewPos(elt._2).get), toReinsert)  //start remove
            else if( elt._1 < pos &&  elt._2==pos)  toReinsert =   QList((elt._1, elt._2-1), toReinsert) // end remove*/

            def updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null):List[(Int,Int)]= {
              if (list.isEmpty && lastZone!=null){
                if(lastZone._2<pos-1) {
                  val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                  var tmpLastZone = (pos, pos)
                  // println("myCond = "+((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size))))
                  return if((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size)))
                    smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                  else  list
                } else list
              } else if(list.isEmpty && lastZone==null){
                val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                var tmpLastZone = (pos, pos)
               // println("myCond = "+((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size))))
                return if((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size)))
                   smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                else  list
              } else {
                val currentZone = list.head
                // println(currentZone + " pos inser " + pos + "seq " + s.newValue.mkString(","))
                var start = currentZone._1
                val tmp = r.oldPosToNewPos(currentZone._2)
                var end = if(tmp.nonEmpty) tmp.get else 0
                if (currentZone._1 > pos) { // ==> juste mettre a jour pcq on delace de 1 ers gauche
                  // check si last est plus petit de pos ==> insert
                  start = r.oldPosToNewPos(currentZone._1).get
                  val zone = (start, end)
                  return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), zone))

                } else if (currentZone._1 == pos ) { // remove start
                  val zone = (start, end)
                  if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), zone))
                  else  return updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), lastZone)
                } else { // pos est après cur1
                  if(currentZone._2<pos){ // pos plus loin
                    return smartPrepend(currentZone._1,currentZone._2,updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1),currentZone))
                  } else if(currentZone._2==pos ){ // end = remove
                    end = r.oldPosToNewPos(currentZone._2-1).get
                    val zone = (start, end) // attention
                    if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), zone))
                    else  return updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), lastZone)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), zone))
                  } else { // remove dans zone
                  //println("pos "+pos+" end "+end)
                    val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(list.drop(1), zone))
                  }
                }
              }
            }



           /* def updateListOfZoneToUpdateAfterRemoveWhitIndex(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null,inde:Int=0):List[(Int,Int)]= {
              if (list.isEmpty && lastZone!=null){
                if(lastZone._2<pos-1) {
                  val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                  var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                  return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                } else list
              } else if(list.isEmpty && lastZone==null){
                val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                var tmpLastZone = (pos, pos)
                // println("myCond = "+((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size))))
                return if((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size)))
                  smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                else  List.empty[(Int,Int)]
              } else {
                val currentZone = list.head
                // println(currentZone + " pos inser " + pos + "seq " + s.newValue.mkString(","))
                var start = currentZone._1
                val tmp = r.oldPosToNewPos(currentZone._2)
                var end = if(tmp.nonEmpty) tmp.get else 0
                if (currentZone._1 > pos) { // ==> juste mettre a jour pcq on delace de 1 ers gauche
                  // check si last est plus petit de pos ==> insert
                  start = r.oldPosToNewPos(currentZone._1).get
                  val zone = (start, end)
                  return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), zone))

                } else if (currentZone._1 == pos ) { // remove start
                val zone = (start, end)
                  if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), zone))
                  else  return updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), lastZone)
                } else { // pos est après cur1
                  if(currentZone._2<pos){ // pos plus loin
                    println(inde)
                    updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1),currentZone,inde+1)
                   // smartPrepend(currentZone._1,currentZone._2,updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1),currentZone))
                  } else if(currentZone._2==pos ){ // end = remove
                    end = r.oldPosToNewPos(currentZone._2-1).get
                    val zone = (start, end) // attention
                    if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), zone))
                    else  return updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), lastZone)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), zone))
                  } else { // remove dans zone
                  //println("pos "+pos+" end "+end)
                  val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterRemoveWhitIndex(list.drop(1), zone))
                  }
                }
              }
            }*/


            /*
              //var tmp:List[(Int,Int)] = if(pos >0) List.apply((0,0)) else list
              var tmp = list
              val toRetour2 = updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove(tmp)

              var test = tmp.filterNot((elt:(Int,Int))=> elt._2<pos)
              var toReinsert:QList[(Int,Int)]=null
              for(elt <- test){
                if (elt._1>pos )  toReinsert =  QList((r.oldPosToNewPos(elt._1).get, r.oldPosToNewPos(elt._2).get), toReinsert)
                else if (elt._1<pos && elt._2>pos)  toReinsert =   QList((elt._1, r.oldPosToNewPos(elt._2).get), toReinsert)
                else if( elt._1==pos && elt._2 > pos)  toReinsert =   QList((elt._1+1, r.oldPosToNewPos(elt._2).get), toReinsert)
                else if( elt._1 < pos &&  elt._2==pos)  toReinsert =   QList((elt._1, elt._2-1), toReinsert)
              }
              tmp = tmp.diff(test)
              var iter = toReinsert.iterator
              while(iter.hasNext){
                val item = iter.next()
                tmp = insertInList(tmp,item._1,item._2)
              }
              var cond1 = (pos < changes.newValue.size)
              var cond2 = (car != v-1 && pos!=startPosOfVehicle(car+1))
            //  println("conde  = "+((cond2 || (car == v-1 && cond1))))
              val toRe = if(cond2 || (car == v-1 && cond1)) insertInList(list,pos,pos) else tmp
              //require(toRe.equals(toRetour2))
             if(toRetour2.diff(toRe).nonEmpty){ println("pos "+pos+" list "+list.mkString(",")+"\n"+ toRetour2.diff(toRe) +" old : "+toRe.mkString(",")+"<- new >"+toRetour2.mkString(",")+"\nseq "+changes)
              System.exit(0)}
              toRe
            */ updateListOfZoneToUpdateAndSearchZoneToUpdateAfterRemove()


        }
      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdate(prev) match{
          case null => null
          case list =>


            /* TODO draft
            def updateListOfZoneToUpdateAfterMove(list:List[(Int,Int)]=list,lastZone :(Int,Int)=null):List[(Int,Int)]= {
              if (list.isEmpty && lastZone!=null){
                if(lastZone._2<pos-1) {
                  val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                  var tmpLastZone = (pos, math.min(pos + 1, if (car != v - 1) startPosOfVehicle(car + 1) - 1 else changes.newValue.size - 1))
                  return smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                } else list
              } else if(list.isEmpty && lastZone==null){
                val car = RoutingConventionMethods.searchVehicleReachingPosition(pos,prev.newValue, v)
                var tmpLastZone = (pos, pos)
                // println("myCond = "+((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size))))
                return if((car != v-1 && pos!=startPosOfVehicle(car+1)) || (car == v-1 && (pos < changes.newValue.size)))
                  smartPrepend(tmpLastZone._1,tmpLastZone._2, List.empty[(Int,Int)])
                else  List.empty[(Int,Int)]
              } else {
                val currentZone = list.head
                // println(currentZone + " pos inser " + pos + "seq " + s.newValue.mkString(","))
                var start = currentZone._1
                val tmp = m.oldPosToNewPos(currentZone._2)
                var end = if(tmp.nonEmpty) tmp.get else 0
                if (currentZone._1 > pos) { // ==> juste mettre a jour pcq on delace de 1 ers gauche
                  // check si last est plus petit de pos ==> insert
                  start = m.oldPosToNewPos(currentZone._1).get
                  val zone = (start, end)
                  return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterMove(list.drop(1), zone))

                } else if (currentZone._1 == pos ) { // remove start
                val zone = (start, end)
                  if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterMove(list.drop(1), zone))
                  else  return updateListOfZoneToUpdateAfterMove(list.drop(1), lastZone)
                } else { // pos est après cur1
                  if(currentZone._2<pos){ // pos plus loin
                    return smartPrepend(currentZone._1,currentZone._2,updateListOfZoneToUpdateAfterMove(list.drop(1),currentZone))
                  } else if(currentZone._2==pos ){ // end = remove
                    end = m.oldPosToNewPos(currentZone._2-1).get
                    val zone = (start, end) // attention
                    if(currentZone._2>currentZone._1) return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterMove(list.drop(1), zone))
                    else  return updateListOfZoneToUpdateAfterMove(list.drop(1), lastZone)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterMove(list.drop(1), zone))
                  } else { // remove dans zone
                  //println("pos "+pos+" end "+end)
                  val zone = (start, end)
                    return smartPrepend(zone._1,zone._2, updateListOfZoneToUpdateAfterMove(list.drop(1), zone))
                  }
                }
              }
            }
*/


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
              tmp = insertInList(tmp,item._1,item._2)
            }















            // maj position des marqueur de vehicule
            if (initVehicule!= destVehicule) {
              var car = if(case1) destVehicule+1 else initVehicule+1
              while (car <= (if(case1) initVehicule else destVehicule) ) {
                startPosOfVehicle.update(car, m.oldPosToNewPos(startPosOfVehicle(car)).get)
                car += 1
              }
            }
            val vehiculeOfSrc = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,fromIncluded-1)
            if(case1) {
              var dst =  if (destVehicule != v - 1)  math.min(m.oldPosToNewPos(after+1).get, startPosOfVehicle(destVehicule + 1)-1) // juste pour savoir si
              else  math.min(m.oldPosToNewPos(after+1).get, routes.newValue.size-1)
              tmp = if (flip) insertInList(tmp, m.oldPosToNewPos(toIncluded).get, dst) else insertInList(insertInList(tmp, m.oldPosToNewPos(fromIncluded).get
                , m.oldPosToNewPos(fromIncluded).get), dst,dst)

              // calculer la valeur du noeud qu'on avait juste après toIncluded (avant le move)

              // si le noeud aprs toinclude n'est pas un marqueur OU s'il y a un node apres toincluded (i.e. c'est pas le dernier node de la seq)
              dst = if ((vehiculeOfSrc == v - 1 || (toIncluded + 1) < startPosOfVehicle(vehiculeOfSrc + 1)) && toIncluded+1 <= routes.newValue.size-1) toIncluded+1
              // sinon on a rien a calculer
              else -1
              if(dst != -1) tmp =insertInList(tmp, dst,dst)

            } else {
              var dst = if (vehiculeOfSrc == v - 1 || m.oldPosToNewPos(toIncluded + 1).get < startPosOfVehicle(vehiculeOfSrc + 1) )// s'il n'y a pas de marqueur ou qu'on est pas sur un marqueur
                fromIncluded
              else -1 //sinon par de calcule
              if(dst != -1) tmp = insertInList(tmp, dst,dst)
              val vehiculeOfNodeFollowingAfter = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,after+1)
              dst = if((after == routes.newValue.size-1) || ( vehiculeOfNodeFollowingAfter!=destVehicule ))  after else after+1
              tmp = if (flip) insertInList(tmp, m.oldPosToNewPos(toIncluded).get, dst) else
                insertInList(insertInList(tmp, m.oldPosToNewPos(after).get+1, m.oldPosToNewPos(after).get+1), dst,dst)
            }
            tmp
        }
      case SeqUpdateAssign(value : IntSequence) => null
      case SeqUpdateLastNotified(value:IntSequence) =>
        require (value quickEquals routes.value)
        List.empty[(Int,Int)]
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
      if(current.value < v){
        //sauver valeur du dernier node du vehicule
        currentCar = current.value
        startPosOfVehiculeInternal(currentCar) =current.position
        valueOfCurrentNode = initValue(current.value)
        setRemainingCapacityOfNode(current.value, valueOfCurrentNode,outputInternal)
      }
      //sinon on continue sur le meme vehicule
      else{
        valueOfCurrentNode =  op(previous.value,current.value,valueOfPresiousNode)
        setRemainingCapacityOfNode(current.value, valueOfCurrentNode,outputInternal )
      }
    }
  }







  /**
    * Computes the capacity of nodes concerned by a SeqUpdate
    * @param s the sequence after the SeqUpdate
    * @param lst the list containing positions where calculations must be performed
    */
  def updateContentForSelectedZones(s:IntSequence, lst: List[(Int, Int)]){
    //println(lst.mkString(","))
    val iter = lst.toIterator
    var pair = iter.next()
    var start = pair._1
    var end = pair._2
    pair = if(iter.hasNext)  iter.next() else null

    var current = s.explorerAtPosition(start).get// first is mandatory ...
    // limite sup pour le noeud
    var upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position) != v-1)
      startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position)+1)
    else routes.newValue.size
    // recup noeud précédent
    var previousPos = s.explorerAtPosition(startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,current.position))).get
    // valeur du noeud précédent
    var valueOfPreviousNode=initValue(previousPos.value)
    // maj noeud precedent s'il existe
    if(current.position > previousPos.position) {
      previousPos = s.explorerAtPosition(current.position-1).get
      valueOfPreviousNode = getRemainingCapacityOfNode(previousPos.value) }
    // calcule  valeur du noeud
    var valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
    var oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)

    // verif condition si on a qu'un seul noeud dans la zone mandat
    var cdt:Boolean = if(current.position==end ) /* si j'ai (deja) finis  ==> verif cdt */ valueOfCurrentNode==oldValueOfCurrentNode else  false

    //maj capa
    setRemainingCapacityOfNode(current.value, valueOfCurrentNode)
    // si fin de zone et qu'il en reste encore
    if(current.position==end && pair != null){
      // si on peut arreter pour cette zone ==> recup zone suivant sinon on fait rien et on continue
      if((cdt || current.position==upperBound-1 )){
        //  println("next Pair ")
        cdt = false
        end = pair._2
        start = pair._1
        current= s.explorerAtPosition(start-1).get // if(start >= current.position) newRoute.explorerAtPosition(start-1).get else current
        upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
          startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
        else routes.newValue.size
        pair= if(iter.hasNext)  /* recupere pair suivant si elle existe*/ iter.next() else null
      }
    }

    // tant qu'on doit continuer et qu'on depasse pas le vehicule des noeud qu'on veut maj
    while(!cdt && current.position<upperBound-1 ){
      // maj noeud precedent
      previousPos=current
      //maj noeud courant
      current = current.next.get

      // tant qu'on est dans la zone mandat
      if(current.position <= end){
        valueOfPreviousNode=getRemainingCapacityOfNode(previousPos.value)
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)
        setRemainingCapacityOfNode(current.value,valueOfCurrentNode)
        if(current.position==end ){
          cdt=  valueOfCurrentNode==oldValueOfCurrentNode
          if((cdt || current.position==upperBound-1 ) && pair != null){
            cdt = false
            end = pair._2
            start = pair._1
            current= s.explorerAtPosition(start-1).get
            upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
              startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
            else routes.newValue.size
            pair= if(iter.hasNext) iter.next() else null
          }
        }

      } else{ //sinon
        if(pair != null && current.position >=pair._1 && current.position ==pair._2 ){ // si je depasse deja la prochaine pair de pos je recupère la suivante
          pair= if(iter.hasNext) iter.next() else null
        }
        valueOfPreviousNode = valueOfCurrentNode
        valueOfCurrentNode = op(previousPos.value,current.value,valueOfPreviousNode)
        oldValueOfCurrentNode = getRemainingCapacityOfNode(current.value)
        //verif cond ( on est plus dans la zone mandat donc dès que cdt ==> on arrete (ou on passe a la zone suivant s'il y en a )
        cdt = valueOfCurrentNode==oldValueOfCurrentNode
        // maj capa si on doit la changer
        //if(!cdt)setRemainingCapacityOfNode(current.value,valueOfCurrentNode)
        setRemainingCapacityOfNode(current.value,valueOfCurrentNode)
        //Si je peut arreter mais que j'ai encore des pair ==> maj
        if((cdt || current.position==upperBound-1 ) && pair != null){
          cdt = false
          end = pair._2
          start = pair._1
          current=  if(start-1 >= current.position) s.explorerAtPosition(start-1).get else current
          upperBound = if(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start) != v-1) // maj upp si change de vehicul
            startPosOfVehicle(RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(s,start)+1)
          else routes.newValue.size
          pair= if(iter.hasNext) iter.next() else null
        }
      }
    }
  }




  /**
    * Insert a new pair into a list or update the list
    * @param list the list in which a pair must be inserted
    * @param start the start position
    * @param end the end position
    * @return the list containing the new pair according right behavior :
    *         =1.= the new pair is contained into a pair already present => do nothing
    *         =2.= the new pair is covered (not all the pair, only a piece)
    *           ==2-I.== [Start -------(NStart ----End] ---------NEnd) ==> [start,NEnd)
    *           ==2-II.== (NStart --------[Start------------NEnd)-------End] ==> (Nstart End]
    *         =3.= the new pair is right next to a old pair => merge both of
    *           ==3-I.== [Start----End](NStart-----NEnd) ==> [Start,NEnd)
    *           ==3-II.== (NStart-----NEnd)[Start----End] ==> (NStart,End]
    *         =4.= add a new pair otherwise
    *         NB : the list is sorted
    */
  private def insertInList(list:List[(Int,Int)],start:Int,end:Int) : List[(Int,Int)] = {

    require(start <= end)
    var concernedByNewPair = list.filterNot((elt:(Int,Int))=> elt._1> end+1 || elt._2< start-1)
    var effectiveStart = if(!concernedByNewPair.isEmpty ) math.min(start,concernedByNewPair.head._1)  else start
    var effectiveEnd = if(!concernedByNewPair.isEmpty ) Math.max(end,concernedByNewPair.last._2)  else end
    var listTmp:List[(Int,Int)] = list
    listTmp = listTmp.diff(concernedByNewPair)
    listTmp = (effectiveStart,effectiveEnd) +:listTmp
    listTmp=listTmp.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2)
    listTmp
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