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
import oscar.cbls.algo.seq.functional.{IntSequence, IntSequenceExplorer}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.routing.convention.{ConcreteVehicleLocation, VehicleLocation}

/**
 * Maintains the content of vehicles at each node and the starting position of each vehicle
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
 */
abstract class AbstractVehicleCapacity(n:Int,
                                       v:Int,
                                       op:(Int,Int,Int)=>Int) extends Invariant{

  /**
   * we only require that zoneStart is <= list.head._2 if exists
   * @param zoneStart
   * @param zoneEnd
   * @param list
   * @return
   * @author renaud.delandtsheer@cetic.be
   */
  def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] = {
    require(zoneStart <= zoneEnd)
    assert(list.sortWith((lft : (Int, Int), rgt : (Int, Int)) => lft._1 < rgt._1 && lft._2 < rgt._2).eq(list))
    list match {
      case Nil => (zoneStart, zoneEnd) :: list
      case (oldStart, oldEnd) :: tail =>
        require(zoneStart <= oldEnd)
        if (zoneEnd < oldStart - 1) {
          //the new interval does not touch the old one
          (zoneStart,zoneEnd) :: list
        } else {
          //the new interval touches the old one, there will be some merge
          smartPrepend(Math.min(zoneStart, oldStart), Math.max(zoneEnd, oldEnd), tail)
        }
    }
  }

  private def shiftPlusDelta(l:List[(Int,Int)],delta:Int):List[(Int,Int)] = {
    l match {
      case Nil => l
      case (a, b) :: tail => (a + delta, b + delta) :: shiftPlusDelta(tail,delta)
    }
  }

  def updateZoneToUpdateAfterInsert(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfInsert:Int,
                                    sequenceBeforeInsert:IntSequence,
                                    vehicleLocationBeforeInsert:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfInsert = vehicleLocationBeforeInsert.vehicleReachingPosition(posOfInsert)
    val startPosOfVehicle = vehicleLocationBeforeInsert.startPosOfVehicle(vehicleOfInsert)
    val relativePosOfInsert = posOfInsert - startPosOfVehicle

    def addInsertionIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil => List((relativePosOfInsert, relativePosOfInsert))
        case (startZone, endZone) :: tail =>
          if (relativePosOfInsert < endZone) {
            smartPrepend(relativePosOfInsert, relativePosOfInsert+1, shiftPlusDelta(zonesToUpdate,1))
          } else if (relativePosOfInsert <= endZone + 1){
            //we are in the zone, or the zone can be extended to iclude the insertion point
            (startZone, endZone + 1) :: shiftPlusDelta(tail,1)
          } else {
            assert(relativePosOfInsert > endZone + 1)
            (startZone, endZone) :: addInsertionIntoZonesToUpdate(tail)
          }
      }
    }
    zoneToUpdate.insert(vehicleOfInsert, addInsertionIntoZonesToUpdate(zoneToUpdate.getOrElse(vehicleOfInsert, List.empty[(Int, Int)])))
  }


  def updateZoneToUpdateAfterRemove(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfRemove:Int,
                                    sequenceBeforeRemove:IntSequence,
                                    vehicleLocationBeforeRemove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfRemove = vehicleLocationBeforeRemove.vehicleReachingPosition(posOfRemove)
    val startPosOfVehicle = vehicleLocationBeforeRemove.startPosOfVehicle(vehicleOfRemove)
    val relativePosOfRemove = posOfRemove - startPosOfVehicle

    def addRemoveIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil => List((relativePosOfRemove, relativePosOfRemove))
        case (startZone, endZone) :: tail =>
          if (relativePosOfRemove + 1 < startZone) {
            smartPrepend(relativePosOfRemove , relativePosOfRemove, shiftPlusDelta(zonesToUpdate,-1))
          } else if (relativePosOfRemove - 1 <= endZone){
            //we are in the zone, or the zone can be extended to include the insertion point
            (startZone, endZone - 1) :: shiftPlusDelta(tail,-1)
          } else {
            assert(vehicleOfRemove > endZone + 1)
            smartPrepend(startZone, endZone, addRemoveIntoZonesToUpdate(tail))
          }
      }
    }
    zoneToUpdate.insert(vehicleOfRemove, addRemoveIntoZonesToUpdate(zoneToUpdate.getOrElse(vehicleOfRemove, List.empty[(Int, Int)])))
  }


  // @Note => O(listToInsert+toInsert)
  def insertInList(listToInsert: List[(Int, Int)], toInsert: List[(Int, Int)]): List[(Int, Int)] = {
    listToInsert match {
      case Nil => toInsert
      case (start, end) :: tail =>
        toInsert match {
          case Nil => listToInsert
          case (s, e) :: tail =>
            if (s > end) smartPrepend(start, end, insertInList(listToInsert.drop(1), toInsert))
            else if (s == start && end == e) insertInList(listToInsert, toInsert.drop(1))
            else smartPrepend(s, e, insertInList(listToInsert, toInsert.drop(1)))
        }
    }
  }




  /*
    /**
     * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the move
     * @param zonesToUpdate
     * @param m
     * @param sequenceBeforeMove
     * @param vehicleLocationBeforeMove
     * @return
     */
    def updateZoneToUpdateAfterMove(zonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    m:SeqUpdateMove,
                                    sequenceBeforeMove:IntSequence,
                                    vehicleLocationBeforeMove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {

      if(zonesToUpdate==null) return null
      if ( m.isNop) return zonesToUpdate

      val fromVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.fromIncluded)
      val startPositionOfFromVehicle = vehicleLocationBeforeMove.startPosOfVehicle(fromVehicle)
      val relativeFromIncluded = m.fromIncluded - startPositionOfFromVehicle
      val relativeToIncluded = m.toIncluded - startPositionOfFromVehicle

      if (m.isSimpleFlip) {// si la valeur a la position toincluded a changer on va d'office recalculer le noeud suivant, pas besoins de l'inclure ici
        zonesToUpdate.insert(fromVehicle,
          insertInList(zonesToUpdate.getOrElse(fromVehicle, List.empty[(Int, Int)]),
            List.apply((relativeFromIncluded,relativeToIncluded))))
      } else {
        val toVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.after)
        val relativeAfter = m.after - vehicleLocationBeforeMove.startPosOfVehicle(toVehicle)
        val delta = m.toIncluded - m.fromIncluded + 1


        def updateListOfZoneToUpdateAfterMove(listOfZonesForVehicle: List[(Int, Int)], sourceSide: Boolean = true, toReinsertInTheOtherSide:List[(Int, Int)] = List.empty[(Int, Int)]): (List[(Int, Int)],List[(Int, Int)]) = {
          listOfZonesForVehicle match {
            case Nil => (listOfZonesForVehicle,toReinsertInTheOtherSide)
            case (startZone, endZone) :: tail =>
              if (sourceSide) {
                // traitement du coté [from,to]
                if (endZone < relativeFromIncluded) (smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail,true,toReinsertInTheOtherSide)._1),toReinsertInTheOtherSide) // on avance
                else if (startZone > relativeToIncluded) (smartPrepend(startZone - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail,true,toReinsertInTheOtherSide)._1),toReinsertInTheOtherSide) // on decale
                else { // on gere les deplacement due au mouvement
                val toReinsertInTheOtherSideInternal = if (!m.flip) (Math.max(startZone, relativeFromIncluded) - relativeFromIncluded, Math.min(relativeToIncluded, endZone) - relativeFromIncluded) :: toReinsertInTheOtherSide else toReinsertInTheOtherSide
                  val toReturn = if (endZone > relativeToIncluded) (smartPrepend(relativeToIncluded + 1 - delta, endZone - delta, updateListOfZoneToUpdateAfterMove(tail,true,toReinsertInTheOtherSideInternal)._1),toReinsertInTheOtherSide)
                  else updateListOfZoneToUpdateAfterMove(tail,true,toReinsertInTheOtherSideInternal)
                  if (startZone >= relativeFromIncluded) toReturn
                  else (smartPrepend(startZone, relativeFromIncluded - 1, toReturn._1),toReturn._2)
                }
              } else { // traitement coté after
                if (endZone <= relativeAfter) (smartPrepend(startZone, endZone, updateListOfZoneToUpdateAfterMove(tail, sourceSide)._1),toReinsertInTheOtherSide) // on avance
                else if (startZone > relativeAfter) (smartPrepend(startZone + delta, endZone + delta, updateListOfZoneToUpdateAfterMove(tail, sourceSide)._1),toReinsertInTheOtherSide) // on decale
                else (smartPrepend(startZone, relativeAfter, smartPrepend(relativeAfter + 1 + delta, endZone + delta, updateListOfZoneToUpdateAfterMove(tail, sourceSide)._1)),toReinsertInTheOtherSide) // si la zone contient after+1
              }
          }
        }
        val updatedListOfZones = updateListOfZoneToUpdateAfterMove(zonesToUpdate.getOrElse(fromVehicle, List.empty[(Int, Int)]))
        val listsOfZoneWhenSourceVehicleListUpdated = zonesToUpdate.insert(fromVehicle, updatedListOfZones._1)
        val listsOfZoneWhenDestinationVehicleListUpdated = listsOfZoneWhenSourceVehicleListUpdated.insert(toVehicle, updateListOfZoneToUpdateAfterMove(listsOfZoneWhenSourceVehicleListUpdated.getOrElse(toVehicle, List.empty[(Int, Int)]), false)._1) //, startPosOfVehicle(toVehicle)

        pushOnTopOfStack((pos) => m.oldPosToNewPos(pos))

        val newRelativeAfter = m.oldPosToNewPos(m.after).get - positionOfVehicle(toVehicle)

        val toReinsertInTheDestinationSideList: List[(Int, Int)] = updatedListOfZones._2.mapConserve((elt:(Int,Int))=>((newRelativeAfter + 1 + elt._1), (newRelativeAfter + 1 + elt._2)))

        val fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone :(RedBlackTreeMap[List[(Int, Int)]],Int,Int) = if (m.moveUpwards) {
          // on sait qu'on a des noeuds après toIncluded vu qu'on deplace vers la droite ;)  on va juste check si toincluded+1 est un tag ou pas
          (if (fromVehicle == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < positionOfVehicle(fromVehicle + 1)) // si c'est faux alors toIncluded+1 est un tag de vehicle
            listsOfZoneWhenDestinationVehicleListUpdated.insert(fromVehicle, insertInList(listsOfZoneWhenDestinationVehicleListUpdated.getOrElse(fromVehicle, List.empty[(Int, Int)]), List.apply((m.fromIncluded - positionOfVehicle(fromVehicle), m.fromIncluded - positionOfVehicle(fromVehicle)))))
          else listsOfZoneWhenDestinationVehicleListUpdated
            , if ((m.after == routes.newValue.size - 1) || m.prev.newValue.valueAtPosition(m.after + 1).get < v) newRelativeAfter + delta else newRelativeAfter + delta + 1
            , newRelativeAfter + 1)
        } else {
          //on verifie s'il reste des noeuds après toincluded (et on verifie si c'est pas un tag ) ou si toinclude est le denier noeud de la seq
          (if ((fromVehicle == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < positionOfVehicle(fromVehicle + 1)) && m.toIncluded + 1 <= routes.newValue.size - 1)
            listsOfZoneWhenDestinationVehicleListUpdated.insert(fromVehicle, insertInList(listsOfZoneWhenDestinationVehicleListUpdated.getOrElse(fromVehicle, List.empty[(Int, Int)]), List.apply((m.toIncluded + 1 - positionOfVehicle(fromVehicle), m.toIncluded + 1 - positionOfVehicle(fromVehicle)))))
          else listsOfZoneWhenDestinationVehicleListUpdated
            , math.min(m.after+delta+1, if (toVehicle != v - 1) positionOfVehicle(toVehicle + 1) - 1 else routes.newValue.size - 1) - positionOfVehicle(toVehicle)
            , m.after+1 - positionOfVehicle(toVehicle))//require(m.oldPosToNewPos(m.fromIncluded).get == m.after+1)
        }
        fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._1.insert(toVehicle, insertInList(fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._1.getOrElse(toVehicle, List.empty[(Int, Int)]),  //  on insert une nouvelle zone sur vehicul destination
          if (m.flip) List.apply((fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3, fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2))  //  si c'est flip alors on sait que toReinsertInTheDestinationSideList est vide donc on rajoute directement une zone
          else (fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3,fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3) :: toReinsertInTheDestinationSideList ::: List.apply((fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2, fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2))))// sinon on rajoute les 2 nouvelle position dans toReinsertInTheDestinationSideList et on l'injecte
      }
    }
  */

  /**
   * Updates vehicles starting positions and list zones of position of nodes which content have to be updated after the move
   * @param zonesToUpdate
   * @param m
   * @param sequenceBeforeMove
   * @param vehicleLocationBeforeMove
   * @return
   */
  def updateZoneToUpdateAfterMove(zonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                  m:SeqUpdateMove,
                                  sequenceBeforeMove:IntSequence,
                                  vehicleLocationBeforeMove:VehicleLocation,
                                  vehicleLocationAfterMove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if(zonesToUpdate==null) return null
    if (m.isNop) return zonesToUpdate

    val sourceVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.fromIncluded)
    val startPositionOfSourceVehicle = vehicleLocationBeforeMove.startPosOfVehicle(sourceVehicle)
    val relativeFromIncluded = m.fromIncluded - startPositionOfSourceVehicle
    val relativeToIncluded = m.toIncluded - startPositionOfSourceVehicle

    if (m.isSimpleFlip) {
      //in case of flip, we must to update all nodes in the flip and the node after the flip
      // (this node after might not exist, actually)
      val shouldNextNodeBeIncluded = m.prev.newValue.valueAtPosition(m.toIncluded+1) match{
        case Some(x) if x >= v => true
        case _ => false
      }
      zonesToUpdate.insert(sourceVehicle,
        insertInList(zonesToUpdate.getOrElse(sourceVehicle, List.empty),
          List((relativeFromIncluded,if(shouldNextNodeBeIncluded) relativeToIncluded + 1 else relativeToIncluded))))
    } else {
      val destinationVehicle = vehicleLocationBeforeMove.vehicleReachingPosition(m.after)
      val relativeAfter = m.after - vehicleLocationBeforeMove.startPosOfVehicle(destinationVehicle)
      val nbPointsInMovedSegment = m.nbPointsInMovedSegment

      /**
       * @param listOfZonesForVehicle the list of zones to update on vehicleFrom
       * @return the lsit of zone where the segment of the move has bee nremoved
       *         and the list of zones that are within the moved segment, not including the first position in this segment unless covered by zone in the input
       */
      def removeMovedZoneFromZonesToUpdate(listOfZonesForVehicle: List[(Int, Int)]): (List[(Int, Int)],List[(Int, Int)]) = {
        listOfZonesForVehicle match {
          case Nil => (Nil,Nil)
          case (startZone, endZone) :: tail =>
            // traitement du coté [from,to]
            if (endZone < relativeFromIncluded) {
              // on est avant from
              val(updatedZones,removedZones) = removeMovedZoneFromZonesToUpdate(tail)
              (smartPrepend(startZone, endZone, updatedZones), removedZones)

            }else if (startZone > relativeToIncluded) {
              // on est après to
              val (updatedTail,removedZones) = removeMovedZoneFromZonesToUpdate(tail)
              (smartPrepend(startZone - nbPointsInMovedSegment, endZone - nbPointsInMovedSegment, updatedTail), removedZones)

            }else {
              // si overlape ==> (min,max) découpe au bon endroit

              val coupleFromRemovedZone =
                (Math.max(startZone, relativeFromIncluded) - relativeFromIncluded,
                  Math.min(relativeToIncluded, endZone) - relativeFromIncluded)

              val (updatedTail,removedZones) = removeMovedZoneFromZonesToUpdate(tail)
              val updatedRemovedZone = coupleFromRemovedZone :: removedZones

              if (endZone > relativeToIncluded) {
                //[from ..... (start ..... to] .... end) ==> overlap
                // on recupère la partie de la zone qui est hors de l'interval et on passe a la suite

                val updatedZones = smartPrepend(relativeToIncluded + 1 - nbPointsInMovedSegment, endZone - nbPointsInMovedSegment, updatedTail)

                if (startZone < relativeFromIncluded) {
                  //(start ..... [from .... end)..... to]  overlap
                  // on récupère la partie de la zone qui est hors de l'interval
                  (smartPrepend(startZone, relativeFromIncluded - 1, updatedZones), updatedRemovedZone)
                }else {
                  // pas d'overlap ===> on a rien a récupéré
                  (updatedZones,updatedRemovedZone)
                }
              }else{
                // pas d'overlap ==> on passe a la paire suivante
                if (startZone < relativeFromIncluded) {
                  //(start ..... [from .... end)..... to]  overlap
                  // on récupère la partie de la zone qui est hors de l'interval
                  (smartPrepend(startZone, relativeFromIncluded - 1, updatedTail), updatedRemovedZone)
                }else {
                  // pas d'overlap ===> on a rien a récupéré
                  (updatedTail,updatedRemovedZone)
                }
              }
            }
        }
      }

      /**
       * this method shifts the zones that will be located after the position where the zones impacted by the move will be inserted, in a second pass...
       * AWFULL!!!!!!!!!!!!!!!!!!!!!
       *
       * @param listOfZonesForVehicle
       * @return
       */
      def updateListOfZoneAfterSideFollowingAMove(listOfZonesForVehicle: List[(Int, Int)]): List[(Int, Int)] = {
        listOfZonesForVehicle match {
          case Nil => listOfZonesForVehicle
          case (startZone, endZone) :: tail =>
            if (endZone <= relativeAfter) smartPrepend(startZone, endZone, updateListOfZoneAfterSideFollowingAMove(tail)) // on avance    /*(start,end)__after */
            else if (startZone > relativeAfter) smartPrepend(startZone + nbPointsInMovedSegment, endZone + nbPointsInMovedSegment, updateListOfZoneAfterSideFollowingAMove(tail)) // on decale  /*after___(start,end) */
            else smartPrepend(startZone, relativeAfter, smartPrepend(relativeAfter + 1 + nbPointsInMovedSegment, endZone + nbPointsInMovedSegment, updateListOfZoneAfterSideFollowingAMove(tail))) // si la zone contient after+1 /*  (start .... after ..... end) */
        }
      }

      val (updatedListOfZonesForVehicleFrom,movedZones) = zonesToUpdate.get(sourceVehicle) match{
        case None => (List.empty,List.empty)
        case Some(l) => removeMovedZoneFromZonesToUpdate(l)
      }

      val listsOfZoneWhenSourceVehicleListUpdated = zonesToUpdate.insert(sourceVehicle, updatedListOfZonesForVehicleFrom)
      val listsOfZoneWhenDestinationVehicleListUpdated =
        listsOfZoneWhenSourceVehicleListUpdated.insert(destinationVehicle, updateListOfZoneAfterSideFollowingAMove(listsOfZoneWhenSourceVehicleListUpdated.getOrElse(destinationVehicle, List.empty[(Int, Int)])))

      val newRelativeAfter = m.oldPosToNewPos(m.after).get - vehicleLocationAfterMove.startPosOfVehicle(destinationVehicle)

      val toReinsertInTheDestinationSideList: List[(Int, Int)] = movedZones.mapConserve((elt:(Int,Int))=> (newRelativeAfter + 1 + elt._1, newRelativeAfter + 1 + elt._2))

      val fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone :(RedBlackTreeMap[List[(Int, Int)]],Int,Int) =
        if (m.moveUpwards) {
          // on sait qu'on a des noeuds après toIncluded vu qu'on deplace vers la droite ;)  on va juste check si toincluded+1 est un tag ou pas
          (if (sourceVehicle == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle + 1)) // si c'est faux alors toIncluded+1 est un tag de vehicle
            listsOfZoneWhenDestinationVehicleListUpdated.insert(sourceVehicle, insertInList(listsOfZoneWhenDestinationVehicleListUpdated.getOrElse(sourceVehicle, List.empty[(Int, Int)]), List.apply((m.fromIncluded - vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle), m.fromIncluded - vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle)))))
          else listsOfZoneWhenDestinationVehicleListUpdated
            , if ((m.after == m.newValue.size - 1) || m.prev.newValue.valueAtPosition(m.after + 1).get < v) newRelativeAfter + nbPointsInMovedSegment else newRelativeAfter + nbPointsInMovedSegment + 1
            , newRelativeAfter + 1)
        } else {
          //on verifie s'il reste des noeuds après toincluded (et on verifie si c'est pas un tag ) ou si toinclude est le denier noeud de la seq
          (if ((sourceVehicle == v - 1 || m.oldPosToNewPos(m.toIncluded + 1).get < vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle + 1)) && m.toIncluded + 1 <= m.newValue.size - 1)
            listsOfZoneWhenDestinationVehicleListUpdated.insert(sourceVehicle, insertInList(listsOfZoneWhenDestinationVehicleListUpdated.getOrElse(sourceVehicle, List.empty[(Int, Int)]), List.apply((m.toIncluded + 1 - vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle), m.toIncluded + 1 - vehicleLocationAfterMove.startPosOfVehicle(sourceVehicle)))))
          else listsOfZoneWhenDestinationVehicleListUpdated
            , math.min(m.after+nbPointsInMovedSegment+1, if (destinationVehicle != v - 1) vehicleLocationAfterMove.startPosOfVehicle(destinationVehicle + 1) - 1 else m.newValue.size - 1) - vehicleLocationAfterMove.startPosOfVehicle(destinationVehicle)
            , m.after+1 - vehicleLocationAfterMove.startPosOfVehicle(destinationVehicle))//require(m.oldPosToNewPos(m.fromIncluded).get == m.after+1)
        }
      fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._1.insert(destinationVehicle, insertInList(fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._1.getOrElse(destinationVehicle, List.empty[(Int, Int)]),  //  on insert une nouvelle zone sur vehicul destination
        if (m.flip) List.apply((fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3, fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2))  //  si c'est flip alors on sait que toReinsertInTheDestinationSideList est vide donc on rajoute directement une zone
        else (fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3,fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._3) :: toReinsertInTheDestinationSideList ::: List.apply((fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2, fromIncludedZoneSideAndtoIncludedZoneSideAndNewlistsOfZone._2))))// sinon on rajoute les 2 nouvelle position dans toReinsertInTheDestinationSideList et on l'injecte
    }
  }



  def insertIntoToUpdateZone(fromPositionRelativeIncluded:Int,toPositionRelativeIncluded:Int,listOfZones:List[(Int,Int)]):List[(Int,Int)] = {
    listOfZones match{
      case Nil => List((fromPositionRelativeIncluded:Int,toPositionRelativeIncluded))
      case (start,end) :: tail =>
        if(end+1 < fromPositionRelativeIncluded){
          //the inserted zone does not touch (start,end), so recurse and prepend
          (start,end) :: insertIntoToUpdateZone(fromPositionRelativeIncluded,toPositionRelativeIncluded,tail)
        }else{
          //the inserted zone is before this one
          smartPrepend(fromPositionRelativeIncluded,toPositionRelativeIncluded,listOfZones)
        }
    }
  }

  def getVehicleContentAtNode(node: Int): Int

  /**
   * sets the conent of the vehicle at node "node"
   * if a node is unrouted, you should call setVehicleContentToUnroutedNode instead
   * @param node the node
   * @param newValueAtNode the new value for the content of the vehicle at this node
   */
  def setVehicleContentAtNode(node: Int, newValueAtNode: Int) : Unit

  def getContentAtVehicleStart(vehicle:Int): Int

  def setVehicleContentAtEnd(vehicle:Int,content:Int)

  def setEndNodeOfVehicle(vehicle:Int,lastNode:Int)

  def setNodesUnrouted(unroutedNodes:Iterable[Int])

  protected def updateVehicleContentOnAllVehicle(s:IntSequence,
                                                 vehiclesToZonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                                 vehicleLocationInSequence:VehicleLocation){

    var tmpExplorer:Option[IntSequenceExplorer] = None

    for((vehicle,sortedZonesToUpdateRelativeToVehicleStartPosition) <- vehiclesToZonesToUpdate.content){
      tmpExplorer = updateVehicleContent(s,
        sortedZonesToUpdateRelativeToVehicleStartPosition,
        vehicleLocationInSequence.startPosOfVehicle(vehicle),
        vehicle,
        tmpExplorer)

      tmpExplorer match{
        case Some(e) if e.position+1 == s.size || (e.position + 1 == vehicleLocationInSequence.startPosOfVehicle(vehicle+1)) =>
          setVehicleContentAtEnd(vehicle,getVehicleContentAtNode(e.value))
          setEndNodeOfVehicle(vehicle,e.value)
        case _ => ;
          //we did not reach th end of the vehicle route in the update, yet the last node might have hanged, so we have to update this
          val positionOfEndNodeOfVehicle = if(vehicle == v-1) s.size-1 else vehicleLocationInSequence.startPosOfVehicle(vehicle+1) -1
          val explorerAtEndNodeOfVehicleOpt = s.explorerAtPosition(positionOfEndNodeOfVehicle)
          val explorerAtEndNodeOfVehicle = explorerAtEndNodeOfVehicleOpt.get
          setEndNodeOfVehicle(vehicle,explorerAtEndNodeOfVehicle.value)
          setVehicleContentAtEnd(vehicle,getVehicleContentAtNode(explorerAtEndNodeOfVehicle.value))
          tmpExplorer = explorerAtEndNodeOfVehicleOpt
      }
    }
  }

  /**
   * performs the update of vehicle content on the mentioned vehicle
   * beware, this does not update the contentAtVehicleEnd; you must do it somewhere else.
   * @param s the sequence
   * @param sortedZonesToUpdateRelativeToVehicleStartPosition a sorted lsit of non-overlapping intervals wit hthe relative positions to update
   * @param startPositionOfVehicle the start position o he vehicle to update
   * @param vehicle the vehicle to update
   * @param explorerToLatestUpdatedPosition an explorer to the latest position that was updated
   * @return the last position that was updated
   */
  private def updateVehicleContent(s:IntSequence,
                                   sortedZonesToUpdateRelativeToVehicleStartPosition: List[(Int, Int)],
                                   startPositionOfVehicle:Int,
                                   vehicle:Int,
                                   explorerToLatestUpdatedPosition:Option[IntSequenceExplorer] = None):Option[IntSequenceExplorer] = {

    sortedZonesToUpdateRelativeToVehicleStartPosition match {
      case Nil => explorerToLatestUpdatedPosition
      case (startCompulsoryRelative, endCompulsoryRelative) :: tail =>
        val startCompulsoryAbsolute = startCompulsoryRelative + startPositionOfVehicle
        val endCompulsoryAbsolute = endCompulsoryRelative + startPositionOfVehicle

        val positionToStartFromAbsolute = explorerToLatestUpdatedPosition match {
          case Some(e) if e.position > startCompulsoryAbsolute => e.position
          case _ => startCompulsoryAbsolute
        }

        val explorerAfterUpdatingThisIntervalOpt =
          if (positionToStartFromAbsolute > endCompulsoryAbsolute) {
            explorerToLatestUpdatedPosition
          } else {
            val explorerToStartUpdate = explorerToLatestUpdatedPosition match {
              case Some(e) if e.position == startCompulsoryAbsolute => e
              case Some(e) if e.position == startCompulsoryAbsolute - 1 => e.next.get
              case _ => s.explorerAtPosition(positionToStartFromAbsolute).get
            }

            if (positionToStartFromAbsolute == startPositionOfVehicle) {
              //we need to update the value at vehicle start
              val newValueAtStart:Int = getContentAtVehicleStart(vehicle)
              val oldValueAtStart = getVehicleContentAtNode(positionToStartFromAbsolute)
              if(newValueAtStart != oldValueAtStart || endCompulsoryAbsolute > 0) {
                //start iterate from here
                setVehicleContentAtNode(positionToStartFromAbsolute, newValueAtStart)
                updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(explorerToStartUpdate,
                  newValueAtStart,
                  endCompulsoryAbsolute, vehicle)
              }else {
                //we do not need to iterate on this because there was no change at this point, and we already reached the end of the compulsory zone
                Some(explorerToStartUpdate)
              }
            } else {
              //we start later than vehicle start
              //so we need to fetch the value at the previous node

              val explorerAtPrev = explorerToStartUpdate.prev.get
              updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(explorerAtPrev,
                getVehicleContentAtNode(explorerAtPrev.value),
                endCompulsoryAbsolute, vehicle)
            }
          }
        //and carry on to next interval
        updateVehicleContent(s,
          tail,
          startPositionOfVehicle,
          vehicle,
          explorerAfterUpdatingThisIntervalOpt)
    }
  }

  /**
   *
   * @param previousUpdatedPosition
   * @param valueAtPreviousUpdatedPosition
   * @param endCompulsoryAbsolute
   *                              @param vehicle the vehicle to which this belongs
   * @return the last position where an update was performed
   */
  private def updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(previousUpdatedPosition:IntSequenceExplorer,
                                                                  valueAtPreviousUpdatedPosition:Int,
                                                                  endCompulsoryAbsolute:Int,vehicle:Int):Option[IntSequenceExplorer] = {
    previousUpdatedPosition.next match{
      case None => None //we'v reached the end of the sequence
      case Some(positionOfCurrent) =>
        val currentNode = positionOfCurrent.value
        if(currentNode < v){
          //we'v just reached another vehicle start
          Some(previousUpdatedPosition)
        }else {
          //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
          val oldValueForCurrentNode = getVehicleContentAtNode(currentNode)
          val newValueForCurrentNode = op(previousUpdatedPosition.value, currentNode, valueAtPreviousUpdatedPosition)

          if (oldValueForCurrentNode != newValueForCurrentNode) {
            setVehicleContentAtNode(currentNode, newValueForCurrentNode)
            updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(
              positionOfCurrent,
              newValueForCurrentNode,
              endCompulsoryAbsolute, vehicle)
          } else if (positionOfCurrent.position < endCompulsoryAbsolute) {
            //thre is no update, but we are in the compulsory zone
            updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(
              positionOfCurrent,
              newValueForCurrentNode,
              endCompulsoryAbsolute, vehicle)
          } else {
            //there is no update, and we are not in the compulsory zone anymore, so we can stop, and return the position
            Some(positionOfCurrent)
          }
        }
    }
  }

  /**
   *Computes content of vehicle and their starting position from scratch
   * @param s the sequence
   * @return (VehicleLocation)
   */
  def computeAndAffectContentAndVehicleStartPositionsFromScratch(s:IntSequence,unrouteAllNodes:Boolean):(ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)

    if(unrouteAllNodes) setNodesUnrouted(0 until n)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)
    setVehicleContentAtNode(0, previousContent)

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          setVehicleContentAtEnd(currentVehicle,previousContent)
          setEndNodeOfVehicle(currentVehicle,previousPosition.value)
          require(currentVehicle == v-1)
          return VehicleLocation(vehicleLocation)
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            setVehicleContentAtEnd(currentVehicle,previousContent)
            setEndNodeOfVehicle(currentVehicle,previousPosition.value)
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            setVehicleContentAtNode(currentNode, previousContent)
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, previousContent, currentNode)
            setVehicleContentAtNode(currentNode, previousContent)
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }

  /**
   *Computes content of vehicle and their starting position from scratch
   * @param s the sequence
   * @return (nodeToContent,vehicleToContentAtEnd,vehicleLocation)
   */
  def computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(s:IntSequence,defaultVehicleContentForUnroutedNodes:Int):(Array[Int],Array[Int],ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)
    val vehicleContent = Array.fill(n)(defaultVehicleContentForUnroutedNodes)
    val vehicleContentAtEndOfRoute = Array.fill(v)(0)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)

    vehicleContent(0) = previousContent

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          vehicleContentAtEndOfRoute(currentVehicle) = previousContent
          require(currentVehicle == v-1)
          return (vehicleContent,vehicleContentAtEndOfRoute,VehicleLocation(vehicleLocation))
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            vehicleContentAtEndOfRoute(currentVehicle) = previousContent
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, previousContent, currentNode)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }
}
