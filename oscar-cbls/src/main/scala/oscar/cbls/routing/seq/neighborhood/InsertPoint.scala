package oscar.cbls.routing.seq.neighborhood

import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.algo.{IdenticalAggregator, HotRestart}
import oscar.cbls.search.core.EasyNeighborhood

/**
 * base class for point insertion moves
 * @author renaud.delandtsheer@cetic.be
 */
abstract class InsertPoint(vrp: VRP,
                           neighborhoodName: String,
                           best: Boolean)
  extends EasyNeighborhood[InsertPointMove](best,neighborhoodName){

  val v = vrp.v
  val seq = vrp.routes

  var insertAtPosition:Int = 0
  var insertedPoint:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    InsertPointMove(insertedPoint, insertAtPosition, newObj, this, vrp, neighborhoodNameToString)

  def doMove(insertedPoint: Int, insertAtPosition:Int) {
    seq.insertAtPosition(insertedPoint, insertAtPosition)
  }
}

/**
 * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
 * where u is the numberof unrouted points, and n is the number of routed points
 * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
 * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
 * @param relevantNeighbors a function that, for each unrouted node gives a list of routed node
 *                          such that it is relevant to insert the unrouted node after this routed node
 * @param vrp the routing problem
 * @param neighborhoodName the name of this neighborhood
 * @param best should we search for the best move or the first move?
 * @param hotRestart set to true fo a hot restart fearture on the node to insert
 * @param nodeSymmetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestartOnNextSymmetryClass when you have symmetries among points to insert and hotRestart,
 *                                  this option will try to have the hotRestart starting
 *                                  at a different symmetry class than the last one.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
case class InsertPointUnroutedFirst(unroutedNodesToInsert: () => Iterable[Int],
                                    relevantNeighbors: () => Int => Iterable[Int],
                                    vrp: VRP,
                                    neighborhoodName: String = "InsertPointUnroutedFirst",
                                    best: Boolean = false,
                                    hotRestart: Boolean = true,
                                    nodeSymmetryClass:Option[Int => Int] = None,
                                    hotRestartOnNextSymmetryClass:Boolean = false)
  extends InsertPoint(vrp: VRP,neighborhoodName, best){

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToCurrentCheckpoint(seqValue)
      a
    }

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(unroutedNodesToInsert(), startIndice)
      else unroutedNodesToInsert()

    val iterationScheme = nodeSymmetryClass match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }

    val relevantNeighborsNow = relevantNeighbors()

    val iterationSchemeIterator = iterationScheme.iterator
    while (iterationSchemeIterator.hasNext) {
      insertedPoint = iterationSchemeIterator.next()
      assert(!vrp.isRouted(insertedPoint),
        "The search zone should be restricted to unrouted nodes when inserting.")

      val it2 = relevantNeighborsNow(insertedPoint).iterator
      while (it2.hasNext) {
        val pointToInsertAfter = it2.next()
        seqValue.positionOfAnyOccurrence(pointToInsertAfter) match{
          case None => //not routed?!
          case Some(position) =>
            insertAtPosition = position


            doMove(insertedPoint, insertAtPosition)

            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
              seq.releaseCurrentCheckpointAtCheckpoint()
              startIndice = if (hotRestartOnNextSymmetryClass) {
                if (iterationSchemeIterator.hasNext)
                  iterationSchemeIterator.next()
                else iterationScheme.head
              } else insertedPoint + 1
              return
            }
        }
      }
    }
    seq.releaseCurrentCheckpointAtCheckpoint()
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}


case class InsertPointMove(insertedPoint: Int,
                           insertAtPosition: Int,
                           override val objAfter: Int,
                           override val neighborhood: InsertPoint,
                           vrp:VRP,
                           override val neighborhoodName: String = "InsertPointMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  //TODO
  override def impactedPoints: List[Int] = List(insertedPoint)

  override def commit() {
    neighborhood.doMove(insertedPoint, insertAtPosition)
  }

  override def toString: String =
    neighborhoodName + ":InsertPoint(insertedPoint:" + insertedPoint +
      " insertAtPosition:" + insertAtPosition + objToString + ")"
}
