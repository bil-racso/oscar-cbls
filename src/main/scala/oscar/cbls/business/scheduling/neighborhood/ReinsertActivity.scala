package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.LoopBehavior
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First}

class ReinsertActivity(schedule: Schedule,
                       neighborhoodName: String,
                       selectIndexBehavior:LoopBehavior = First(),
                       selectReinsertBehavior:LoopBehavior = Best(),
                       searchIndices: Option[() => Iterable[Long]] = None)
  extends EasyNeighborhoodMultiLevel[ReinsertActivityMove](neighborhoodName) {

  var currentIndex: Int = -1
  var reinsertIndex: Int = -1

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    // Iteration zone on activities indices
    // Checking the Hot Restart
    val iterationZone1: () => Iterable[Long] = searchIndices.getOrElse(() => 0L until schedule.activitiesPriorList.value.size.toLong)
    val hotRestart = true
    val iterationZone: Iterable[Long] =
      if (hotRestart) HotRestart(iterationZone1(), currentIndex.toLong)
      else iterationZone1()
    // iterating over the indices in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndexBehavior.toIterator(iterationZone)
    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schedule.activitiesPriorList.defineCurrentValueAsCheckpoint(true)
    // Main loop
    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next().toInt
      // explore the insertable zone of the current index
      val reinsertableZone = schedule.reinsertableIndices(currentIndex)
      val (reinsertableIterator, notifyReinsertFound) = selectReinsertBehavior.toIterator(reinsertableZone)
      while (reinsertableIterator.hasNext) {
        reinsertIndex = reinsertableIterator.next()
        // Perform move on sequence
        performMove(currentIndex, reinsertIndex)
        val newObj = obj.value
        // Rollback to checkpoint
        schedule.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
        // Notification of finding indices
        if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
          notifyIndexFound()
          notifyReinsertFound()
        }
      }
    }
    schedule.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): ReinsertActivityMove =
    ReinsertActivityMove(currentIndex, reinsertIndex, this, neighborhoodNameToString, newObj)

  def performMove(currentIndex: Int, reinsertIndex: Int): Unit = {
    if (currentIndex < reinsertIndex) {
      schedule.activitiesPriorList.swapSegments(currentIndex, currentIndex, false,
        currentIndex+1, reinsertIndex, false)
    }
    else if (reinsertIndex < currentIndex) {
      schedule.activitiesPriorList.swapSegments(reinsertIndex, currentIndex-1, false,
        currentIndex, currentIndex, false)
    }
  }
}

case class ReinsertActivityMove(currentIndex: Int,
                                reinsertIndex: Int,
                                override val neighborhood: ReinsertActivity,
                                override val neighborhoodName: String = "ReinsertActivityMove",
                                override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter) {

  override def impactedActivities: Iterable[Int] = {
    val minIndex = math.min(currentIndex, reinsertIndex)
    val maxIndex = math.max(currentIndex, reinsertIndex)
    for { i <- minIndex to maxIndex } yield i
  }

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.performMove(currentIndex, reinsertIndex)
  }
}