package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.LoopBehavior
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First}

class SwapActivity(schedule: Schedule,
                   neighborhoodName: String,
                   selectIndexBehavior:LoopBehavior = First(),
                   selectSwapBehavior:LoopBehavior = Best(),
                   searchIndices: Option[() => Iterable[Long]] = None)
  extends EasyNeighborhoodMultiLevel[SwapActivityMove](neighborhoodName) {

  var currentIndex: Int = -1
  var swappingIndex: Int = -1

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
      // explore the swappable zone from the current index
      val swappableZone = schedule.swappableIndices(currentIndex)
      val (swappableIterator, notifySwappingFound) = selectSwapBehavior.toIterator(swappableZone)
      while (swappableIterator.hasNext) {
        swappingIndex = swappableIterator.next()
        // Perform move on sequence
        performMove(currentIndex, swappingIndex)
        val newObj = obj.value
        // Rollback to checkpoint
        schedule.activitiesPriorList.rollbackToTopCheckpoint(seqValueCheckPoint)
        // Notification of finding indices
        if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
          notifyIndexFound()
          notifySwappingFound()
        }
      }
    }
    schedule.activitiesPriorList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): SwapActivityMove =
    SwapActivityMove(currentIndex, swappingIndex, this, neighborhoodNameToString, newObj)

  def performMove(currentIndex: Int, swappingIndex: Int): Unit = {
    // Swap 1-segments in sequence
    schedule.activitiesPriorList.swapSegments(currentIndex, currentIndex, false,
      swappingIndex, swappingIndex, false)
  }
}

case class SwapActivityMove(firstIndex: Int,
                            secondIndex: Int,
                            override val neighborhood: SwapActivity,
                            override val neighborhoodName: String = "SwapActivityMove",
                            override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter) {
  override def impactedActivities: Iterable[Int] = IndexedSeq(firstIndex, secondIndex)

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.performMove(firstIndex, secondIndex)
  }
}