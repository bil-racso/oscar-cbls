package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import scala.collection.mutable.PriorityQueue
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

abstract class MaxCumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int = 1) extends Constraint(starts.head.store, "Max Cumulative") {

  // Low priority
  priorityL2 = 0

  // The constraint is idempotent
  idempotent = true

  val nTasks = starts.size
  val Tasks = 0 until nTasks

  // The events to process (min heap on date)
  protected val events: PriorityQueue[Event]

  // The tasks to check (max heap on demand)
  private val check: PriorityQueue[(Int, Int)] = new PriorityQueue()
  private val inCheck: Array[Boolean] = Array.fill(nTasks)(false)

  // The tasks in conflicts (min heap on demand)
  private val conflict: PriorityQueue[(Int, Int)] = new PriorityQueue()
  private val inConflict: Array[Boolean] = Array.fill(nTasks)(false)

  // Sweep line status
  private var activeTasks: List[Int] = List()
  private var nActiveTasks: Int = 0
  private var position: Int = 0
  private var nextPosition: Int = 0
  private var gap: Int = 0

  // Avoids the creation of events at runtime
  protected val eventList: Array[EventList]

  // TODO : improve event registration
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      if (!capacity.isBound) capacity.callPropagateWhenBoundsChange(this)
      for (t <- Tasks) {
        if (!starts(t).isBound) starts(t).callPropagateWhenBoundsChange(this)
        if (!ends(t).isBound) starts(t).callPropagateWhenBoundsChange(this)
        if (!demands(t).isBound) starts(t).callPropagateWhenBoundsChange(this)
        if (!resources(t).isBound) starts(t).callPropagateWhenDomainChanges(this)
      }
      Suspend
    }
  }

  override def propagate(): CPOutcome = {

    // Generates the events 
    initEvents()

    // Initializes the sweep line
    check.clear()
    conflict.clear()
    for (t <- Tasks) {
      inCheck(t) = false
      inConflict(t) = false
    }
    position = 0
    nextPosition = 0
    gap = capacity.max

    while (!events.isEmpty) {
      // Handles the event between position and nextPosition
      val (newPosition, newNextPosition) = processEvents()
      // Moves the sweep-line
      position = newPosition
      nextPosition = newNextPosition
      // Filter the earliest starting times
      if (filterMin() == Failure) {
        return Failure
      }
    }

    Suspend
  }

  private def processEvents(): (Int, Int) = {
    position = events.head.date
    val (scp, ecpd, pr) = extractEvents()
    // Processing SCP Events
    for (event <- scp) {
      val oldEct = ends(event.task).min
      if (inConflict(event.task)) {
        // TODO: Check if Failure is possible
        starts(event.task).updateMin(event.date)
        //statusHandler.setReady(t) TODO : Check
      } else if (inCheck(event.task)) {
        //statusHandler.setReady(t) TODO : Check
      }

      if (position < ends(event.task).min) {
        gap = gap - demands(event.task).min
        if (oldEct <= position) {
          events.enqueue(eventList(event.task).buildECPD) // TODO: Check, multi event in the heap
        }
      }
    }

    // Processing ECPD Events
    for (event <- ecpd) {
      if (position < ends(event.task).min) {
        events.enqueue(eventList(event.task).buildECPD) // TODO: Check, multi event in the heap
      } else {
        gap = gap + demands(event.task).min
      }
    }

    // Determine the next event date
    nextPosition = if (events.isEmpty) Int.MaxValue else events.head.date

    // Processing PR Events
    for (event <- pr) {
      if (demands(event.task).min > gap) {
        conflict.enqueue((demands(event.task).min, event.task))
      }
      else if (nextPosition < ends(event.task).min) {       
        check.enqueue((demands(event.task).min, event.task))
      }
    }

    return (position, nextPosition)
  }

  private def filterMin(): CPOutcome = {

    if (gap < 0) Failure 
    else {
      // Tasks no longer in check 
      while (!check.isEmpty) {
        if (check.head._1 > gap) {
          
        }
      }
    }
    
    /*for (r <- Resources) {
      if (gap(r) < 0) return Failure
    }

    // Tasks no longer in check
    for (r <- Resources) {
      if (previousGap(r) > gap(r)) {
        val list = statusHandler.checkTasks
        for (pos <- list) {
          val t = pos.elem
          if (height(t)(r) > gap(r)) {
            if (position < tasks(t).ect) {
              statusHandler.setConflict(t, r)
            } else {
              statusHandler.setReady(t)
            }
          }
        }

        previousGap(r) = gap(r)
      }
    }

    // Tasks no longer in conflict
    for (r <- Resources) {
      if (previousGap(r) < gap(r)) {
        val list = statusHandler.conflictTasks(r)
        for (pos <- list) {
          val t = pos.elem
          if (height(t)(r) <= gap(r)) {
            var res = -1
            for (r <- Resources if height(t)(r) > gap(r)) res = r
            if (res != -1) {
              statusHandler.setConflict(t, res)
            } else {
              val ecp = tasks(t).ect
              tasks(t).adjustStart(position)
              if (nextPosition < tasks(t).ect) statusHandler.setCheck(t)
              else statusHandler.setReady(t)
              if (tasks(t).lst >= ecp && tasks(t).lst < tasks(t).ect) {
                events enqueue ECPD(t, tasks(t).ect)
              }
            }
          }
        }
        previousGap(r) = gap(r)
      }
    }*/

    Suspend
  }

  private def extractEvents(): (List[Event], List[Event], List[Event]) = {
    extractEvents0(List(), List(), List())
  }

  private def extractEvents0(scp: List[Event], ecpd: List[Event], pr: List[Event]): (List[Event], List[Event], List[Event]) = {
    if (events.isEmpty || events.head.date > position) (scp, ecpd, pr)
    else {
      val event = events.dequeue()
      if (event.isSCP) extractEvents0(event :: scp, ecpd, pr)
      else if (event.isECPD) extractEvents0(scp, event :: ecpd, pr)
      else if (event.isPR) extractEvents0(scp, ecpd, event :: pr)
      else sys.error("event does not exist")
    }
  }

  private def initEvents(): Unit = {
    // Events could remain in the structure in case of a previous Failure
    events.clear()
    for (t <- Tasks) {
      if (resources(t).isBoundTo(id)) {
        // SCP
        events.enqueue(eventList(t).buildSCP)
        // ECPD (if compulsory part)
        if (starts(t).max < ends(t).min) {
          events.enqueue(eventList(t).buildECPD)
        }
        // PR (if not fixed)
        if (!starts(t).isBound) {
          events.enqueue(eventList(t).buildPR)
        }
      }
    }
  }

}

abstract class Event(val task: Int, val dec: Int, val date: Int) {
  def isSCP: Boolean
  def isECPD: Boolean
  def isPR: Boolean
}

abstract class EventList {
  val buildSCP: Event
  val buildECPD: Event
  val buildPR: Event
}

