package oscar.cp.memScheduling

import oscar.cp.core.CPVarInt

class Activity(startVar: CPVarInt, durVar: CPVarInt, endVar: CPVarInt, n: String = null, existingId: Option[Int] = None)(implicit val scheduler: CPScheduler) {

  // Linking the variables
  scheduler.add(startVar + durVar == endVar)
  // If no id is specified, link the activity to the scheduler and get an id
  val id = existingId.getOrElse(scheduler.addActivity(this))
  // Default name
  val name = Option(n) getOrElse ("Activity " + id)

  // The variables
  def start = startVar
  def end = endVar
  def dur = durVar

  // Earliest starting time
  def est = start.min
  // Latest starting time
  def lst = start.max
  // Earliest completion time assuming the smallest duration
  def ect = end.min
  // Latest completion time assuming the smallest duration
  def lct = end.max

  // Current minimal duration of this activity
  def minDuration = dur.min
  // Current maximal duration of this activity
  def maxDuration = dur.max

  def store = scheduler

  override def toString = name + "(s: " + start + ", d: " + dur + ", e: " + end + ")"

  def needs(resource: UnitResource) {
    resource.addActivity(this)
  }
  
  // Precedences 
  // -----------------------------------------------------------

  def starts(delay: Int) = ActivityPrecedence(startVar, delay)
  def ends(delay: Int) = ActivityPrecedence(endVar, delay)
  def startsExactly(delay: Int) = ActivityPrecedence(startVar, delay, true)
  def endsExactly(delay: Int) = ActivityPrecedence(endVar, delay, true)

  def precedes(act: Activity) = endsBeforeStartOf(act)
  def endsBeforeEndOf(act: Activity) = scheduler.add(endVar <= act.end)
  def endsBeforeStartOf(act: Activity) = scheduler.add(endVar <= act.start)
  def startsBeforeEndOf(act: Activity) = scheduler.add(startVar <= act.end)
  def startsBeforeStartOf(act: Activity) = scheduler.add(startVar <= act.start)

  def endsAtEndOf(act: Activity) = scheduler.add(endVar == act.end)
  def endsAtStartOf(act: Activity) = scheduler.add(endVar == act.start)
  def startsAtEndOf(act: Activity) = scheduler.add(startVar == act.end)
  def startsAtStartOf(act: Activity) = scheduler.add(startVar == act.start)
  
}

object Activity {

  def apply(dur: ImplicitVarInt, n: String = null)(implicit scheduler: CPScheduler) = {

    val durVar = dur.toCPVarInt(scheduler)

    val startVar: CPVarInt = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
    val endVar: CPVarInt = CPVarInt(scheduler, durVar.min to scheduler.horizon)

    new Activity(startVar, durVar, endVar, n)(scheduler)
  }
}