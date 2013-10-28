package oscar.cp.memScheduling

import oscar.cp.core.CPVarInt

class ActivityET(startVar: CPVarInt, durVar: CPVarInt, endVar: CPVarInt, earlyPen: Int, tardiPen:Int, n: String = null, existingId: Option[Int] = None)(implicit Scheduler: CPScheduler) 
			extends Activity(startVar, durVar, endVar, n, existingId) {
  
  def earlinessPenalty = earlyPen
  def tardinessPenalty = tardiPen

}

object ActivityET {
  def apply(dur: ImplicitVarInt, earlyPen: Int, tardiPen: Int, n: String = null)(implicit scheduler: CPScheduler) = {
    val durVar = dur.toCPVarInt(scheduler)

    val startVar: CPVarInt = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
    val endVar: CPVarInt = CPVarInt(scheduler, durVar.min to scheduler.horizon)

    new ActivityET(startVar, durVar, endVar, earlyPen, tardiPen, n)(scheduler)
  }
}