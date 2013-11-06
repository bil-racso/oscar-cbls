package oscar.cp.memScheduling

import oscar.cp.scheduling.ImplicitVarInt
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling.Activity

object Activities {
	
  def apply(durations : Array[ImplicitVarInt])(implicit scheduler: CPScheduler): Array[Activity] = {
	  Array.tabulate(durations.length)(i => Activity(scheduler, durations(i)))
	}
}