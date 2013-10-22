package oscar.cp.memScheduling

import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling.UnitResource

object UnitResources {
	def apply(scheduler: CPScheduler, nbResources: Int): Array[UnitResource] = {
	  Array.tabulate(nbResources)(i => UnitResource(scheduler))
	}
}