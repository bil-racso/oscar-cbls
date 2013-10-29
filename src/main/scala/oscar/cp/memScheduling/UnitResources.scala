package oscar.cp.memScheduling

object UnitResources {
	def apply(nbResources: Int)(implicit scheduler: CPScheduler): Array[UnitResource] = {
	  Array.tabulate(nbResources)(i => UnitResource()(scheduler))
	}
}