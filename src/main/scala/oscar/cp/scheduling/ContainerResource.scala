package oscar.cp.scheduling

import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class ContainerResource(scheduler : CPScheduler, capa : Int, initStock : Int) extends CumulativeResource(scheduler, capa, true) {

	override def setup() {
		
		if (initStock > 0) {
			val act = Activity(scheduler, 0)
			scheduler.add(act.start == 0)
			activitiesSet += (act -> ProdConsActivity(act, id, initStock))
		}
		
		scheduler.add(cumulative(activities, id, max = capa, min = 0))
	}
}

object ContainerResource {
	
	def apply(scheduler : CPScheduler, capa : Int, initStock : Int = 0) = new ContainerResource(scheduler, capa, initStock)
}