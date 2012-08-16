package oscar.cp.constraints

import scala.math.max
import scala.math.min

import oscar.cp.core.Store
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.scheduling.MirrorCumulativeActivity

class QuadraticCumulativeEdgeFinding(cp: Store, allTasks : Array[CumulativeActivity], C : Int, r : Int) extends Constraint(cp, "Quadratic Cumulative Edge-Finding") {

	// The tasks
	var tasks = allTasks
	
	var nTasks = tasks.size
	var Tasks  = 0 until nTasks
	
	override def setup(l: CPPropagStrength) : CPOutcome = {
	
		setPriorityL2(0)
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
        		if (!tasks(i).start.isBound) 
        			tasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).dur.isBound)
	        		tasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).end.isBound) 
	        		tasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).height.isBound) 
	        		tasks(i).height.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).resource.isBound)
	        		tasks(i).resource.callPropagateWhenDomainChanges(this)
    		}
        }
        
        return oc   
  	}
	
	override def propagate(): CPOutcome = {
		
		val lToRTasks : Array[CumulativeActivity] = tasks.filter(_.resource.isBoundTo(r))
		val rToLTasks : Array[CumulativeActivity] = lToRTasks.map(new MirrorCumulativeActivity(_))

		// Adjusts starting time
		if (edgeFind(lToRTasks) == CPOutcome.Failure)
			return CPOutcome.Failure
			
		// Adjusts ending time
		if (edgeFind(rToLTasks) == CPOutcome.Failure)
			return CPOutcome.Failure
			
		return CPOutcome.Suspend
	}
	
	private def edgeFind(tasks : Array[CumulativeActivity]): CPOutcome = {
		
		// Init
		val LB    = Array.tabulate(nTasks)(i => tasks(i).est)
		val Dupd  = Array.fill(nTasks)(Int.MinValue)
		val SLupd = Array.fill(nTasks)(Int.MinValue)
		val E     = Array.fill(nTasks)(Int.MinValue)
		
		// Non-decreasing sequences
		val lctList = (0 until nTasks).toArray.sortBy(t => tasks(t).lct)
		val estList = (0 until nTasks).toArray.sortBy(t => tasks(t).est)
		
		var u = 0
		while(u < nTasks) {

			val U = lctList(u)
			
			var energy    = 0
			var maxEnergy = 0
			var r_rho     = Int.MinValue
			
			var ii = nTasks-1
			while (ii >= 0) {
				
				val i = estList(ii)
				
				if (tasks(i).lct <= tasks(U).lct) {
					
					energy += tasks(i).minEnergy
					
					val density1 = energy.toFloat / (tasks(U).lct - tasks(i).est)
					val density2 = maxEnergy.toFloat / (tasks(U).lct - r_rho)
					
					if (density1 > density2) {
						maxEnergy = energy
						r_rho = tasks(i).est
					}
				} else {
					
					val rest = maxEnergy - (C - tasks(i).minHeight)*(tasks(U).lct - r_rho)
					
					if (rest > 0) {
						Dupd(i) = max(Dupd(i), r_rho + (rest.toFloat/tasks(i).minHeight).ceil.toInt)
					}
				}
				
				E(i) = energy
				
				ii -= 1
			}
			
			var minSL = Int.MaxValue
			var r_tau = tasks(U).lct
			
			ii = 0
			while (ii < nTasks) {
				
				val i = estList(ii)
				
				if (C*(tasks(U).lct - tasks(i).est) - E(i) < minSL) {
					r_tau = tasks(i).est
					minSL = C*(tasks(U).lct - r_tau) - E(i)
				}
				
				if (tasks(i).lct > tasks(U).lct) {
					val rest = tasks(i).minHeight*(tasks(U).lct - r_tau) - minSL
					
					if (r_tau < tasks(U).lct && rest > 0)
						SLupd(i) = max(SLupd(i), r_tau + (rest.toFloat/tasks(i).minHeight).ceil.toInt)
						
					if (tasks(i).est + tasks(i).minDuration >= tasks(U).lct || minSL - tasks(i).minEnergy < 0)
						LB(i) = max(LB(i), max(Dupd(i), SLupd(i)))
				}
				
				ii += 1
			}
			
			u += 1
		}
		
		for (i <- Tasks)
			if (tasks(i).adjustStart(LB(i)) == CPOutcome.Failure)
				return CPOutcome.Failure	
        
		return CPOutcome.Suspend
	}

}