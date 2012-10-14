package oscar.cp.mem

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.pareto.ParetoSet

object DummyProblem extends App {
	
	// Sugar 
	type Sol = Array[Int]
	
	// Problem
	// -------------------------
	
	val nObjs = 2
	val Objs  = 0 until nObjs
	
	val pareto = ParetoSet[Sol](nObjs)
	
	val cp = CPSolver()
	
	val obj1 = CPVarInt(cp, 0 to 20)
	val obj2 = CPVarInt(cp, 0 to 20)
	
	// LNS
	// -------------------------
	
	cp.lns(100, 100) {
		
		// Next not optimal objective 
		val obj = (cp.objective.currentObjectiveIdx + 1)%nObjs
		cp.objective.currentObjective = obj
		
		// Relaxation of the objectives (diversification)
		objRelax(obj)
		
		// Problem relaxation is objRelax in this example
		
	}
	
	def objRelax(obj : Int, intensification : Boolean = false) {
		
		for (o <- Objs) {
			// The -1 avoids to find an already found solution
			if (intensification || o == obj) 
				cp.objective.bestObjs(o) = pareto.currentSol(o) - 1
			else 
				cp.objective.bestObjs(o) = pareto.currentPoint.upperValue(o) - 1
		}
	}
	
	def solFound {
		
		val objs = buildSol
		val sol  = buildSol
		
		// This could be false (framework)
		pareto insert (objs, sol)
		
		// Consider the first objective
		cp.objective.currentObjective = 0
		// Consider the next point in the set
		pareto.nextPoint
	}
	
	def buildSol : Sol = Array(obj1.value, obj2.value)
	
	// Search
	// -------------------------
	
	cp.minimize(obj1, obj2) subjectTo {
		
		cp.post(obj2 - obj1 == 20)
		
	} exploration {
		
		cp.binaryFirstFail(obj1)
		
		solFound
		
		println(obj1.value + " " + obj2.value)
	}
	
	for (i <- 1 to pareto.size) {
		val p = pareto.nextPoint()
		println("sol n¡ " + i + " : " + p(0) + " " + p(1))
	}
}