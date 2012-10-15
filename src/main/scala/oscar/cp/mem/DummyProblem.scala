package oscar.cp.mem

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.pareto.ParetoSet
import oscar.visual.VisualPareto

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
	
	var nObjRestart = 0
	
	cp.lns(400, 100) {
		
		// Next objective 
		val obj = nextObj		
		
		// Objective relaxation (diversification)
		objRelax(obj)		
		// Problem relaxation is objRelax in this example
		probRelax(obj)
	}
	
	def nextObj = {
		
		// Next point if no amelioration from the previous point
		if (nObjRestart == nObjs) {
			pareto.nextPoint()
			nObjRestart = 0
		}
			
		nObjRestart += 1
		
		val obj = (cp.objective.currentObjectiveIdx+1)%nObjs
		cp.objective.currentObjective = obj
		obj
	}
	
	def objRelax(obj : Int, intensification : Boolean = false) {
		
		for (o <- Objs) {		
			if (intensification || o == obj) {
				cp.objective.bounds(o)   = pareto.currentPoint(o)
				cp.objective.bestObjs(o) = pareto.currentPoint(o)
			}
			else {
				// The -1 avoid to find an already found solution
				cp.objective.bounds(o)   = pareto.currentPoint.upperValue(o) - 1
				cp.objective.bestObjs(o) = pareto.currentPoint.upperValue(o) - 1
			}
		}
		
		// Better solution
		cp.objective.bounds(obj)   -= 1
		cp.objective.bestObjs(obj) -= 1
	}
	
	def probRelax(o : Int) {}
	
	def solFound {
		
		val objs = buildSol
		val sol  = buildSol
		
		// Always true in this framework
		pareto insert (objs, sol)
		
		// Reset the number of considered objectives
		nObjRestart = 0
		// Consider the next point in the set
		pareto.nextPoint()
	}
	
	def buildSol : Sol = Array(obj1.value, obj2.value)
	
	// Visu
	// -------------------------
	
	// Search
	// -------------------------
	
	cp.minimize(obj1, obj2) subjectTo {
		
		cp.post(obj2 + obj1 == 20)
		
	} exploration {
		
		cp.binaryFirstFail(obj1)
		
		solFound
	}
	
	println
	for (i <- 1 to pareto.size) {
		val p = pareto.nextPoint()
		println("sol n " + i + " : " + p(0) + " " + p(1))
	}
}