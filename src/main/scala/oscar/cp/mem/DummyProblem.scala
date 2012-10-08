package oscar.cp.mem

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.paretoFront._
import oscar.cp.mem.pareto.ParetoSet

object DummyProblem extends App {
	
	// Problem
	// -------------------------
	
	val pareto = new ParetoSet(2)
	
	val cp = CPSolver()
	
	val obj1 = CPVarInt(cp, -20 to 0)
	val obj2 = CPVarInt(cp, 0 to 20)
	
	// LNS
	// -------------------------
	
	cp.lns(100, 100) {
		
		
	}
	
	def solFound {
		
		val objs = buildSol
		val sol  = buildSol
		
		pareto insert (objs, sol)
		
		cp.objective.currentObjective = 0
		
		
	}
	
	def buildSol = Array[Int](obj1.value, obj2.value)
	
	// Search
	// -------------------------
	
	cp.minimize(obj1, obj2) subjectTo {
		
		cp.post(obj2 == obj1 + 20)
		
	} exploration {
		
		cp.binaryFirstFail(obj1)
		
		solFound
		
		println(obj1.value + " " + obj2.value)
	}
}