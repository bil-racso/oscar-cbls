package oscar.examples.cp.BinPacking.Tester2

import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance
import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.BinPackingFlowExtended
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPacking

class InstanceSolver(instance : BinPackingInstance) {
	

	
	def solve(classic : Boolean = true, current : Boolean = true, extended : Boolean = true) =
	{
	  val cp = new CPSolver()
	  val itemsCPVar = (for(i <- instance.items) yield CPVarInt(cp, instance.binForItems(i))).toArray
	  var nbSol = 0
	  val startTime = System.currentTimeMillis
	  	
	  cp.solveAll subjectTo {
		  
		  
		  if(classic)
			  cp.add(new BinPacking(itemsCPVar,instance.itemsSizes,instance.bins.map(i=>CPVarInt(cp,instance.binCapacities(i))).toArray))	
		  if(current)
			  cp.add(new BinPackingFlow(itemsCPVar,instance.itemsSizes,instance.bins.map(i=>CPVarInt(cp,instance.binCapacities(i))).toArray))
		  if(extended)			  
			  cp.add(new BinPackingFlowExtended(itemsCPVar,instance.itemsSizes,instance.bins.map(i=>CPVarInt(cp,instance.binCapacities(i))).toArray))
		} exploration {
		  
		  cp.binaryFirstFail(itemsCPVar)
		  nbSol += 1
		  println("solution "+ nbSol)
		  
	    }
		val duration =  System.currentTimeMillis - startTime
	 (nbSol, duration, cp.nFail)
	  
	 
	}
}