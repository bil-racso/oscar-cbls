package oscar.examples.cp.BinPacking.Tester2

import oscar.examples.cp.BinPacking.DataStructures.BinPackingInstance
import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.BinPackingFlowExtended
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPacking
import oscar.cp.constraints.Sum

class InstanceSolver(instance : BinPackingInstance) {
	

	
	def solve(classic : Boolean = true, current : Boolean = true, extended : Boolean = true) =
	{
	  val cp = new CPSolver()
	  val x = (for(i <- instance.items) yield CPVarInt(cp, instance.binForItems(i))).toArray
	  val l = instance.bins.map(i=>CPVarInt(cp,instance.binCapacities(i))).toArray
	  val c = Array.tabulate(instance.bins.size+1)(j => CPVarInt(cp,0 to instance.items.size))
	  var nbSol = 0
	  val startTime = System.currentTimeMillis
	  cp.timeLimit = 5
	  
	  cp.solve subjectTo {
		postConstraints(cp,x,l,c,classic,current,extended)
	  } exploration {		  
		  cp.deterministicBinaryFirstFail(x)
		  nbSol += 1
	    } run(1)
		val duration =  System.currentTimeMillis - startTime
	 (nbSol, duration, cp.nFail)
	  
	 
	}
	
	def solveAll(classic : Boolean = true, current : Boolean = true, extended : Boolean = true) =
	{
	  val cp = new CPSolver()
	  val x = (for(i <- instance.items) yield CPVarInt(cp, instance.binForItems(i))).toArray
	  val l = instance.bins.map(i=>CPVarInt(cp,instance.binCapacities(i))).toArray
	  val c = Array.tabulate(instance.bins.size+1)(j => CPVarInt(cp,0 to instance.items.size))	  
	  var nbSol = 0
	  val startTime = System.currentTimeMillis
	  cp.timeLimit = 60		
	  
	  cp.solve subjectTo {
		  postConstraints(cp,x,l,c,classic,current,extended)

		} exploration {		  
		  cp.deterministicBinaryFirstFail(x)
		  nbSol += 1
	    } run()
		val duration =  System.currentTimeMillis - startTime
	 (nbSol, duration, cp.nFail)
	  
	 
	}
	
	def postConstraints(cp:CPSolver,x:Array[CPVarInt],l:Array[CPVarInt],c:Array[CPVarInt],
	    classic:Boolean, current:Boolean, extended:Boolean) {
	  
	  if(classic){
			  cp.add(new BinPacking(x,instance.itemsSizes,l))
			  val itemInBin = Array.tabulate(instance.items.size, instance.bins.size)((i,j) => CPVarInt(cp,Array(0,1)))
			 for (j <- instance.bins; i<- instance.items)
			   cp.add(itemInBin(i)(j) == x(i).isEq(j))
			 for(j <- instance.bins)
				 cp.add(new Sum((for(i<-instance.items) yield itemInBin(i)(j) * instance.itemsSizes(i)).toArray,l(j)))
		  }
		  if(current)
			  cp.add(new BinPackingFlow(x,instance.itemsSizes,l,c))
		  if(extended)			  
			  cp.add(new BinPackingFlowExtended(x,instance.itemsSizes,l,c))
	}
}
