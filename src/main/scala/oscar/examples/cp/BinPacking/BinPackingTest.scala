package oscar.examples.cp.BinPacking

import java.text.DecimalFormat
import oscar.cp.core.CPVarInt
import oscar.cp.core.NoSolutionException
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.BinPackingFlowExtended
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPacking

class BinPackingTest (bpi:BinPackingInstance)
{
  
	var extendedFail = false
	var normalFail = false
	var classicFail = false
	
	var extendedCardDomainsSize = 0
	var normalCardDomainsSize = 0
	
	var extendedAllocDomainsSize = 0
	var normalAllocDomainsSize = 0
	var classicAllocDomainsSize = 0
	
	var normalTime : Long = 0
	var extendedTime : Long = 0
	var classicTime : Long = 0
	
	
	/*
	 * return first the items allocations then the cardinalities
	 */
	def solve(extended:Boolean = true) : (Array[CPVarInt],Array[CPVarInt]) =
	{
	  val cp = CPSolver()
	  val itemsCPVar = (for(i <- bpi.items) yield CPVarInt(cp, bpi.binForItems(i))).toArray
	  
	  val bpf = if (extended)
	  				new BinPackingFlowExtended(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))).toArray)
	  			else 
	  			    new BinPackingFlow(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))).toArray)
	  
	  cp.add(bpf)
	  
	  (itemsCPVar,bpf.c)
	}
	
	//the classic binpacking return the items allocation
	def solveClassic() : Array[CPVarInt] =
	{
	  val cp = CPSolver()
	  val itemsCPVar = (for(i <- bpi.items) yield CPVarInt(cp, bpi.binForItems(i))).toArray
	  
	  val bp = new BinPacking(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))).toArray)
	  			
	  cp.add(bp)
	  itemsCPVar
	  
	}
	
	/*
	 * @return
	 * countNorm, countExt, normFail, extFail
	 */
	def testNormalExtendedAndClassic() =
	{

	  var normalPropagateRestultCard = Array[CPVarInt]()
	  var extendedPropagateRestultCard = Array[CPVarInt]()
	  
	  var normalPropagateRestultAlloc = Array[CPVarInt]()
	  var extendedPropagateRestultAlloc = Array[CPVarInt]()
	  var classicPropagateRestultAlloc = Array[CPVarInt]()
	  
	 
	  
	  try{
		  val t = System.currentTimeMillis
		  val (a,b) =  solve(false)
		  normalPropagateRestultAlloc = a
		  normalPropagateRestultCard = b 
		  normalTime = System.currentTimeMillis - t
	  } catch {
	    case e:NoSolutionException => normalFail = true
	  }
	  
	   try{
	     val t = System.currentTimeMillis
		 val (a,b) =  solve(true)
		  extendedPropagateRestultAlloc = a
		  extendedPropagateRestultCard = b 
		 extendedTime = System.currentTimeMillis - t
		 
	  } catch {
	    case e:NoSolutionException => extendedFail = true 
	  }
	  
	  try{
	    val t = System.currentTimeMillis
		classicPropagateRestultAlloc = solveClassic
		classicTime = System.currentTimeMillis - t
	  } catch {
	    case e:NoSolutionException => classicFail = true 
	  }
	  
	  if (normalFail && !extendedFail)
	    throw new Exception("Normal find failure but not extended")
	  

	  if (normalPropagateRestultCard.zip(extendedPropagateRestultCard).exists{case (n,e) =>  n.getMin > e.getMin || n.getMax < e.getMax})
		throw new Exception("Normal did better than extended normal")
		  

	  	extendedCardDomainsSize = extendedPropagateRestultCard.foldLeft(0)(_ + _.getSize)
	  	normalCardDomainsSize 	= normalPropagateRestultCard.foldLeft(0)(_ + _.getSize)
	
		extendedAllocDomainsSize 	= extendedPropagateRestultAlloc.foldLeft(0)(_ + _.getSize)
		normalAllocDomainsSize 		= normalPropagateRestultAlloc.foldLeft(0)(_ + _.getSize)
		classicAllocDomainsSize 	= classicPropagateRestultAlloc.foldLeft(0)(_ + _.getSize)
	
	}

}