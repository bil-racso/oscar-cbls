package oscar.examples.cp.BinPacking

import java.io.FileWriter
import java.text.DecimalFormat
import oscar.cp.core.CPVarInt
import oscar.cp.core.NoSolutionException
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.BinPackingFlowExtended
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPacking

class BinPackingTester(bpi:BinPackingInstance)
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

object BinPackingTester{
  	def testAndStats(aInstances : Stream[BinPackingInstance],numberOfNonTrivial : Int = 10) = {
	  var notTrivial = 0
	  
  	  var extBetterThanNormal			= 0

	  
	  var extBetterThanClassic			= 0
	  var classicBetterThanExt			= 0
	  
	  var normFail 	= 0
	  var extFail 	= 0
	  var classFail = 0
	  
	  var totalNotTrivialNormalTime : Long = 0
	  var totalNotTrivialExtendedTime : Long = 0
	  var totalNotTrivialClassicTime : Long = 0
	  
	  var nonTrivialToGo = numberOfNonTrivial
	  val formatter = new DecimalFormat("#.###")
	  
	  
	  /*
	   * return false if the instance is trivial (fail for both)
	   */
	  def treatInstance(inst : BinPackingInstance) : Boolean =  {
		  val tester = new BinPackingTester(inst)
		  tester.testNormalExtendedAndClassic
		  
		  if (!(tester.normalFail && tester.classicFail && tester.extendedFail))
		  {
		    notTrivial += 1
		    totalNotTrivialClassicTime += tester.classicTime
		    totalNotTrivialNormalTime += tester.normalTime
		    totalNotTrivialExtendedTime += tester.extendedTime
		    
		    if(tester.normalFail) normFail += 1
		    if(tester.extendedFail) extFail += 1
		    if(tester.classicFail) classFail += 1
		    

		    if (tester.extendedFail && !tester.normalFail) extBetterThanNormal += 1
		    else if(!tester.extendedFail && !tester.normalFail 
		        && tester.extendedCardDomainsSize != tester.normalCardDomainsSize)
		      extBetterThanNormal += 1
		    
		    print(tester.extendedAllocDomainsSize + " " + tester.classicAllocDomainsSize)
		    if 		(tester.extendedFail && !tester.classicFail) extBetterThanClassic += 1
		    else if (!tester.extendedFail && tester.classicFail) classicBetterThanExt += 1
		    else if(!tester.extendedFail && !tester.classicFail 
		        && tester.extendedAllocDomainsSize < tester.classicAllocDomainsSize)
		      extBetterThanClassic += 1
		    else if(!tester.extendedFail && !tester.classicFail 
		        && tester.extendedAllocDomainsSize > tester.classicAllocDomainsSize)
		      classicBetterThanExt += 1  
		    
		    true
		  } 
		  else
		    false

	  }
	  
	  def testFirst(instances: Stream[BinPackingInstance])
	  {
	    //print("Instance to test : " + instances.head.description())
	    if(nonTrivialToGo > 0 ) 
	    {
	    	try
	    	{
		    	 if (treatInstance(instances.head)){
				  print(	"Extended is better in " + formatter.format(((extBetterThanNormal + 0.0) / notTrivial)*100) + "% cases than normal ("+extBetterThanNormal+"/"+notTrivial+"),\n"
						  + "Extended is better in " + formatter.format(((extBetterThanClassic + 0.0) / notTrivial)*100) + "% cases than classic ("+extBetterThanClassic+"/"+notTrivial+"),\n"
						  + "Classic is better in " + formatter.format(((classicBetterThanExt + 0.0) / notTrivial)*100) + "% cases than extended ("+classicBetterThanExt+"/"+notTrivial+"),\n"
						  + "extended found " + extFail + " fails normal " + normFail +" and classic " + classFail + "\n"
						  + "times for extended " + totalNotTrivialExtendedTime/notTrivial + ", normal " + totalNotTrivialNormalTime/notTrivial +" and classic " + totalNotTrivialClassicTime/notTrivial + "\n"
						  + "------------------------------------------------------------------------------------------\n"
						);
					  nonTrivialToGo -= 1
				 } 
	    	} catch {
	    		case e:Exception => 
	    		  print("Exception : " + e.getMessage())

	    		  val fw = new FileWriter("cdbinPackingFowCardOpt.err", true)
				  try {
					  fw.write("Exception : " + e.getMessage() + "\n" + instances.head.description + "\n-------------------------------\n\n\n\n")
				  }
	    		  finally fw.close() 
	    	}
			 
		    
		    testFirst(instances.tail)
	    }
	    
	    	
      
	  }
	  
	  testFirst(aInstances)
  	}
  
}
