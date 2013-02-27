package oscar.examples.cp
import java.io._

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.cp.constraints.BinPacking
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPackingFlowExtended
import java.text.DecimalFormat

case class BinPackingInstance()
{
    var binCapacities = Array[Range]() 
    var itemsSizes = Array[Int]()
    var binForItems = Array[Array[Int]]() 
    
    def items = 0 until itemsSizes.length
    def bins = 0 until binCapacities.length
    
    def description() = 
    {
      var str = "BinPackingInstance with " + binCapacities.length + " bins and " + itemsSizes.length + " items"
      str += "\n bin capacities : " + binCapacities.map(r => r.start + " to " + r.end).mkString(",")
      str += "\n items sizes : " + itemsSizes.mkString(",")
      str += "\n bins for items : " + binForItems.map("\n\tArray(" + _.mkString(",") + ")").mkString(",")
      str += "\n"
      str
      
    }
}

class BinPackingInstanceGenerator
{
  

	var itemSizeMin					= 5
	var itemSizeMax					= Int.MaxValue
	var binCapacityMin				= 10
	var binCapacityMax				= Int.MaxValue //(not for the waste bin)

	
	
	var binCapacityMean				= 100
	var binCapacityDeviation		= 10
	
	var binCapacityDomainSizeMean  		= 20
	var binCapacityDomainSizeDeviation	= 2
	
	var itemsSizeMean				= 10
	var itemsSizeDeviation			= 4
	
	var numberOfBins				= 10
	var wasteBin 					= true
	
	//if we need 100 items to fill the bins there will be 150 availables 
	var itemAvailableToNeededRatio:Double 	= 1.5
	
	//for instance an item that need 10 items will have 15 available
	var itemAvailableToNeededRatioByBin:Double = 1.5
	
	def profileValues 	: Array[String] = Array(binCapacityMean, binCapacityDeviation, itemsSizeMean, itemsSizeDeviation, numberOfBins, wasteBin, itemAvailableToNeededRatio).map(_.toString)
	def profileKeys 	: Array[String] = Array("binCapacityMean", "binCapacityDeviation", "itemsSizeMean", "itemsSizeDeviation", "numberOfBins", "wasteBin", "itemAvailableToNeededRatio")	
	
	
	def generate () : Stream[BinPackingInstance] = 
	{
	  val r = new util.Random
	  val bpi = new BinPackingInstance()
	  
	  
	  
	  
	  bpi.binCapacities = Stream.continually{	val c = (r.nextGaussian * binCapacityDeviation + binCapacityMean);  
	  											val s = (r.nextGaussian * binCapacityDomainSizeDeviation + binCapacityDomainSizeMean)/ 2 .abs;
	  											(c - s).abs.toInt to (c + s).toInt}.filterNot(r => r.start < binCapacityMin || r.end > binCapacityMax).take(numberOfBins).toArray
	  
	  val numberOfItems = ((bpi.binCapacities.foldLeft(0)((s, r) => s + (r.start + r.end)/2 ) / itemsSizeMean) * itemAvailableToNeededRatio).toInt
	  
	  
	  
	  bpi.itemsSizes = Stream.continually{val c = ((r.nextGaussian * itemsSizeDeviation) + itemsSizeMean);  c.toInt}.filterNot(c => c < itemSizeMin || c > itemSizeMax).take(numberOfItems).toArray
	  
	  
	  val binItemProbabilities = Array.tabulate[Double](numberOfBins)
	  {
	    bin =>
	    val capacity = bpi.binCapacities(bin)	    
	    val numberOfItemsNeeded = (((capacity.start + capacity.end)/2.0) / itemsSizeMean ) * itemAvailableToNeededRatioByBin
		numberOfItemsNeeded / numberOfItems
	  }
	  
	  
	  bpi.binForItems = Array.tabulate[Array[Int]](numberOfItems)
	  {
  		item =>
		(bpi.binCapacities.indices).filter(a => r.nextDouble <= binItemProbabilities(a)).toArray
	  }
		
	  if(wasteBin) 
	  {
	    bpi.binCapacities = bpi.binCapacities :+ (0 to bpi.itemsSizes.foldLeft(0)(_ + _))
	    
	    //here the waste bin is the last one so the one with at index = to the precedent number of bins
	    bpi.binForItems = bpi.binForItems.map(_:+numberOfBins) //:+ bpi.itemsSizes.indices.toArray
	  }
	  bpi #:: generate()
	  
	}

}

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
	  val itemsCPVar = for(i <- bpi.items) yield CPVarInt(cp, bpi.binForItems(i))
	  
	  val bpf = if (extended)
	  				new BinPackingFlowExtended(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))))
	  			else 
	  			    new BinPackingFlow(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))))
	  
	  cp.add(bpf)
	  
	  (itemsCPVar,bpf.c)
	}
	
	//the classic binpacking return the items allocation
	def solveClassic() : Array[CPVarInt] =
	{
	  val cp = CPSolver()
	  val itemsCPVar = for(i <- bpi.items) yield CPVarInt(cp, bpi.binForItems(i))
	  
	  val bp = new BinPacking(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))))
	  			
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

object BinPackingFlowCardOpt extends App {

  

  val  test0 = {
	  val bpi = BinPackingInstance();
	  bpi.binCapacities = Array(45 to 45,20 to 20,15 to 15,10 to 10)
	  bpi.binForItems = Array(
		  Array(0,1),
		  Array(0,1,2,3),
		  Array(0,1,2,3),
		  Array(0,1,2,3),
		  Array(0,1,2,3),
		  Array(1,2,3),
		  Array(1,2,3),
		  Array(2,3),
		  Array(2,3),
		  Array(2,3),
		  Array(2,3),
		  Array(2,3))
	  bpi.itemsSizes = Array(15,10,10,10,10,5,5,5,5,5,5,5)
	  bpi
  }
  

  
  
  val  test1 = {
	  val bpi = BinPackingInstance();
	  bpi.binCapacities = Array(0 to 20,20 to 20,20 to 20)
	  bpi.binForItems = Array(
			  Array(0,1,2),
			  Array(0,1,2),
			  Array(0,1,2),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1))
	  bpi.itemsSizes = Array(10,10,10,5,5,5,5)
	  bpi
  }
  
  val  test2 = {
	  val bpi = BinPackingInstance();
	  bpi.binCapacities = Array(11 to 20,20 to 20,20 to 20)
	  bpi.binForItems = Array(
			  Array(0,1,2),
			  Array(0,1,2),
			  Array(0,1,2),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1))
	  bpi.itemsSizes = Array(10,10,10,5,5,5,5)
	  bpi
  }
  
    val  test3 = {
	  val bpi = BinPackingInstance();
	  bpi.binCapacities = Array(11 to 20,20 to 20,20 to 20)
	  bpi.binForItems = Array(
			  Array(1,2),
			  Array(1,2),
			  Array(1,2),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1),
			  Array(0,1))
	  bpi.itemsSizes = Array(10,10,10,5,5,5,5)
	  bpi
  }
    
    
   // print((new BinPackingTester(test4)).testNormalVsExtended())
  //BinPackingTester.testAndStats(List(test0,test1,test2,test3))
 
 
  
    val instancesGenerator = new BinPackingInstanceGenerator()
    instancesGenerator.itemAvailableToNeededRatio = 1.5
    instancesGenerator.numberOfBins = 10
    val instances = instancesGenerator.generate()
 
  
  
  BinPackingTester.testAndStats(instances, 1000)
}