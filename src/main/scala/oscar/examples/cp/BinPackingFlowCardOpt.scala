package oscar.examples.cp


import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
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
      str += "\n bin capacities : " + binCapacities.mkString(",")
      str += "\n items sizes : " + itemsSizes.mkString(",")
      str += "\n bins for items : " + binForItems.foldLeft("")(_ + "\n\t" + _.mkString(","))
      str += "\n"
      str
      
    }
}

class BinPackingInstanceGenerator
{
  
	var itemsNeededByBin			= 10
	var itemsNeededByBinDeviation	= 4

	var itemsSizeMean				= 10
	var itemsSizeDeviation			= 4
	var itemSizeMin					= 5
	var itemSizeMax					= Int.MaxValue

	//the standard deviation on the bin capacity
	var binCapacityDeviation		= 10
	
	//the min capacity of a bin
	var binCapacityMin				= 10
	
	// the max capacity of a bin (not for the waste bin)
	var binCapacityMax				= Int.MaxValue
	
	  
	var numberOfBins				= 10
	var wasteBin 					= true
	
	var itemAvailableToNeededRatio 	= 5
	
	
	def generate () : Stream[BinPackingInstance] = 
	{
	  
	  val binCapacityMean = itemsNeededByBin * itemsSizeMean;
	  
	  val r = new util.Random
	  val bpi = new BinPackingInstance()
	  
	  
	  
	  
	  bpi.binCapacities = Stream.continually{val c = (r.nextGaussian * binCapacityDeviation + binCapacityMean);  c.toInt to c.toInt}.filterNot(r => r.start < binCapacityMin || r.end > binCapacityMax).take(numberOfBins).toArray
	  
	  val numberOfItems = bpi.binCapacities.foldLeft(0)((s, r) => s + (r.start + r.end)/2 ) / itemsSizeMean
	  
	  
	  
	  bpi.itemsSizes = Stream.continually{val c = ((r.nextGaussian * itemsSizeDeviation) + itemsSizeMean);  c.toInt}.filterNot(c => c < itemSizeMin || c > itemSizeMax).take(numberOfItems).toArray
	  
	  
	  val binItemProbabilities = Array.tabulate[Double](numberOfBins)
	  {
	    bin =>
	    val capacity = bpi.binCapacities(bin)	    
	    val numberOfItemsNeeded = (((capacity.start + capacity.end)/2.0) / itemsSizeMean ) * itemAvailableToNeededRatio
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
	    bpi.binForItems = bpi.binForItems :+ bpi.itemsSizes.indices.toArray
	  }
	  bpi #:: generate()
	  
	}

}

class BinPackingTester(bpi:BinPackingInstance)
{
	def solve(extended:Boolean = true) : Array[CPVarInt] =
	{
	  val cp = CPSolver()
	  val itemsCPVar = for(i <- bpi.items) yield CPVarInt(cp, bpi.binForItems(i))
	  
	  val bpf = if (extended)
	  				new BinPackingFlowExtended(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))))
	  			else 
	  			    new BinPackingFlow(itemsCPVar,bpi.itemsSizes,bpi.bins.map(i=>CPVarInt(cp,bpi.binCapacities(i))))
	  
	  cp.add(bpf)
	  
	  bpf.c
	}
	
	/*
	 * @return
	 * countNorm, countExt, normFail, extFail
	 */
	def testNormalVsExtended(): (Int,Int, Int, Int) =
	{
	  var normalFail = false
	  var extendedFail = false
	  var normalPropagateRestult = Array[CPVarInt]()
	  var extendedPropagateRestult = Array[CPVarInt]()
	  
	  try{
		  normalPropagateRestult = solve(false)
	  } 
	  catch {
	    case e:NoSolutionException => normalFail = true
	    
	  }
	  
	   try{
		  extendedPropagateRestult = solve(true)
	  } 
	  catch {
	    case e:NoSolutionException => extendedFail = true 
	    
	  }
	  
	  if (normalFail && !extendedFail)
	    throw new Exception("Normal find failure but not extended")
	  
	  if(normalFail) (0,0,1,1) 
 	  else if (extendedFail) (0,0,0,1)
	  else{
		  val resultDiff = normalPropagateRestult.zip(extendedPropagateRestult).foldLeft((0,0)){
		    case ((countNorm, countExt), (n, e)) if n.getMin > e.getMin || n.getMax < e.getMax=> 
		      throw new Exception("Normal did better than extended normal ("+n.getMin+","+n.getMax+") extended ("+e.getMin+","+e.getMax+")" )
		    case ((countNorm, countExt), (n, e)) => 
		      (countNorm + n.getSize,countExt + e.getSize)
		  }
		  (resultDiff._1,resultDiff._2,0,0) 
	  }
	   
	}
}

object BinPackingTester{
  	def testAndStats(aInstances : Stream[BinPackingInstance],numberOfNonTrivial : Int = 10) = {
	  var extBetter	= 0
	  var notTrivial = 0
	  var extBetterNoFail	= 0
	  var normFail 	= 0
	  var extFail 	= 0
	  var extImprovementMeanCount = 0.0
	  var nonTrivialToGo = numberOfNonTrivial
	  val formatter = new DecimalFormat("#.###")
	  
	  
	  /*
	   * return false if the instance is trivial (fail for both)
	   */
	  def treatInstance(inst : BinPackingInstance) : Boolean =  {
		  new BinPackingTester(inst).testNormalVsExtended match
		  {
		    case (_,_,1,1) 				=> false
		    case (_,_,_,1) 				=> extFail +=1; extBetter += 1; notTrivial +=1; true
		    case (n,e,_,_) if (n!=e) 	=> extBetter += 1;extBetterNoFail += 1;notTrivial +=1;  extImprovementMeanCount += (e + 0.0)/n; true 
		    case (n,e,_,_) 				=> notTrivial +=1; true
		  }
	  }
	  
	  def testFirst(instances: Stream[BinPackingInstance])
	  {
	    print("Instance to test : " + instances.head.description())
	    if(nonTrivialToGo > 0 ) 
	    {
	      
	    	 if (treatInstance(instances.head)){
			  print("Extended is better in " + formatter.format(((extBetter + 0.0) / notTrivial)*100) + "% cases ("+extBetter+"/"+notTrivial+"),"
				+" it found " + extFail + " fails against " + normFail +" for the normal version"
				+" and the improvement mean is " + formatter.format(extImprovementMeanCount / extBetterNoFail) + " by cardinality\n");
				  nonTrivialToGo -= 1
			 }
			 else 
			    print("\n\ntrivial\n\n")
 			 }
	    
	    	testFirst(instances.tail)
      
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
	  
  
  //BinPackingTester.testAndStats(List(test0,test1,test2,test3))
 
    val instancesGenerator = new BinPackingInstanceGenerator()
    val instances = instancesGenerator.generate()
 //for(instance <- instances)
 //  print("---------------------\n" + instance.description + "---------------------\n")
  
  BinPackingTester.testAndStats(instances, 20)
}