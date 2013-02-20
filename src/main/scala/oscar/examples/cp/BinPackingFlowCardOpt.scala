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
		      throw new Exception("Normal did better than extended")
		    case ((countNorm, countExt), (n, e)) => 
		      (countNorm + n.getSize,countExt + e.getSize)
		  }
		  (resultDiff._1,resultDiff._2,0,0) 
	  }
	   
	}
}

object BinPackingTester{
  	def testAndStats(instances : Seq[BinPackingInstance]) = {
	  var extBetter	= 0
	  var extBetterNoFail	= 0
	  var normFail 	= 0
	  var extFail 	= 0
	  var extImprovementMeanCount = 0.0
	  
	  for (inst <- instances) new BinPackingTester(inst).testNormalVsExtended match
	  {
	    case (_,_,1,1) => normFail +=1; extFail +=1
	    case (_,_,_,1) => extFail +=1; extBetter
	    case (n,e,_,_) if (n!=e) => extBetter += 1;extBetterNoFail += 1;;  extImprovementMeanCount += (e + 0.0)/n
	    
	  }
	  
	  val formatter = new DecimalFormat("#.###")
	  print("Extended is better in " + formatter.format(((extBetter + 0.0) / instances.size)*100) + "% cases,"
	      +" it found " + extFail + " fails against " + normFail +" for the normal version"
	      +" and the improvement mean is " + formatter.format(extImprovementMeanCount / extBetterNoFail) + " by cardinality\n");
	}
  
}

object BinPackingFlowCardOpt extends App {

  
  /*
  val bins = 0 until 4
  val items = 0 until 12
  val binCapacities = Array(45,20,15,10)
  val itemsSizes 	= Array(15,10,10,10,10,5,5,5,5,5,5,5)
  val binForItems 	= Array(
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
		  
  */
  
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
	  
  
  BinPackingTester.testAndStats(List(test1,test2,test3))
 
  
  
}