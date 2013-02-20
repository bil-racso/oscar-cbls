package oscar.examples.cp


import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPackingFlowExtended

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
	def testNormalVsExtended() = 
	{
	  try{
		  print("result normal \n" + solve(false).mkString("\n") + "\n")
	  } 
	  catch {
	    case e:NoSolutionException => printf("normal fail \n") 
	    
	  }
	  
	   try{
		  print("result extended  \n" + solve(true).mkString("\n") + "\n")
	  } 
	  catch {
	    case e:NoSolutionException => printf("extended fail \n") 
	    
	  }
	   
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
	  
  
  val bpt = new BinPackingTester(test3)
  
  bpt.testNormalVsExtended()
  
 
  
  
}