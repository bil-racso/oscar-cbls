package oscar.examples.cp.BinPacking
import java.io._
import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.cp.constraints.BinPacking
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPackingFlowExtended
import java.text.DecimalFormat
import scala.Array.canBuildFrom
import scala.collection.immutable.Stream.consWrapper





object BinPackingFlowCardOpt extends App {

  

  val  test0 = {
	  val bpi = new BinPackingInstance();
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
	  val bpi = new BinPackingInstance();
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
	  val bpi =new BinPackingInstance();
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
	  val bpi = new BinPackingInstance();
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
 
  
  
  (new BinPackingTester()).testAndStats(instances, 1000)
}