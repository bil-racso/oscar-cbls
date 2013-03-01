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
import scala.actors.Actor


class ProfileTester(profile: (Int,Int,Int,Int,Int,Int,Int,Boolean,Double,Double), resultManager:Actor) extends Actor{
  def act(){
    val instancesGenerator = new BinPackingInstanceGenerator()
    instancesGenerator.setProperties(profile)
    val instances 	= instancesGenerator.generate()
    val results 	= (new BinPackingTester()).testAndStats(instances, 1000)
    resultManager 	! (instancesGenerator.profileValues.map(_.toString),results.map(_.toString))
  }
  
}

class ProfilesTester(profiles: List[(Int,Int,Int,Int,Int,Int,Int,Boolean,Double,Double)], bpResults: BinPackingResults) extends Actor {
  
  def initTesters(profiles : List[(Int,Int,Int,Int,Int,Int,Int,Boolean,Double,Double)], remaining:Int = 8, actors:List[Actor] = Nil) 
  		: (List[(Int,Int,Int,Int,Int,Int,Int,Boolean,Double,Double)], List[Actor]) = 
  {
    profiles match{
      case h :: t if (remaining > 0)=> {
        var actor = new ProfileTester(h, this); 
        println("start new profile remaining :" + t.size)
        initTesters(t, remaining - 1, actor :: actor :: actors)
        
      }
      case _ => (profiles,actors)
    }
    
  }
  
  def act(){
     
    var (remainingProfiles, actors) = initTesters(profiles)

    for(actor <- actors) actor.start
    
    while(true)
    {
      receive{
        case (profile : Array[String], results : Array[String]) =>  
          {
        	  actors = actors.filterNot(_ == sender)
        	  bpResults.add(profile, results)
        	  if(remainingProfiles != Nil)
        	  {
        		val actor = new ProfileTester(remainingProfiles.head, this)
        		remainingProfiles = remainingProfiles.tail
        		actors = actor :: actors
        		actor.start
        		println("start new profile remaining :" + remainingProfiles.size)
        	  } else if(actors == Nil) {
        	    exit()
        	  }
          } 
        
      }
    }
  }
  
}

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
    
  val  test4 = {
	  val bpi = new BinPackingInstance();
	  bpi.binCapacities = Array(102 to 102,100 to 100,101 to 101,105 to 105,106 to 106,97 to 97,99 to 99,88 to 88,104 to 104,101 to 101,0 to 1027)
	  bpi.binForItems = Array(
Array(10),
	Array(10),
	Array(5,8,10),
	Array(2,7,10),
	Array(4,5,9,10),
	Array(4,8,10),
	Array(1,10),
	Array(1,10),
	Array(10),
	Array(3,10),
	Array(2,5,10),
	Array(1,10),
	Array(0,2,10),
	Array(1,8,10),
	Array(6,10),
	Array(5,6,9,10),
	Array(5,7,10),
	Array(10),
	Array(4,9,10),
	Array(3,10),
	Array(0,3,10),
	Array(5,10),
	Array(7,10),
	Array(1,10),
	Array(9,10),
	Array(0,10),
	Array(8,10),
	Array(0,6,10),
	Array(7,9,10),
	Array(4,5,10),
	Array(2,10),
	Array(0,4,9,10),
	Array(10),
	Array(3,10),
	Array(7,10),
	Array(7,8,10),
	Array(7,10),
	Array(2,9,10),
	Array(4,8,10),
	Array(2,10),
	Array(0,9,10),
	Array(2,6,9,10),
	Array(6,10),
	Array(0,3,4,10),
	Array(0,1,3,8,10),
	Array(1,10),
	Array(4,10),
	Array(10),
	Array(10),
	Array(4,10),
	Array(0,10),
	Array(3,4,10),
	Array(8,10),
	Array(0,7,8,10),
	Array(0,10),
	Array(3,9,10),
	Array(7,9,10),
	Array(0,8,9,10),
	Array(0,4,10),
	Array(6,10),
	Array(0,1,10),
	Array(4,10),
	Array(3,9,10),
	Array(2,5,7,10),
	Array(10),
	Array(5,10),
	Array(0,6,10),
	Array(7,9,10),
	Array(9,10),
	Array(7,10),
	Array(0,6,10),
	Array(2,3,10),
	Array(3,10),
	Array(5,9,10),
	Array(0,4,6,9,10),
	Array(10),
	Array(10),
	Array(0,4,6,10),
	Array(0,10),
	Array(1,8,10),
	Array(2,4,8,10),
	Array(8,9,10),
	Array(10),
	Array(1,8,10),
	Array(4,10),
	Array(3,8,10),
	Array(6,10),
	Array(10),
	Array(10),
	Array(0,4,7,9,10),
	Array(1,10),
	Array(3,8,10),
	Array(2,6,8,10),
	Array(5,6,10),
	Array(10),
	Array(5,7,10),
	Array(0,10),
	Array(0,4,6,7,10),
	Array(3,8,10),
	Array(1,10)
			  )
	  bpi.itemsSizes = Array(13,6,12,11,11,12,16,13,11,13,14,14,12,8,8,7,8,11,10,5,11,10,7,12,14,7,15,8,16,10,5,6,9,13,10,7,10,10,11,13,8,11,15,9,13,15,12,7,10,11,9,7,10,15,15,7,10,16,8,10,10,15,15,7,10,11,9,6,8,13,12,8,11,11,9,8,8,8,17,10,16,6,8,8,7,5,12,8,6,8,9,6,10,13,14,11,9,10,8,10)
	  bpi
  }
    
    
    print((new BinPackingTest(test4)).testNormalExtendedAndClassic)
  //BinPackingTester.testAndStats(List(test0,test1,test2,test3))
 
 
  /*
    val binCapacityMean					= 100
	val binCapacityDeviation			= 10
	
	val binCapacityDomainSizeMean  		= 20
	val binCapacityDomainSizeDeviation	= 2
	
	val itemsSizeMean					= 10
	val itemsSizeDeviation				= 4
	
	val numberOfBins					= 10
	val wasteBin 						= true
	
	//if we need 100 items to fill the bins there will be 150 availables 
	val itemAvailableToNeededRatio:Double 	= 1.5
	
	//for instance an item that need 10 items will have 15 available
	val itemAvailableToNeededRatioByBin:Double = 1.5
	
	
	var profiles = for(binCapacityDomainSizeMean <- 0 to 20) 
	  yield (binCapacityMean, binCapacityDeviation,binCapacityDomainSizeMean,binCapacityDomainSizeDeviation, itemsSizeMean, itemsSizeDeviation, numberOfBins, wasteBin, itemAvailableToNeededRatio,itemAvailableToNeededRatioByBin)
	
	profiles = profiles ++ (for(itemAvailableToNeededRatioByBin <- 1.2 to(2,0.2) ) 
	  yield (binCapacityMean, binCapacityDeviation,binCapacityDomainSizeMean,binCapacityDomainSizeDeviation, itemsSizeMean, itemsSizeDeviation, numberOfBins, wasteBin, itemAvailableToNeededRatio,itemAvailableToNeededRatioByBin))
	
    val results = new BinPackingResults()
    
    val profilesTester = new ProfilesTester(profiles.toList, results) 
    profilesTester.start
    */
}