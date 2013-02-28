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
    
    
   // print((new BinPackingTester(test4)).testNormalVsExtended())
  //BinPackingTester.testAndStats(List(test0,test1,test2,test3))
 
 
  
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
}