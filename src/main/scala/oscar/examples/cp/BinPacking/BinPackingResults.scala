package oscar.examples.cp.BinPacking

import java.io.FileWriter
import scala.collection.mutable.HashMap

object BinPackingResult
{
  val PROFILE_KEYS 	= List("binCapacityMean", "binCapacityDeviation","binCapacityDomainSizeMean","binCapacityDomainSizeDeviation", "itemsSizeMean", "itemsSizeDeviation", "numberOfBins", "wasteBin", "itemAvailableToNeededRatio","itemAvailableToNeededRatioByBin")
	val VALUES_KEYS 	= List("extBetterThanNormal%", "extBetterThanClassic%", "classicBetterThanExt%", "normFail%", "extFail%", "classFail%", "totalNotTrivialNormalTime%", "totalNotTrivialExtendedTime%", "totalNotTrivialClassicTime%")
}

class BinPackingResult
{
	var values 	: HashMap[String,Double] = new HashMap()
	var profile : HashMap[String,Double] = new HashMap()
	
	
	
	def initFromString(str:String)
	{
	  val str_values = str.split("\t")
	  
	  var i = 0
	  for(value <- BinPackingResult.PROFILE_KEYS) {profile(value) = str_values(i).toDouble ; i+=1}
	  for(value <- BinPackingResult.VALUES_KEYS) {values(value) = str_values(i).toDouble; i+=1} 
	}
	
	def profileForVariable(key:String) : String = 
	{
	  
	  profile.filterNot(e => e._1 == key).map(e => e._1 + ":" + e._2).mkString(", ")
	}
	
	def profileForKey(key:String) = profile(key)
	def valueForKey(key:String) = values(key)
		
}

class BinPackingResults {

  var results = List[BinPackingResult]() 
    
  val resultFilePath = "BinPackingFlowCardOptResults.txt"
  
  def add(profile:Array[String], results:Array[String]) 
  {
	  val fw = new FileWriter(resultFilePath, true)
	  try {
		  val result:String = profile.mkString("\t") + "\t" + results.mkString("\t") + "\n"
		  fw.write(result)
	  }
	  finally fw.close() 
  }
  
  def profilesForVariable(key:String) = 
  {
    val map = HashMap[String,Int]()
    for(result <- results)
    {
      val rProfile = result.profileForVariable(key)
      
      if (map.contains(rProfile))
        map(rProfile) += 1
      else 
        map(rProfile) = 1
    }
    map.filter(_._2 > 1)
  }
  
  def values(profileVariable:String,valueVariable:String,profile:String) = 
  {   
    println(profile)
    
    results.filter(_.profileForVariable(profileVariable) == profile).map{
      result =>
      (result.profileForKey(profileVariable),result.valueForKey(valueVariable))
    }
    
  }
  
  def readFile() {
    val file = scala.io.Source.fromFile(resultFilePath)
    
    results = file.getLines().map{
    	  line =>
    	   
	      val result = new BinPackingResult
	      result.initFromString(line)
	      result
    }.toList
  }
  
  
}