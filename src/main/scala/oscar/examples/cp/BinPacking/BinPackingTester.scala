package oscar.examples.cp.BinPacking

import java.io.FileWriter
import java.text.DecimalFormat
import oscar.cp.core.CPVarInt
import oscar.cp.core.NoSolutionException
import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.BinPackingFlowExtended
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinPacking



class BinPackingTester{
  
	// return resultKeys, resultValues
  	def testAndStats(aInstances : Stream[BinPackingInstance],numberOfNonTrivial : Int = 10) : (Array[String], Array[String]) = 
  	{
	  var notTrivial = 0
	  
  	  var extBetterThanNormal			= 0.0

	  
	  var extBetterThanClassic			= 0.0
	  var classicBetterThanExt			= 0.0
	  
	  var normFail 	= 0.0
	  var extFail 	= 0.0
	  var classFail = 0.0
	  
	  var totalNotTrivialNormalTime : Long = 0
	  var totalNotTrivialExtendedTime : Long = 0
	  var totalNotTrivialClassicTime : Long = 0
	  
	  var nonTrivialToGo = numberOfNonTrivial
	  val formatter = new DecimalFormat("#.###")
	  
	  def resultValues 	: Array[String] = Array(extBetterThanNormal, extBetterThanClassic, classicBetterThanExt, normFail, extFail, classFail, totalNotTrivialNormalTime, totalNotTrivialExtendedTime, totalNotTrivialClassicTime).map(_ / notTrivial).map(_.toString)
	  def resultKeys 	: Array[String] = Array("extBetterThanNormal%", "extBetterThanClassic%", "classicBetterThanExt%", "normFail%", "extFail%", "classFail%", "totalNotTrivialNormalTime%", "totalNotTrivialExtendedTime%", "totalNotTrivialClassicTime%")	
	  
	  /*
	   * return false if the instance is trivial (fail for both)
	   */
	  def treatInstance(inst : BinPackingInstance) : Boolean =  {
		  val tester = new BinPackingTest(inst)
		  tester.testNormalExtendedAndClassic()
		  
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
	  
	  
	  
	  def testFirst(instances: Stream[BinPackingInstance]) : (Array[String], Array[String]) =
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
	    } else 
	      (resultKeys, resultValues)
	    
	    	
      
	  }
	  
	  testFirst(aInstances)
  	}
  
}
