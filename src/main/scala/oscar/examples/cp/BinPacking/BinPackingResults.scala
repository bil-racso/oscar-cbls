package oscar.examples.cp.BinPacking

import java.io.FileWriter

class BinPackingResults {

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
}