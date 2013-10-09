package oscar.cp.dsl

import scala.io.Source
import scala.collection.mutable.ArrayBuilder
import scala.Array.canBuildFrom

class InstanceReader(filepath: String) {
  val file = Source.fromFile(filepath).getLines
  
  implicit def stringToIntArray(line: String) = new { 
    def toIntArray = line.trim.split(" +").map(_.stripSuffix(".00")).map(_.toInt) 
  }
  
  /**
   * Reads the next line of the file as an array of Int.
   */
  def readLine: Array[Int] = {
    file.next.toIntArray
  }
  
  /**
   * Reads the file by batches of batchLines lines and builds an array of nbColumns+1 arrays.
   * The first array contains the batch numbers from which the other datas have been read.
   * The following arrays contains the datas read from the nbColumns columns.
   * 
   * Example:
   * --------
   * Calling readDatas(5, 2) on the following file
   * 2 4 6
   * 7 1
   * 8 0 2
   * 9 3
   * 0 9 1
   * 4 2
   * 
   * Will try to read 5 columns in batches of 2 lines and will lead to the following array:
   * [[0 1 2]		// Batches index other datas are coming from. In this example we read 3 bathes of 2 lines.
   *  [2 8 0]		// Values found in the first column of the three batches of lines we read. 
   *  [4 0 9]		// Values found in the second column of the three batches of lines we read.
   *  [6 2 1]		// Values found in the third column of the three batches of lines we read.
   *  [7 9 4]		// Values found in the fourth column of the three batches of lines we read.
   *  [1 3 2]]	// Values found in the fifth column of the three batches of lines we read.
   */
  def readDatas(nbColumns: Int, batchLines: Int = 1): Array[Array[Int]] = {
    val datas = Array.fill(nbColumns+1){new ArrayBuilder.ofInt}
    var batchIndex = 0
    
    while (file.hasNext) {
      // Read lines by batches
      val lines = new StringBuilder()
	    for (line <- 0 until batchLines) {
	      lines append file.next
	    }
	    
	    // Classify values into their respectful arrays
	    for (values <- lines.toString.toIntArray.grouped(nbColumns)){
	      datas(0) += batchIndex
	    	for (dataIndex <- 0 until nbColumns){
	    		datas(dataIndex+1) += values(dataIndex)
	    	}
	    }
	    batchIndex += 1
	  }
    return datas.map(_.result)
  }

}